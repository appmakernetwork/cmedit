-- | A file-tree browser model used by the Open dialog. This module is pure:
-- it holds the (lazily loaded) directory tree and the cursor/scroll state, and
-- provides the navigation operations. Directory listings are fetched by the IO
-- driver and fed back via 'fillChildren' / 'mkBrowser'.
module Cmedit.Browser
  ( FileNode(..)
  , Browser(..)
  , Entry
  , mkBrowser
  , mkBrowserNoParent
  , fillChildren
  , mergeChildren
  , expandedDirPaths
  , visibleRows
  , rowCount
  , selectedNode
  , moveSel
  , setSel
  , collapseSelected
  , expandSelected
  , parentDir
  , scrollInto
  , typeAhead
  , toggleHidden
  , nodeNeedsLoad
  , nodeAt
  , expandAt
  , selectPath
  ) where

import Data.Char (toLower)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, takeFileName)

-- | A directory entry as reported by the IO driver: its full path, whether it
-- is a directory, and (for files) its size in bytes if it could be stat'd.
type Entry = (FilePath, Bool, Maybe Integer)

data FileNode = FileNode
  { fnName     :: !Text
  , fnPath     :: !FilePath
  , fnIsDir    :: !Bool
  , fnParent   :: !Bool                 -- ^ The synthetic ".." entry.
  , fnExpanded :: !Bool
  , fnChildren :: !(Maybe [FileNode])   -- ^ Nothing until the directory is loaded.
  , fnSize     :: !(Maybe Integer)      -- ^ File size in bytes (Nothing for dirs / unstat-able).
  } deriving (Show)

data Browser = Browser
  { brRoot       :: !FileNode   -- ^ The root directory; its children are the listing.
  , brSelected   :: !Int        -- ^ Index into 'visibleRows'.
  , brTop        :: !Int        -- ^ First visible row (scroll offset).
  , brShowHidden :: !Bool
  } deriving (Show)

-- | Build a browser rooted at @path@ with the given top-level entries. A ".."
-- entry is prepended unless we are already at the filesystem root.
mkBrowser :: FilePath -> [Entry] -> Browser
mkBrowser = mkBrowserWith True

-- | Like 'mkBrowser' but with no synthetic ".." entry. Used by the workspace
-- file-explorer panel, whose root is fixed (you cannot navigate above it).
mkBrowserNoParent :: FilePath -> [Entry] -> Browser
mkBrowserNoParent = mkBrowserWith False

mkBrowserWith :: Bool -> FilePath -> [Entry] -> Browser
mkBrowserWith withParent path entries =
  let parents = if withParent then parentEntries path else []
      kids = parents ++ mkNodes entries
      root = FileNode (T.pack path) path True False True (Just kids) Nothing
  in Browser root 0 0 False

parentEntries :: FilePath -> [FileNode]
parentEntries path
  | takeDirectory path /= path =
      [FileNode (T.pack "..") (takeDirectory path) True True False Nothing Nothing]
  | otherwise = []

mkNodes :: [Entry] -> [FileNode]
mkNodes = sortNodes . map node
  where node (p, isDir, sz) = FileNode (T.pack (takeFileName p)) p isDir False False Nothing sz

-- Directories first, then files; each alphabetical, case-insensitive.
-- Decorate-sort-undecorate: the case-folded key is computed once per node,
-- not once per comparison — re-listing a 10k-entry directory (the freshness
-- poll does this whenever its mtime moves) was spending most of its time in
-- repeated T.toLower.
sortNodes :: [FileNode] -> [FileNode]
sortNodes ns = map snd (sortBy (comparing fst) [ (key n, n) | n <- ns ])
  where key n = (not (fnIsDir n), T.toLower (fnName n))

-- | Insert the children of a just-listed directory at @path@.
fillChildren :: FilePath -> [Entry] -> Browser -> Browser
fillChildren path entries br =
  br { brRoot = modifyNode path (\n -> n { fnChildren = Just (mkNodes entries)
                                         , fnExpanded = True }) (brRoot br) }

-- | Install a fresh listing for the directory at @path@, keeping the loaded
-- subtree and expansion state of entries that persist — so a background
-- refresh (or a re-list on expand) never collapses what the user has opened.
-- Synthetic ".." entries are preserved as-is.
mergeChildren :: FilePath -> [Entry] -> Browser -> Browser
mergeChildren path entries br = br { brRoot = modifyNode path merge (brRoot br) }
  where
    merge n =
      let old      = fromMaybe [] (fnChildren n)
          parents  = filter fnParent old
          oldByPath = M.fromList [ (fnPath o, o) | o <- old, not (fnParent o) ]
          keep fresh = case M.lookup (fnPath fresh) oldByPath of
            Just o | fnIsDir o && fnIsDir fresh ->
              fresh { fnExpanded = fnExpanded o, fnChildren = fnChildren o }
            _ -> fresh
      in n { fnExpanded = True, fnChildren = Just (parents ++ map keep (mkNodes entries)) }

-- | Every expanded, loaded directory in the tree (the root included, synthetic
-- ".." entries excluded) — the set a background refresh needs to watch.
expandedDirPaths :: Browser -> [FilePath]
expandedDirPaths br = go (brRoot br)
  where
    go n | fnIsDir n && not (fnParent n) && fnExpanded n =
             fnPath n : concatMap go (fromMaybe [] (fnChildren n))
         | otherwise = []

modifyNode :: FilePath -> (FileNode -> FileNode) -> FileNode -> FileNode
modifyNode target f node
  | fnPath node == target = f node
  | otherwise = node { fnChildren = fmap (map (modifyNode target f)) (fnChildren node) }

-- | The flattened, currently-visible rows as @(depth, node)@ pairs, filtered
-- by the hidden-files setting.
visibleRows :: Browser -> [(Int, FileNode)]
visibleRows br = go 0 (fromMaybe [] (fnChildren (brRoot br)))
  where
    go d ns = concatMap (one d) (filter keep ns)
    keep n = brShowHidden br || fnParent n || not (isHidden (fnName n))
    one d n = (d, n) : if fnIsDir n && fnExpanded n
                         then go (d + 1) (fromMaybe [] (fnChildren n))
                         else []
    isHidden nm = not (T.null nm) && T.head nm == '.'

rowCount :: Browser -> Int
rowCount = length . visibleRows

selectedNode :: Browser -> Maybe FileNode
selectedNode br =
  let rows = visibleRows br
  in if null rows then Nothing
     else Just (snd (rows !! clamp 0 (length rows - 1) (brSelected br)))

clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

moveSel :: Int -> Browser -> Browser
moveSel delta br =
  let n = rowCount br
  in if n == 0 then br else br { brSelected = clamp 0 (n - 1) (brSelected br + delta) }

setSel :: Int -> Browser -> Browser
setSel i br =
  let n = rowCount br
  in if n == 0 then br else br { brSelected = clamp 0 (n - 1) i }

-- | True when the selected directory's children still need to be fetched.
nodeNeedsLoad :: FileNode -> Bool
nodeNeedsLoad n = fnIsDir n && not (fnParent n) && isNothing (fnChildren n)

-- Collapse the selected directory (no-op for files).
collapseSelected :: Browser -> Browser
collapseSelected br = case selectedNode br of
  Just n | fnIsDir n && fnExpanded n ->
    br { brRoot = modifyNode (fnPath n) (\m -> m { fnExpanded = False }) (brRoot br) }
  _ -> br

-- Expand the selected directory if its children are already loaded.
expandSelected :: Browser -> Browser
expandSelected br = case selectedNode br of
  Just n | fnIsDir n ->
    br { brRoot = modifyNode (fnPath n) (\m -> m { fnExpanded = True }) (brRoot br) }
  _ -> br

-- | The parent directory of the browser root (for the ".." re-root).
parentDir :: Browser -> FilePath
parentDir br = takeDirectory (fnPath (brRoot br))

-- | Adjust the scroll offset so the selection is within a window of @height@.
scrollInto :: Int -> Browser -> Browser
scrollInto height br =
  let sel = brSelected br
      top0 = brTop br
      top1 | sel < top0              = sel
           | sel >= top0 + height    = sel - height + 1
           | otherwise               = top0
  in br { brTop = max 0 top1 }

-- | Jump to the next visible entry whose name starts with @c@ (wrapping).
-- One pass over the rotated row list — indexing per candidate would go
-- quadratic on huge directory listings.
typeAhead :: Char -> Browser -> Browser
typeAhead c br =
  let rows = zip [0 ..] (visibleRows br)
      lc = toLower c
      starts (_, (_, n)) = not (T.null (fnName n))
                           && toLower (T.head (fnName n)) == lc
      (before, after) = splitAt (brSelected br + 1) rows
  in case filter starts (after ++ before) of
       ((i, _) : _) -> br { brSelected = i }
       []           -> br

toggleHidden :: Browser -> Browser
toggleHidden br = setSel (brSelected br) br { brShowHidden = not (brShowHidden br) }

-- | Find the tree node at an exact path, if it has been loaded into the tree.
nodeAt :: FilePath -> Browser -> Maybe FileNode
nodeAt path br = go (brRoot br)
  where
    go n | fnPath n == path = Just n
         | otherwise = firstJust (map go (fromMaybe [] (fnChildren n)))
    firstJust = foldr (\x acc -> maybe acc Just x) Nothing

-- | Mark the (already-loaded) directory at @path@ expanded. No-op if absent.
expandAt :: FilePath -> Browser -> Browser
expandAt path br = br { brRoot = modifyNode path (\n -> n { fnExpanded = True }) (brRoot br) }

-- | Select the visible row for @path@ (if it is currently visible). No-op else.
selectPath :: FilePath -> Browser -> Browser
selectPath path br =
  case [ i | (i, (_, n)) <- zip [0 ..] (visibleRows br), fnPath n == path ] of
    (i : _) -> setSel i br
    []      -> br
