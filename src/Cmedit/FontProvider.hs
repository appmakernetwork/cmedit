{-# LANGUAGE BangPatterns #-}
-- | System fonts for the vector front-ends (plan 0034, workstream F): find
-- the installed font files once, and turn a request ("Helvetica, bold", a
-- CSS @font-family@ list) into a parsed "Cmedit.Font" on demand.
--
-- This is driver-side code (like "Cmedit.Clipboard"): the directory walk and
-- the file reads are IO, and they run on background threads only — the
-- discovery on a thread forked at startup, the loads inside the SVG parse
-- job (and, later, the PDF page renderer's job). Nothing here is ever called
-- from the event-loop thread. Every decision that can be pure is, and is
-- exported for the tests: which files a request ranks ('FontSys.substituteFor',
-- unchanged), how an SVG query maps onto a request ('queryRequest'), the
-- 'Face' adapter ('fontFace'), and the cache's admission and eviction
-- ('cacheInsert').
--
-- __The shape the PDF renderer reuses__ is 'fontFor': a 'FontRequest' (a
-- family as written plus PDF's descriptor flags) in, @(key, Font)@ out, the
-- key unique per loaded face for glyph-cache keys. The SVG path is a thin
-- layer over it ('facesForQueries').
--
-- Bounds: the walk visits at most 'maxFontEntries' directory entries at most
-- 'maxFontDepth' deep, follows symlinked directories once each (by canonical
-- path); a font file
-- over 'maxFontFileBytes' is skipped; the cache holds at most
-- 'maxCachedFonts' faces and 'maxCachedFontBytes' of font program
-- (least-recently-used out first); a file that failed to parse is remembered
-- (up to 256) so it is not re-read for every request, but never because of
-- an asynchronous exception: a job killed mid-load leaves the cache exactly
-- as it was ('trySync'). Discovery that has not finished within the caller's
-- timeout reads as 'Nothing' from 'providerFiles'; the caller draws without
-- fonts for now and redraws when 'startFontDiscovery''s callback fires.
module Cmedit.FontProvider
  ( -- * Pure parts
    queryRequest
  , fontFace
  , hostOS
  , FontCache(..)
  , CacheEntry(..)
  , emptyFontCache
  , cacheLookup
  , cacheInsert
  , cacheFailed
  , maxCachedFonts
  , maxCachedFontBytes
  , maxFontFileBytes
  , maxFontEntries
  , maxFontDepth
  , fontCandidatesPer
    -- * The provider (IO, background threads only)
  , FontProvider
  , newFontProvider
  , startFontDiscovery
  , providerFiles
  , fontFor
  , facesForQueries
  , discoverFontFiles
  , trySync
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, SomeAsyncException, evaluate, fromException, throwIO, try)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesDirectoryExist, getFileSize, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import qualified System.Info
import System.Timeout (timeout)

import Cmedit.Font
import Cmedit.FontSys
import Cmedit.Geom (Affine(..))
import Cmedit.Svg (Face(..), FontQuery(..), FontStyle(..))

------------------------------------------------------------------------------
-- Pure parts

-- | The operating system, for 'fontDirectories'.
hostOS :: OS
hostOS = case System.Info.os of
  "mingw32" -> OSWindows
  "darwin"  -> OSMac
  _         -> OSLinux

-- | An SVG font query as a substitution request: its first family (the
-- layout asks for the others one at a time itself, see "Cmedit.Svg"'s
-- per-character fallback), bold from weight 600 up, italic for italic and
-- oblique, and the generic families' flags set so the ranking honours them.
queryRequest :: FontQuery -> FontRequest
queryRequest q =
  let fam = case fqFamilies q of (f : _) -> T.strip f; [] -> "serif"
      low = T.toLower fam
  in (fontRequest fam)
       { frBold = fqWeight q >= 600
       , frItalic = fqStyle q /= FontNormal
       , frMono = low == "monospace"
       , frSerif = low == "serif" }

-- | Adapt a parsed font to the SVG layout's 'Face'. Glyph space is the
-- font's own (units of the em size implied by 'fontMatrix'); the ascent and
-- descent, which "Cmedit.Font" reports in ems, are scaled into those units.
fontFace :: Int -> Font -> Face
fontFace key f =
  let Affine a _ _ _ _ _ = fontMatrix f
      upem = if a > 0 && not (isInfinite (1 / a)) then 1 / a else 1000
  in Face { faceKey = key
          , faceUnitsPerEm = upem
          , faceAscent = fontAscent f * upem
          , faceDescent = fontDescent f * upem
          , faceGlyph = \c -> do
              g <- glyphForChar f c
              pure (g, glyphPath f g, glyphAdvance f g)
          , faceKern = glyphKerning f }

-- | One cached face.
data CacheEntry = CacheEntry
  { ceFont  :: Font
  , ceKey   :: !Int    -- ^ Unique per loaded face, for the scene's glyph cache keys.
  , ceBytes :: !Int    -- ^ Size of the font program it holds on to.
  , ceUsed  :: !Int    -- ^ Use stamp (LRU).
  }

-- | Loaded faces by file, plus the files known not to parse.
data FontCache = FontCache
  { fcEntries :: !(M.Map FilePath CacheEntry)
  , fcFailed  :: !(S.Set FilePath)
  , fcTick    :: !Int
  , fcNextKey :: !Int
  }

emptyFontCache :: FontCache
emptyFontCache = FontCache M.empty S.empty 0 1

maxCachedFonts :: Int
maxCachedFonts = 12

maxCachedFontBytes :: Int
maxCachedFontBytes = 96 * 1024 * 1024

maxFontFileBytes :: Integer
maxFontFileBytes = 48 * 1024 * 1024

-- | How many ranked candidates one request tries before giving up (a
-- candidate may fail to parse, or be over the size cap).
fontCandidatesPer :: Int
fontCandidatesPer = 4

-- | Look a file up, stamping it as used.
cacheLookup :: FilePath -> FontCache -> (Maybe (Int, Font), FontCache)
cacheLookup p c = case M.lookup p (fcEntries c) of
  Nothing -> (Nothing, c)
  Just e  -> let t = fcTick c + 1
             in ( Just (ceKey e, ceFont e)
                , c { fcEntries = M.insert p e { ceUsed = t } (fcEntries c), fcTick = t } )

-- | Admit a freshly parsed face, evicting least-recently-used faces until
-- both bounds hold again (the new face itself is never the one evicted, so
-- a single face larger than the byte cap is still usable once).
cacheInsert :: FilePath -> Int -> Font -> FontCache -> ((Int, Font), FontCache)
cacheInsert p bytes f c0 =
  let t = fcTick c0 + 1
      k = fcNextKey c0
      e = CacheEntry f k bytes t
      c1 = c0 { fcEntries = M.insert p e (fcEntries c0), fcTick = t, fcNextKey = k + 1 }
  in ((k, f), evict c1)
  where
    total c = sum (map ceBytes (M.elems (fcEntries c)))
    evict c
      | M.size (fcEntries c) <= maxCachedFonts && total c <= maxCachedFontBytes = c
      | otherwise =
          case sortOn (ceUsed . snd) [ kv | kv@(q, _) <- M.toList (fcEntries c), q /= p ] of
            ((q, _) : _) -> evict c { fcEntries = M.delete q (fcEntries c) }
            [] -> c

-- | Remember a file that did not parse (bounded).
cacheFailed :: FilePath -> FontCache -> FontCache
cacheFailed p c
  | S.size (fcFailed c) >= 256 = c
  | otherwise = c { fcFailed = S.insert p (fcFailed c) }

------------------------------------------------------------------------------
-- The provider

-- | Discovered files (empty until discovery finishes) and the face cache.
data FontProvider = FontProvider
  { fpFiles :: !(MVar [FilePath])
  , fpCache :: !(MVar FontCache)
  }

newFontProvider :: IO FontProvider
newFontProvider = FontProvider <$> newEmptyMVar <*> newMVar emptyFontCache

-- | Walk the system font directories on a background thread. The list is
-- published exactly once, even if the walk throws (as an empty list then).
startFontDiscovery :: FontProvider -> IO () -> IO ()
startFontDiscovery fp onDone = do
  _ <- forkIO $ do
    r <- trySync (do home <- either (const "") id <$> trySync getHomeDirectory
                     env <- forM fontEnvVars (\k -> fmap ((,) k) <$> lookupEnv k)
                     discoverFontFiles (fontDirectories hostOS [ kv | Just kv <- env ] home))
    _ <- tryPutMVar (fpFiles fp) (either (const []) id r)
    -- Tell the driver: a parse that gave up waiting for the walk drew its
    -- text without fonts, and can now be redone with them.
    _ <- trySync onDone
    pure ()
  pure ()

-- | The discovered files, waiting at most this many microseconds for the
-- walk to finish; 'Nothing' when it has not (as opposed to @Just []@: the walk
-- finished and found no fonts at all). The distinction is what lets a
-- picture drawn without its fonts be redrawn once they turn up.
providerFiles :: Int -> FontProvider -> IO (Maybe [FilePath])
providerFiles us fp = timeout us (readMVar (fpFiles fp))

-- | 'try' that lets asynchronous exceptions through: a job killed because a
-- newer one superseded it must die, and must not mistake its own death for a
-- broken font file (and remember that file as one).
trySync :: IO a -> IO (Either SomeException a)
trySync act = do
  r <- try act
  case r of
    Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    _ -> pure r

maxFontEntries :: Int
maxFontEntries = 50000

maxFontDepth :: Int
maxFontDepth = 10

-- | Every font file under these directories (bounded; see the module
-- header). A directory that is missing or unreadable is simply empty.
-- Symlinked directories are followed (distributions link whole font trees
-- into @/usr/share/fonts@), with a visited set of canonical paths so a link
-- cycle is walked once; and the entry budget is charged for a listing
-- /before/ any of its entries is stat-ed, so one enormous directory cannot
-- cost more than the budget allows.
discoverFontFiles :: [FilePath] -> IO [FilePath]
discoverFontFiles roots = do
  budget <- newIORef maxFontEntries
  visited <- newIORef S.empty
  acc <- newIORef []
  mapM_ (walk budget visited acc 0) roots
  reverse <$> readIORef acc
  where
    fontExt p = map toLower (takeExtension p) `elem` [".ttf", ".otf", ".ttc", ".otc", ".pfb", ".pfa", ".t1"]
    walk budget visited acc depth dir
      | depth > maxFontDepth = pure ()
      | otherwise = do
          left <- readIORef budget
          canon <- either (const dir) id <$> trySync (canonicalizePath dir)
          seen <- S.member canon <$> readIORef visited
          if left <= 0 || seen then pure () else do
            modifyIORef' visited (S.insert canon)
            r <- trySync (listDirectory dir)
            case r of
              Left _ -> pure ()
              Right names0 -> do
                let names = take left names0
                modifyIORef' budget (subtract (length names))
                mapM_ (\n -> do
                         let p = dir </> n
                         if fontExt p
                           then modifyIORef' acc (p :)
                           else do
                             isDir <- safeBool (doesDirectoryExist p)
                             if isDir then walk budget visited acc (depth + 1) p else pure ())
                      names
    safeBool act = either (const False) id <$> trySync act

-- | A parsed font for a request: the ranked substitutes are tried in order
-- (at most 'fontCandidatesPer'), loaded through the cache. 'Nothing' when no
-- font is installed at all, or none of the candidates loads. Background
-- threads only: it may read a font file.
fontFor :: FontProvider -> [FilePath] -> FontRequest -> IO (Maybe (Int, Font))
fontFor fp files req = go (take fontCandidatesPer (substituteFor files req))
  where
    go [] = pure Nothing
    go (p : ps) = do
      r <- modifyMVar (fpCache fp) (\c -> load p c)
      case r of
        Just x  -> pure (Just x)
        Nothing -> go ps
    load p c
      | S.member p (fcFailed c) = pure (c, Nothing)
      | otherwise = case cacheLookup p c of
          (Just x, c') -> pure (c', Just x)
          (Nothing, _) -> do
            r <- trySync (readFontFile p)
            case r of
              Right (Just (n, f)) -> let (x, c') = cacheInsert p n f c in pure (c', Just x)
              _ -> pure (cacheFailed p c, Nothing)

-- Read and parse one font file (size-capped). Synchronous failures are the
-- caller's to record; asynchronous ones propagate ('trySync').
readFontFile :: FilePath -> IO (Maybe (Int, Font))
readFontFile p = do
  sz <- getFileSize p
  if sz > maxFontFileBytes then pure Nothing else do
    bs <- BS.readFile p
    case parseFont bs of
      Left _ -> pure Nothing
      Right f -> do
        _ <- evaluate (fontNumGlyphs f)
        pure (Just (BS.length bs, f))

-- | Resolve every query up front and hand back a pure lookup for
-- 'Cmedit.Svg.seFonts'. Queries that resolve to the same file share one
-- 'Face' (same key), which is what lets the layout's fallback chain
-- de-duplicate them. A query the pre-pass did not list answers 'Nothing'.
facesForQueries :: FontProvider -> [FilePath] -> [FontQuery] -> IO (FontQuery -> Maybe Face)
facesForQueries fp files qs = do
  pairs <- forM qs $ \q -> do
    r <- fontFor fp files (queryRequest q)
    pure (qkey q, fmap (\(k, f) -> fontFace k f) r)
  let m = M.fromList [ (k, f) | (k, Just f) <- pairs ]
  pure (\q -> M.lookup (qkey q) m)
  where qkey q = (fqFamilies q, fqWeight q, styleIx (fqStyle q))
        styleIx s = case s of FontNormal -> 0 :: Int; FontItalic -> 1; FontOblique -> 2
