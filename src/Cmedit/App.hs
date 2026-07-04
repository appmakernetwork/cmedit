-- | The IO driver: terminal setup/teardown, a threaded input reader, a
-- SIGWINCH-driven resize, the main event loop, and turning 'Effect's into
-- real-world actions (clipboard, files). The pure model lives in
-- "Cmedit.Editor"; this module is the thin shell around it.
module Cmedit.App
  ( run
  ) where

import Control.Concurrent (forkIO, getNumCapabilities, myThreadId)
import Control.Concurrent.STM
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Control.Exception (SomeException, bracket, bracket_, finally, try)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, char7, hPutBuilder)
import Data.IORef
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getCurrentDirectory, getFileSize, getModificationTime, listDirectory
  , removeDirectoryRecursive, removeFile, renamePath )
import System.FilePath ((</>), makeRelative, splitDirectories, takeDirectory, takeFileName)
import System.IO

import Cmedit.About (aboutTickUs)
import Cmedit.Ansi
import Cmedit.Browser (Browser(..), FileNode(..))
import qualified Cmedit.Browser as Br
import Cmedit.Caps
import Cmedit.Clipboard
import Cmedit.ConfigFile
  ( RecentEntry(..), ThemeName(..), loadRecentFile, saveRecentFile
  , loadHistoryFile, saveHistoryFile )
import Cmedit.Definition (DefReq(..))
import qualified Cmedit.Definition as D
import Cmedit.Editor
import Cmedit.Gfx
import Cmedit.Image (Image, imgW, imgH, sniffImage, decodeImage, scaleRGBA)
import Cmedit.Input
import Cmedit.Render
import Cmedit.Search (SearchReq(..), FileResult(..))
import qualified Cmedit.Search as S
import qualified Cmedit.QuickOpen as Q
import Cmedit.Term
import Cmedit.TextBuffer
import Cmedit.Types

-- | Run the editor: open any named files, then loop until the user quits.
-- @cfgWarns@ are problems found in the user's config file, surfaced on the
-- status line once the screen is up.
run :: Config -> [String] -> [FilePath] -> Bool -> IO ()
run cfg cfgWarns files readOnly = do
  inTty  <- hIsTerminalDevice stdin
  outTty <- hIsTerminalDevice stdout
  if not (inTty && outTty)
    then hPutStrLn stderr "cmedit: stdin and stdout must be a terminal"
    else runTui cfg cfgWarns files readOnly

runTui :: Config -> [String] -> [FilePath] -> Bool -> IO ()
runTui cfg cfgWarns files readOnly = do
  -- Configure the handles BEFORE entering raw mode. GHC's hSetBuffering /
  -- hSetEcho snapshot the current terminal state and restore it when the
  -- standard handles are finalised at exit; if we entered raw mode first, that
  -- snapshot would be the raw state and GHC would "restore" the terminal to
  -- raw, leaving it unusable after we quit. Doing it first means GHC snapshots
  -- the pristine state, and our own restore agrees with it.
  configureHandles
  -- Give the RTS a few real cores (it defaults to one capability, which would
  -- serialise the search walker's worker pool), but cap it well below the
  -- machine so a broad workspace search can't monopolise the box.
  procs <- getNumProcessors
  setNumCapabilities (max 1 (min 4 (procs - 1)))
  -- Enter raw mode (capturing pristine attributes) and always restore them on
  -- the way out, even if an async signal interrupts the loop.
  bracket setRawMode restoreTermAttrs $ \_ -> do
    size <- getTerminalSize
    recents <- loadRecentFile
    (findHist, replHist) <- loadHistoryFile
    ed0'' <- buildInitialEditor cfg recents size files readOnly
    let ed0' = ed0'' { edFindHist = findHist, edReplHist = replHist }
    -- Config-file problems beat the welcome text (the user should fix them),
    -- but not a real load message.
    let ed0 = case cfgWarns of
                (w : _) | edStatus ed0' == "" || T.isPrefixOf "Welcome" (edStatus ed0') ->
                  setStatus (T.pack ("config: " ++ w
                              ++ (case length cfgWarns of
                                    1 -> ""
                                    n -> " (+" ++ show (n - 1) ++ " more)"))) ed0'
                _ -> ed0'
    -- The kernel often knows the terminal's pixel size (ws_xpixel/ws_ypixel);
    -- when it does, the image view gets the true cell aspect ratio from the
    -- first frame. The XTWINOPS replies refine or supply it later.
    ed0Px <- applyTerminalPixels ed0
    editorRef <- newIORef ed0Px
    prevRef   <- newIORef (Nothing :: Maybe Screen)
    titleRef  <- newIORef ""
    q         <- newTQueueIO
    loadQ     <- newTQueueIO
    searchQ   <- newTQueueIO
    searchGen <- newTVarIO 0
    defGen    <- newTVarIO 0
    dirMtimes <- newIORef M.empty
    focused   <- newIORef True
    recentsRef <- newIORef (map rePath (edRecent ed0))
    quickGen  <- newTVarIO 0
    capsRef   <- newIORef defaultCaps
    pointerRef <- newIORef "default"
    themeRef  <- newIORef (Nothing :: Maybe ThemeName)
    gfxRef    <- newIORef (Nothing :: Maybe GfxKey)
    let drv = Drv loadQ searchQ searchGen defGen dirMtimes focused recentsRef quickGen
                  capsRef pointerRef themeRef gfxRef
    src       <- mkHandleSource stdin
    clickRef  <- newIORef (ClickState 0 (-1) (-1) 0)
    mainTid   <- myThreadId

    -- SIGWINCH pushes a resize event; terminating signals interrupt the loop.
    installResizeHandler (atomically (writeTQueue q KResize))
    installInterruptHandlers mainTid

    -- Background reader: parse keys and enqueue them.
    void $ forkIO $ readerLoop src q clickRef

    bracket_
      (enterScreen ed0)
      leaveScreen
      ((do renderNow drv editorRef prevRef titleRef
           eventLoop editorRef prevRef titleRef q drv src)
        -- Always record the recents (with final cursor positions) and the
        -- find/replace history on the way out — including SIGTERM/SIGHUP.
        `finally` (do edF <- readIORef editorRef
                      saveRecentFile (recentsForPersist edF)
                      saveHistoryFile (edFindHist edF) (edReplHist edF)))

-- Construct the starting editor. Directory arguments open as the workspace
-- folder (the explorer panel); file arguments are loaded (the first becomes the
-- active document, the rest join the open-files list).
buildInitialEditor :: Config -> [RecentEntry] -> (Int, Int) -> [FilePath] -> Bool -> IO Editor
buildInitialEditor cfg recents size args readOnly = do
  tagged <- mapM (\a -> (,) a <$> doesDirectoryExist a) args
  let dirs  = [ a | (a, True)  <- tagged ]
      files = [ a | (a, False) <- tagged ]
  base <- loadInitialFiles cfg recents size files readOnly
  case dirs of
    []       -> pure base
    (d0 : _) -> do
      cpath   <- canonicalizeSafe d0
      entries <- listEntries cpath
      let ed = explorerStart cpath entries base
      -- Keep editing focus when files were also opened; else focus the panel.
      pure (if null files then ed else ed { edFocus = FEdit })

-- Load the file arguments (no directories) into a fresh editor. The persisted
-- recents are installed first so opening a remembered file restores its cursor.
loadInitialFiles :: Config -> [RecentEntry] -> (Int, Int) -> [FilePath] -> Bool -> IO Editor
loadInitialFiles cfg recents size files readOnly = do
  let base = (newEditor size cfg) { edStatus = "Welcome to CMeDit \x2014 press F1 for help"
                                  , edRecent = recents }
  case files of
    [] -> pure base
    (f0 : rest) -> do
      -- Store canonical paths so re-opening the same file (e.g. via the browser,
      -- which yields absolute paths) is recognised as already-open.
      f0' <- canonicalizeSafe f0
      ed0' <- openPath setLoaded imageLoaded f0' base
      let ed1 = if readOnly then ed0' { edReadOnly = True } else ed0'
      foldM addOne ed1 rest
  where
    -- 2nd+ files named on the command line; silently skip binary/too-large ones.
    addOne e f0 = do
      f <- canonicalizeSafe f0
      o <- classifyFile f
      pure $ case o of
        OutText p lr  -> addDocument p lr e
        OutImage p im -> addImageDocument p im e
        OutError _    -> e

------------------------------------------------------------------------------
-- Terminal screen lifecycle

enterScreen :: Editor -> IO ()
enterScreen ed = do
  emit $ pushTitle   -- save the user's title so ours can be popped off on exit
           <> enterAltScreen <> enableMouse <> enableBracketedPaste <> enableKittyKeys
           <> enableFocusEvents
           <> hideCursor <> clearScreen <> setTitle (windowTitle ed)
           -- Capability probes: background colour (theme=auto), pixel
           -- geometry (image aspect), XTVERSION (SGR fingerprint), kitty
           -- graphics, the REP behaviour probe, and DA1 (sixel; also a fence
           -- every terminal answers). Replies arrive interleaved with keys
           -- and are decoded to KReply events; a terminal that answers none
           -- of them simply keeps every fallback behaviour.
           <> queryBg <> queryCellPx <> queryTextPx <> queryVersion
           <> kittyGfxProbe <> repProbe <> queryDA1
  hFlush stdout

leaveScreen :: IO ()
leaveScreen = do
  emit $ resetSgr <> kittyGfxDeleteAll <> resetCursorColor <> setPointerShape "default"
           <> disableFocusEvents <> disableKittyKeys <> disableBracketedPaste
           <> disableMouse <> showCursor <> leaveAltScreen <> popTitle
  hFlush stdout

emit :: Builder -> IO ()
emit = hPutBuilder stdout

------------------------------------------------------------------------------
-- Input reader thread

-- Last left-button press: monotonic time, cell, and accumulated click count.
data ClickState = ClickState !Double !Int !Int !Int

-- Max gap between presses (at the same cell) to count as a multi-click.
doubleClickSecs :: Double
doubleClickSecs = 0.4

readerLoop :: ByteSource -> TQueue Key -> IORef ClickState -> IO ()
readerLoop src q clickRef = go
  where
    go = do
      k0 <- nextKey src
      k  <- annotateClicks clickRef k0
      atomically (writeTQueue q k)
      case k of
        KUnknown [] -> pure ()    -- EOF
        _           -> go

-- Tag a left-button press with its click count (2 = double, 3 = triple) by
-- comparing its time and cell to the previous press; everything else passes
-- through. Terminals don't report click counts, so we time them ourselves.
annotateClicks :: IORef ClickState -> Key -> IO Key
annotateClicks ref k = case k of
  KMouse me | mePressed me && not (meDrag me) && meButton me == MBLeft -> do
    now <- getMonotonicTime
    ClickState lt lr lc ln <- readIORef ref
    let sameSpot = meRow me == lr && meCol me == lc
        n | now - lt <= doubleClickSecs && sameSpot = if ln >= 3 then 1 else ln + 1
          | otherwise                               = 1
    writeIORef ref (ClickState now (meRow me) (meCol me) n)
    pure (KMouse me { meClicks = n })
  _ -> pure k

------------------------------------------------------------------------------
-- Driver context (queues + shared state the effect handlers need)

data Drv = Drv
  { drvLoadQ     :: !(TQueue LoadOutcome)
  , drvSearchQ   :: !(TQueue SearchMsg)
  , drvSearchGen :: !(TVar Int)       -- ^ id of the newest search; the walker bails when it changes.
  , drvDefGen    :: !(TVar Int)       -- ^ id of the newest definition scan (independent of searches).
  , drvDirMtimes :: !(IORef (M.Map FilePath UTCTime))  -- ^ Each listed dir's mtime at listing time (for the freshness poll).
  , drvFocused   :: !(IORef Bool)     -- ^ Terminal focus, if the terminal reports it (defaults True).
  , drvRecents   :: !(IORef [FilePath])  -- ^ Paths of the recents list as last persisted (order matters).
  , drvQuickGen  :: !(TVar Int)       -- ^ id of the newest quick-open walk (independent of searches).
  , drvCaps      :: !(IORef TermCaps) -- ^ What the startup probes learned about the terminal.
  , drvPointer   :: !(IORef String)   -- ^ The last OSC 22 pointer shape emitted (hover hint).
  , drvTheme     :: !(IORef (Maybe ThemeName)) -- ^ The theme whose cursor colour (OSC 12) is current.
  , drvGfx       :: !(IORef (Maybe GfxKey))    -- ^ The pixel-image placement currently on screen, if any.
  }

-- | Identity of an on-screen kitty/sixel placement: re-emitted only when any
-- part of this changes (or after a full redraw invalidated the terminal).
data GfxKey = GfxKey
  { gkPath :: !(Maybe FilePath)
  , gkCrop :: !(Int, Int, Int, Int)
  , gkGeom :: !(Int, Int, Int, Int)     -- ^ (top, left, tw, th) of the text area.
  , gkPx   :: !(Int, Int)               -- ^ Cell pixel size in effect.
  , gkKind :: !GfxKind
  } deriving (Eq, Show)

-- | Persist the recent-files list when its path set/order changed (opens,
-- closes and saves reorder it; cursor moves alone do not, so this is not a
-- per-keystroke write). Final positions are written once more on exit.
maybePersistRecents :: Drv -> IORef Editor -> IO ()
maybePersistRecents drv editorRef = do
  ed <- readIORef editorRef
  let paths = map rePath (edRecent ed)
  old <- readIORef (drvRecents drv)
  when (paths /= old) $ do
    saveRecentFile (recentsForPersist ed)
    writeIORef (drvRecents drv) paths

-- A message from a background search/replace worker to the main loop.
data SearchMsg
  = SMFile     !Int !FileResult   -- ^ gen, one file's disk matches.
  | SMProgress !Int !Int          -- ^ gen, files scanned so far.
  | SMDone     !Int !Bool         -- ^ gen, whether the global cap was hit.
  | SMReplaceDone !Int            -- ^ total occurrences replaced (open + on-disk).
  | SMDefFile  !Int !FileResult   -- ^ def-gen, one file's definition sites.
  | SMDefDone  !Int               -- ^ def-gen, the definition scan finished.
  | SMQuickFiles !Int ![FilePath] -- ^ quick-gen, a batch of workspace-relative file paths.
  | SMQuickDone  !Int             -- ^ quick-gen, the quick-open walk finished.

------------------------------------------------------------------------------
-- Main loop

-- One thing the loop woke up for.
data LoopAction = GotKey !Key | GotLoad !LoadOutcome | GotSearch !SearchMsg | Tick | FsTick

-- Spinner animation interval (µs) while a background file load is in progress.
spinnerDelayUs :: Int
spinnerDelayUs = 100000

-- Interval (µs) between filesystem freshness passes. Each pass is a handful of
-- stat calls (open files + expanded explorer directories) — far cheaper than a
-- keystroke; directories are re-listed only when their mtime actually moved.
fsPollDelayUs :: Int
fsPollDelayUs = 2000000

eventLoop :: IORef Editor -> IORef (Maybe Screen) -> IORef String
          -> TQueue Key -> Drv -> ByteSource -> IO ()
eventLoop editorRef prevRef titleRef q drv _src = registerDelay fsPollDelayUs >>= loop
  where
    loadQ   = drvLoadQ drv
    searchQ = drvSearchQ drv
    loop pollT = do
      -- While a file is loading or a search is running, arm a timer so the
      -- spinner animates; otherwise block purely on input / load / search results.
      ed0 <- readIORef editorRef
      -- Animate only when something is visibly moving: a file load spinner, a
      -- running search whose panel is on screen, or the About-box wordmark
      -- (which ticks faster, and stops re-arming once it has settled).
      mtick <- if isJust (edLoading ed0) || (searchRunning ed0 && searchViewActive ed0)
                 then Just <$> registerDelay spinnerDelayUs
                 else if aboutAnimating ed0
                   then Just <$> registerDelay aboutTickUs
                   else pure Nothing
      action <- atomically $
                (GotLoad   <$> readTQueue loadQ)
        `orElse` (GotSearch <$> readTQueue searchQ)
        `orElse` (GotKey    <$> readTQueue q)
        `orElse` (case mtick of
                    Just tv -> readTVar tv >>= check >> pure Tick
                    Nothing -> retry)
        `orElse` (readTVar pollT >>= check >> pure FsTick)
      case action of
        -- A background load finished: install it, apply any pending result-jump,
        -- and drop the spinner.
        GotLoad o -> do
          modifyIORef' editorRef (applyPendingJump . endLoading . applyOutcome setLoadedNew imageLoadedNew o)
          maybePersistRecents drv editorRef
          case o of
            OutText p _  -> notifyUnfocused drv ("Finished loading " ++ takeFileName p)
            OutImage p _ -> notifyUnfocused drv ("Finished loading " ++ takeFileName p)
            OutError _   -> pure ()
          renderNow drv editorRef prevRef titleRef
          loop pollT
        -- A streamed search/replace result: drain the whole backlog and fold it
        -- in before a *single* repaint — a broad search (a common word over a huge
        -- tree) floods thousands of results, and repainting per result would peg
        -- the terminal and freeze the UI. Only repaint when the panel is on screen.
        GotSearch msg -> do
          rest <- atomically (flushTQueue searchQ)
          mapM_ handleSearchMsg (msg : rest)
          ed <- readIORef editorRef
          when (searchViewActive ed || edFocus ed == FDefPick || edFocus ed == FQuickOpen) $
            renderNow drv editorRef prevRef titleRef
          loop pollT
        -- Advance the spinner(s) / About animation one frame.
        Tick -> do
          modifyIORef' editorRef (tickLoading . searchTick . tickAbout)
          renderNow drv editorRef prevRef titleRef
          loop pollT
        -- Periodic filesystem freshness pass; repaint only if it changed anything.
        FsTick -> do
          changed <- pollFs drv editorRef
          when changed $ renderNow drv editorRef prevRef titleRef
          registerDelay fsPollDelayUs >>= loop
        -- A key: drain everything else already queued and apply the whole batch
        -- before a single repaint (so held keys / fast typing never lag).
        GotKey k -> do
          rest <- atomically (flushTQueue q)
          keep <- applyBatch (k : rest)
          when keep $ do
            maybePersistRecents drv editorRef
            renderNow drv editorRef prevRef titleRef
            loop pollT

    handleSearchMsg msg = case msg of
      SMFile gen fr     -> modifyIORef' editorRef (searchFileFound gen fr)
      SMProgress gen n  -> modifyIORef' editorRef (searchProgress gen n)
      SMDone gen trunc  -> do
        modifyIORef' editorRef (searchDone gen trunc)
        cur <- readTVarIO (drvSearchGen drv)
        when (gen == cur) $ do
          ed <- readIORef editorRef
          let n = maybe 0 (S.ssTotal) (edSearch ed)
          notifyUnfocused drv ("Search finished: " ++ show n
                               ++ " match" ++ (if n == 1 then "" else "es"))
      SMDefFile gen fr  -> modifyIORef' editorRef (defFound gen fr)
      SMDefDone gen     -> modifyIORef' editorRef (defDone gen)
      SMQuickFiles gen ps -> modifyIORef' editorRef (quickFilesFound gen (map T.pack ps))
      SMQuickDone gen   -> modifyIORef' editorRef (quickDone gen)
      SMReplaceDone tot -> do
        ed <- readIORef editorRef
        let (ed1, effs) = replaceDone tot ed
        ed2 <- performEffects drv effs ed1
        writeIORef editorRef ed2
        notifyUnfocused drv ("Replace All finished: " ++ show tot
                             ++ " occurrence" ++ (if tot == 1 then "" else "s"))

    -- Apply each key in order (side effects still run per key). Returns False to
    -- stop the loop (quit or EOF) without a trailing render.
    applyBatch [] = pure True
    applyBatch (k : ks) = case k of
      KUnknown [] -> pure False              -- EOF: exit
      KResize     -> do
        sz <- getTerminalSize
        modifyIORef' editorRef (resize sz)
        -- Pixel geometry may have changed with the size (or a font change).
        readIORef editorRef >>= applyTerminalPixels >>= writeIORef editorRef
        writeIORef prevRef Nothing           -- a resize forces a full redraw
        applyBatch ks
      -- Terminal focus tracking: on focus-in, run a freshness pass right away —
      -- background changes (a git pull in another window) show up the moment
      -- the user comes back, without the poll having to run while we're away.
      -- The background-colour query re-runs too, so theme=auto follows a
      -- system light/dark switch made while we were away.
      KFocus f    -> do
        writeIORef (drvFocused drv) f
        when f $ do
          void (pollFs drv editorRef)
          emit queryBg
          hFlush stdout
        applyBatch ks
      -- A reply to one of our startup queries: fold it into the caps /
      -- editor state. Never reaches the pure update.
      KReply rep  -> do
        applyReplyIO drv editorRef rep
        applyBatch ks
      _ -> do
        -- Hover feedback: suggest a pointer shape for whatever the mouse is
        -- over (emitted only on transitions; ignored by plain terminals).
        case k of
          KMouse me -> do
            ed <- readIORef editorRef
            let shape = pointerShapeFor ed (meRow me) (meCol me)
            old <- readIORef (drvPointer drv)
            when (shape /= old) $ do
              writeIORef (drvPointer drv) shape
              emit (setPointerShape shape)   -- flushed with the batch's repaint
          _ -> pure ()
        ed <- readIORef editorRef
        let (ed1, effs) = update k ed
        ed2 <- performEffects drv effs ed1
        writeIORef editorRef ed2
        if edQuit ed2 then pure False else applyBatch ks

------------------------------------------------------------------------------
-- Terminal replies / capabilities

-- | Read the kernel's idea of the terminal pixel size and derive the cell
-- geometry from it. Terminals that leave ws_xpixel/ws_ypixel at zero keep
-- whatever the editor already knows (the XTWINOPS replies fill it in, or the
-- image view keeps the classic 2:1 assumption).
applyTerminalPixels :: Editor -> IO Editor
applyTerminalPixels ed = do
  mpx <- getTerminalPixels
  let (rows, cols) = edSize ed
  pure $ case mpx of
    Just (x, y) | rows > 0, cols > 0, x >= cols, y >= rows ->
      setCellPx (x `div` cols, y `div` rows) ed
    _ -> ed

-- | Fold a terminal reply into the capability record and, where it carries
-- editor-visible state (background colour, pixel geometry), the editor.
applyReplyIO :: Drv -> IORef Editor -> TermReply -> IO ()
applyReplyIO drv editorRef rep = do
  modifyIORef' (drvCaps drv) (applyReply rep)
  case rep of
    -- theme=auto: dark or light follows the reported background.
    TrBgColor r g b -> modifyIORef' editorRef (setDetectedDark (isDarkRgb r g b))
    -- Cell pixel size, directly (XTWINOPS 16) …
    TrCellPx h w | w > 0 && h > 0 -> modifyIORef' editorRef (setCellPx (w, h))
    -- … or derived from the text-area size (XTWINOPS 14) when nothing better
    -- is known (the ioctl and the 16t reply both take precedence).
    TrTextPx h w -> modifyIORef' editorRef $ \ed ->
      let (rows, cols) = edSize ed
      in if isNothing (edCellPx ed) && rows > 0 && cols > 0 && w >= cols && h >= rows
           then setCellPx (w `div` cols, h `div` rows) ed
           else ed
    _ -> pure ()

-- | Post a desktop notification (OSC 9) — but only when the terminal is
-- unfocused; a user watching the screen doesn't need to be pinged.
notifyUnfocused :: Drv -> String -> IO ()
notifyUnfocused drv msg = do
  focused <- readIORef (drvFocused drv)
  unless focused $ do
    emit (notify msg)
    hFlush stdout

------------------------------------------------------------------------------
-- Filesystem freshness (the explorer / stale-file poll)

-- | One freshness pass: refresh the stale-on-disk flags of all open documents
-- (the ◆ markers and Revert follow them) and re-list any expanded explorer
-- directory whose mtime moved since it was listed, merging the fresh listing
-- in without disturbing expansion or selection. Returns True when anything
-- visible may have changed. Skipped while a background load is pending
-- (nothing should race the install) or while the terminal is unfocused (the
-- focus-in event runs a pass immediately instead).
pollFs :: Drv -> IORef Editor -> IO Bool
pollFs drv editorRef = do
  ed      <- readIORef editorRef
  focused <- readIORef (drvFocused drv)
  if isJust (edLoading ed) || not focused
    then pure False
    else do
      -- Open documents: newer on disk than the recorded baseline?
      let paths = [ p | Just p <- edPath ed : map docPath (edBefore ed ++ edAfter ed) ]
      stats <- forM paths (\p -> (,) p <$> fileMtime p)
      let ed1 = noteDiskMtimes stats ed
          flagsChanged =
            (edDiskChanged ed1, map docDiskChanged (edBefore ed1 ++ edAfter ed1))
              /= (edDiskChanged ed, map docDiskChanged (edBefore ed ++ edAfter ed))
      -- Explorer: re-list expanded directories whose mtime moved.
      (ed2, relisted) <- case edExplorer ed1 of
        Nothing -> pure (ed1, False)
        Just br -> do
          seen <- readIORef (drvDirMtimes drv)
          (edN, seen', hit) <- foldM checkDir (ed1, seen, False) (Br.expandedDirPaths br)
          writeIORef (drvDirMtimes drv) seen'
          pure (edN, hit)
      writeIORef editorRef ed2
      pure (flagsChanged || relisted)
  where
    checkDir (e, seen, hit) d = do
      mt <- dirMtimeHiRes d
      case mt of
        Nothing  -> pure (e, M.delete d seen, hit)   -- gone; its parent's re-list drops the node
        Just now -> case M.lookup d seen of
          Just old | old == now -> pure (e, seen, hit)
          Nothing  ->                                -- first sighting: record the baseline only
            pure (e, M.insert d now seen, hit)
          _        -> do                             -- changed: fold in a fresh listing
            entries <- listEntries d
            pure (explorerLoaded d entries e, M.insert d now seen, True)

-- Re-list one explorer directory right now (after a create/rename/delete),
-- keeping the poll's mtime baseline in step.
refreshExplorerDir :: Drv -> FilePath -> Editor -> IO Editor
refreshExplorerDir drv dir ed
  | isNothing (edExplorer ed) = pure ed
  | otherwise = do
      recordDirMtime drv dir
      entries <- listEntries dir
      pure (explorerLoaded dir entries ed)

dropTrailingSlash :: FilePath -> FilePath
dropTrailingSlash = reverse . dropWhile (== '/') . reverse

-- A short, readable reason for a failed file operation.
fsOpError :: SomeException -> String
fsOpError = show

-- Hi-res mtime for the directory poll: seconds-only granularity would miss a
-- change landing within the same second the directory was listed.
-- getModificationTime keeps sub-second precision where the OS provides it.
dirMtimeHiRes :: FilePath -> IO (Maybe UTCTime)
dirMtimeHiRes path = do
  r <- try (getModificationTime path) :: IO (Either SomeException UTCTime)
  pure (either (const Nothing) Just r)

-- Remember a directory's mtime at the moment it is (re-)listed, so the poll
-- has a baseline. Recorded *before* the listing is taken: a change racing the
-- listing then still bumps the mtime past the baseline and is caught next pass.
recordDirMtime :: Drv -> FilePath -> IO ()
recordDirMtime drv d = do
  mt <- dirMtimeHiRes d
  modifyIORef' (drvDirMtimes drv) (maybe (M.delete d) (M.insert d) mt)

------------------------------------------------------------------------------
-- Effects

performEffects :: Drv -> [Effect] -> Editor -> IO Editor
performEffects _ [] ed = pure ed
performEffects drv (e : es) ed = perform drv e ed >>= performEffects drv es

perform :: Drv -> Effect -> Editor -> IO Editor
perform drv eff ed = let loadQ = drvLoadQ drv in case eff of
  EffCopy txt -> do
    outcome <- copyToClipboard txt
    case outcome of
      UseOsc52 -> emit (osc52Copy txt) >> hFlush stdout
      _        -> pure ()
    pure (confirmCopyOutcome outcome ed)

  EffPaste -> do
    mt <- pasteFromClipboard
    let txt = fromMaybe (edClipboard ed) mt
    pure (applyPaste txt ed)

  EffSaveTo path -> do
    -- If the active document is in CSV table mode, flush the table to the
    -- line buffer first so what we write matches what is shown; then apply
    -- the configured save-time cleanups (trim whitespace / final newline).
    let ed' = applySaveFixups (syncCsvToBuffer ed)
    res <- saveFile path (edEncoding ed') (edLineEnding ed') (edFinalNewline ed') (edBuffer ed')
    case res of
      Right (n, mt) -> let (ed1, effs) = onSaved n mt ed' in performEffects drv effs ed1
      Left err      -> pure (setError err ed')

  EffOpen path -> do
    -- Canonicalise so the already-open check (in setLoadedNew/imageLoadedNew)
    -- matches files opened earlier by any route.
    cpath <- canonicalizeSafe path
    msz <- fileSizeSafe cpath
    case msz of
      -- Big (but openable) files load on a background thread with a spinner, so
      -- the event loop keeps painting; small/new/oversized files resolve inline.
      Just sz | sz > asyncThresholdBytes && sz <= maxOpenBytes -> do
        void $ forkIO (classifyFile cpath >>= atomically . writeTQueue loadQ)
        pure (beginLoading (takeFileName cpath) ed)
      -- Small files install inline; apply any pending result-jump immediately.
      _ -> applyPendingJump . flip (applyOutcome setLoadedNew imageLoadedNew) ed <$> classifyFile cpath

  -- Reload the active file in place, discarding unsaved edits (the Revert
  -- command). Goes through the same magic-byte sniff as opening.
  EffRevert path -> openPath revertLoaded imageLoaded path ed

  EffStatFile path -> do
    mt <- fileMtime path
    pure (noteDiskMtime mt ed)

  EffBrowse mhint -> do
    dir0 <- case mhint of
              Just p | not (null p) -> pure p
              _                     -> getCurrentDirectory
    dir <- canonicalizeSafe dir0
    entries <- listEntries dir
    pure (startBrowser dir entries ed)

  EffListDir path -> do
    entries <- listEntries path
    pure (browserLoaded path entries ed)

  EffExplorerOpen path -> do
    cpath   <- canonicalizeSafe path
    recordDirMtime drv cpath
    entries <- listEntries cpath
    pure (explorerStart cpath entries ed)

  EffExplorerList path -> do
    recordDirMtime drv path
    entries <- listEntries path
    pure (explorerLoaded path entries ed)

  -- Explorer file management. Each op runs, then the affected directory is
  -- re-listed immediately (the freshness poll would catch it anyway, but the
  -- user is looking right at it).
  EffCreatePath raw -> do
    let isDir = "/" `isPrefixOf` reverse raw    -- trailing slash
        path  = if isDir then dropTrailingSlash raw else raw
    r <- try $ if isDir
           then createDirectoryIfMissing True path
           else do
             createDirectoryIfMissing True (takeDirectory path)
             fileThere <- doesFileExist path
             dirThere  <- doesDirectoryExist path
             when (fileThere || dirThere) $
               ioError (userError (takeFileName path ++ " already exists"))
             BS.writeFile path BS.empty
    case r :: Either SomeException () of
      Left e   -> pure (setError (fsOpError e) ed)
      Right () -> do
        cpath <- canonicalizeSafe path
        ed1 <- refreshExplorerDir drv (takeDirectory cpath) ed
        let ed2 = selectInExplorer cpath
                    (fileOpDone ("Created " ++ takeFileName path) ed1)
        -- A new file opens straight away so you can start typing.
        if isDir then pure ed2 else openPath setLoadedNew imageLoadedNew cpath ed2

  EffRenamePath old new -> do
    r <- try (renamePath old new)
    case r :: Either SomeException () of
      Left e   -> pure (setError (fsOpError e) ed)
      Right () -> do
        cnew <- canonicalizeSafe new
        ed1 <- refreshExplorerDir drv (takeDirectory old) ed
        ed2 <- if takeDirectory cnew == takeDirectory old
                 then pure ed1 else refreshExplorerDir drv (takeDirectory cnew) ed1
        pure (selectInExplorer cnew
                (fileOpDone ("Renamed to " ++ takeFileName new)
                   (renamePaths old cnew ed2)))

  EffDeletePath path -> do
    isDir <- doesDirectoryExist path
    r <- try (if isDir then removeDirectoryRecursive path else removeFile path)
    case r :: Either SomeException () of
      Left e   -> pure (setError (fsOpError e) ed)
      Right () -> do
        ed1 <- refreshExplorerDir drv (takeDirectory path) ed
        pure (fileOpDone ("Deleted " ++ takeFileName path) ed1)

  EffSetTitle t -> do
    emit (setTitle t) >> hFlush stdout
    pure ed

  EffBell -> do
    emit (char7 '\BEL') >> hFlush stdout
    pure ed

  -- Kick off a workspace search: canonicalise the root, seed the panel with the
  -- open documents' in-memory matches (so unsaved edits are searched too), then
  -- fork the disk walker which streams the rest back over the search queue.
  EffStartSearch req -> do
    canonRoot <- canonicalizeSafe (sqRoot req)
    let req' = req { sqRoot = canonRoot }
        seed = searchOpenDocs canonRoot req' ed
        ed1  = searchSeed (sqGen req) canonRoot seed ed
    atomically (writeTVar (drvSearchGen drv) (sqGen req))
    void $ forkIO (runWalker (drvSearchQ drv) (drvSearchGen drv) req')
    pure ed1

  -- Start a quick-open walk: canonicalise the root, seed the recents-first
  -- ordering, then stream the tree's files back in batches.
  EffQuickOpen gen root -> do
    canonRoot <- canonicalizeSafe root
    atomically (writeTVar (drvQuickGen drv) gen)
    void $ forkIO (runQuickWalker (drvSearchQ drv) (drvQuickGen drv) gen canonRoot)
    pure (quickOpenSeed gen canonRoot ed)

  -- Kick off a background go-to-definition scan (the pure layer has already
  -- seeded the picker from the open documents' buffers).
  EffFindDefs req -> do
    canonRoot <- canonicalizeSafe (dfRoot req)
    atomically (writeTVar (drvDefGen drv) (dfGen req))
    void $ forkIO (runDefWalker (drvSearchQ drv) (drvDefGen drv) req { dfRoot = canonRoot })
    pure ed

  -- Rewrite the closed files for a large workspace Replace All, then report the total.
  EffReplaceOnDisk req -> do
    let subst = replaceSubst (rrCase req) (rrWord req) (rrRegex req) (rrTerm req) (rrRepl req)
    diskCount <- foldM (\acc p -> do
                          r <- replaceInFile p subst
                          pure (acc + either (const 0) id r)) 0 (rrPaths req)
    atomically (writeTQueue (drvSearchQ drv) (SMReplaceDone (rrOpenCount req + diskCount)))
    pure ed

  -- Staged Replace All: open each closed file with the replacement applied (as an
  -- unsaved document), then expand the explorer so the changed files are visible.
  EffStageReplace req -> do
    let subst = replaceSubst (rrCase req) (rrWord req) (rrRegex req) (rrTerm req) (rrRepl req)
    (edStaged, staged) <- foldM (\(e, c) p -> do
        ebs <- try (BS.readFile p) :: IO (Either SomeException BS.ByteString)
        case ebs of
          Right bs | not (looksBinary bs) ->
            let lr      = loadFromBytes False Nothing bs
                (e', k) = addStagedDoc p lr subst e
            in pure (e', c + k)
          _ -> pure (e, c)) (ed, 0) (rrPaths req)
    edRevealed <- revealInExplorer (modifiedDocPaths edStaged) edStaged
    pure (stageReplaceDone (rrOpenCount req + staged) edRevealed)

  -- Save every open document that has unsaved changes (File ▸ Save All).
  EffSaveAll -> do
    let edFixed = applySaveFixupsAll ed
    results <- forM (modifiedDocsToSave edFixed) $ \(p, enc, le, fin, buf) -> do
      r <- saveFile p enc le fin buf
      pure (p, either (const Nothing) snd r)   -- (path, Just mtime) on success, Nothing on error
    pure (savedAll [ (p, mt) | (p, mt) <- results, isJust mt ] edFixed)

-- The result of reading and classifying a file, before it touches the editor.
data LoadOutcome
  = OutText  !FilePath !LoadResult
  | OutImage !FilePath !Image
  | OutError !String

-- Read and classify a path without touching the editor. Refuses files that are
-- too large or binary (so a huge blob can never be decoded into millions of junk
-- lines and hang the app), decodes images by magic bytes, and otherwise decodes
-- text. A missing path becomes a new empty buffer (so opening a not-yet-created
-- file still works). This runs on the main thread for small files and on a
-- background thread for large ones.
classifyFile :: FilePath -> IO LoadOutcome
classifyFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (OutText path emptyLoadResult)  -- new file
    else do
      msz <- fileSizeSafe path
      case msz of
        Just sz | sz > maxOpenBytes ->
          pure (OutError (takeFileName path ++ ": too large to open ("
                          ++ humanSize sz ++ ", limit " ++ humanSize maxOpenBytes ++ ")"))
        _ -> do
          ebs <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
          case ebs of
            Left e   -> pure (OutError (show e))
            Right bs -> case sniffImage bs of
              Just _  -> pure (either (\err -> OutError (takeFileName path ++ ": " ++ err))
                                      (OutImage path) (decodeImage bs))
              Nothing
                | looksBinary bs ->
                    pure (OutError (takeFileName path ++ ": binary file ("
                                    ++ humanSize (fromIntegral (BS.length bs))
                                    ++ ") \x2014 cannot be edited"))
                | otherwise -> do
                    ro <- not <$> canWrite path
                    mt <- fileMtime path
                    pure (OutText path (loadFromBytes ro mt bs))

-- Apply a load outcome to the editor via the matching pure installer.
applyOutcome :: (FilePath -> LoadResult -> Editor -> Editor)
             -> (FilePath -> Image -> Editor -> Editor)
             -> LoadOutcome -> Editor -> Editor
applyOutcome installText installImage o ed = case o of
  OutText p lr  -> installText p lr ed
  OutImage p im -> installImage p im ed
  OutError msg  -> setError msg ed

-- Open a path synchronously (used at startup and for Revert). The interactive
-- Open path (EffOpen) loads large files on a background thread instead.
openPath :: (FilePath -> LoadResult -> Editor -> Editor)
         -> (FilePath -> Image -> Editor -> Editor)
         -> FilePath -> Editor -> IO Editor
openPath installText installImage path ed =
  flip (applyOutcome installText installImage) ed <$> classifyFile path

-- Files larger than this (but within 'maxOpenBytes') load on a background thread
-- with a spinner, so the UI stays responsive; smaller ones load inline.
asyncThresholdBytes :: Integer
asyncThresholdBytes = 2 * 1024 * 1024

-- List a directory for the file browser as (fullPath, isDirectory, size) tuples
-- (size is Nothing for directories). Unreadable directories yield an empty
-- listing rather than an error.
listEntries :: FilePath -> IO [(FilePath, Bool, Maybe Integer)]
listEntries dir = do
  r <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case r of
    Left _      -> pure []
    Right names -> forM (sort names) $ \n -> do
      let p = dir </> n
      isd <- doesDirectoryExist p
      sz  <- if isd then pure Nothing else fileSizeSafe p
      pure (p, isd, sz)

-- | Expand the explorer tree so the given (changed) files become visible: load
-- any not-yet-listed ancestor directories, expand them, and select the first
-- file. Used after a staged Replace All so the user can walk the dirty files.
revealInExplorer :: [FilePath] -> Editor -> IO Editor
revealInExplorer paths ed = case edExplorer ed of
  Nothing  -> pure ed
  Just br0 -> do
    let root    = fnPath (brRoot br0)
        rootPfx = if not (null root) && last root == '/' then root else root ++ "/"
        targets = filter (rootPfx `isPrefixOf`) paths
    br1 <- foldM (revealPath root) br0 targets
    let br2 = case targets of (p : _) -> Br.selectPath p br1; [] -> br1
        th  = explorerTreeHeight ed
    pure ed { edExplorer = Just (Br.scrollInto th br2) }

-- Ensure every ancestor directory of @p@ (under @root@) is loaded and expanded.
revealPath :: FilePath -> Browser -> FilePath -> IO Browser
revealPath root br0 p = foldM ensure br0 (ancestorDirs root p)
  where
    ensure br dir = case Br.nodeAt dir br of
      Just n | isNothing (fnChildren n) -> do    -- not listed yet: load then expand
        entries <- listEntries dir
        pure (Br.fillChildren dir entries br)
      Just _  -> pure (Br.expandAt dir br)        -- already loaded: just expand
      Nothing -> pure br                          -- parent not loaded (shouldn't happen going shallow→deep)

-- Directories from just under @root@ down to @p@'s parent, shallowest first.
ancestorDirs :: FilePath -> FilePath -> [FilePath]
ancestorDirs root p =
  let parts = filter (`notElem` [".", ""]) (splitDirectories (takeDirectory (makeRelative root p)))
      go _   []       = []
      go acc (x : xs) = let d = acc </> x in d : go d xs
  in go root parts

-- The size of a file in bytes, or Nothing if it can't be stat'd.
fileSizeSafe :: FilePath -> IO (Maybe Integer)
fileSizeSafe p = do
  r <- try (getFileSize p) :: IO (Either SomeException Integer)
  pure (either (const Nothing) Just r)

canonicalizeSafe :: FilePath -> IO FilePath
canonicalizeSafe p = do
  r <- try (canonicalizePath p) :: IO (Either SomeException FilePath)
  pure (either (const p) id r)

------------------------------------------------------------------------------
-- Background workspace-search walker

-- | The workspace term search: a 'runScan' whose matcher is the user's query
-- (compiled once; an invalid regex yields no matches at all).
runWalker :: TQueue SearchMsg -> TVar Int -> SearchReq -> IO ()
runWalker q genRef req = runScan ScanSpec
  { scRoot       = sqRoot req
  , scInclude    = sqInclude req
  , scExclude    = sqExclude req
  , scSkip       = sqSkip req
  , scAlive      = (gen ==) <$> readTVarIO genRef
  , scMatcherFor = const (either (const Nothing) (Just . S.matcherLine)
                            (S.compileMatcher (sqCase req) (sqWord req) (sqRegex req) (sqTerm req)))
  , scEmit       = \fr -> atomically (writeTQueue q (SMFile gen fr))
  , scProgress   = \n -> atomically (writeTQueue q (SMProgress gen n))
  , scDone       = \n trunc -> do atomically (writeTQueue q (SMProgress gen n))
                                  atomically (writeTQueue q (SMDone gen trunc))
  }
  where gen = sqGen req

-- | A background go-to-definition scan: walk only the file formats we can
-- detect definitions in, matching each line against the language's definition
-- shapes for the requested identifier.
runDefWalker :: TQueue SearchMsg -> TVar Int -> DefReq -> IO ()
runDefWalker q genRef req = runScan ScanSpec
  { scRoot       = dfRoot req
  , scInclude    = D.defExtensionGlobs
  , scExclude    = []
  , scSkip       = dfSkip req
  , scAlive      = (gen ==) <$> readTVarIO genRef
  , scMatcherFor = \path -> (\lg -> D.defLineCols lg (dfName req)) <$> D.langOf path
  , scEmit       = \fr -> atomically (writeTQueue q (SMDefFile gen fr))
  , scProgress   = \_ -> pure ()
  , scDone       = \_ _ -> atomically (writeTQueue q (SMDefDone gen))
  }
  where gen = dfGen req

-- | The quick-open walk: enumerate every regular file under the root (the
-- same pruning as the search walker — dot/heavy dirs, symlinks — but no file
-- reads at all), streaming workspace-relative paths in batches so the picker
-- fills as the walk runs. Bails when a newer walk supersedes it, and stops at
-- 'Q.maxQuickFiles'.
runQuickWalker :: TQueue SearchMsg -> TVar Int -> Int -> FilePath -> IO ()
runQuickWalker q genRef gen root = do
  batchRef <- newIORef ([] :: [FilePath], 0 :: Int)
  countRef <- newIORef (0 :: Int)
  let alive = (gen ==) <$> readTVarIO genRef
      flush = do
        (b, _) <- atomicModifyIORef' batchRef (\s -> (([], 0), s))
        unless (null b) $ do
          ok <- alive
          when ok $ atomically (writeTQueue q (SMQuickFiles gen (reverse b)))
      push rel = do
        modifyIORef' countRef (+ 1)
        n <- atomicModifyIORef' batchRef (\(b, k) -> ((rel : b, k + 1), k + 1))
        when (n >= 400) flush
      walk dir = do
        ok <- alive
        cnt <- readIORef countRef
        when (ok && cnt < Q.maxQuickFiles) $ do
          names <- listDirNames dir
          forM_ names $ \name -> do
            keep <- alive
            c <- readIORef countRef
            when (keep && c < Q.maxQuickFiles) $ do
              let path = dir </> name
              mst <- statEntry path
              case mst of
                Nothing              -> pure ()
                Just EntryOther      -> pure ()   -- symlinks and specials: skip
                Just EntryDir        -> unless (S.dirPruned [] name) (walk path)
                Just (EntryFile _)   -> push (makeRelative root path)
  walk root
  flush
  ok <- alive
  when ok $ atomically (writeTQueue q (SMQuickDone gen))

-- | What a tree scan needs: where to walk, what to skip, how to match a file's
-- lines (per path, so the definition scan can pick a matcher by language), and
-- where results go. Emission callbacks are only invoked while 'scAlive'.
data ScanSpec = ScanSpec
  { scRoot       :: FilePath
  , scInclude    :: [String]
  , scExclude    :: [String]
  , scSkip       :: [FilePath]
  , scAlive      :: IO Bool
  , scMatcherFor :: FilePath -> Maybe (T.Text -> [(Int, Int)])
  , scEmit       :: FileResult -> IO ()
  , scProgress   :: Int -> IO ()
  , scDone       :: Int -> Bool -> IO ()   -- ^ files scanned, hit a cap.
  }

-- | Recursively scan the tree under the root, grep matching text files, and
-- stream results back. It stays cheap on huge trees: it prunes
-- hidden/heavy/excluded directories, skips symlinks (no loops), skips
-- over-large files and binaries (by extension without opening them, otherwise
-- by NUL-sniffing the first block before reading the rest), throttles progress
-- updates, stops accumulating once the global match cap is reached, and bails
-- the moment a newer scan supersedes it ('scAlive' turns False). The walk
-- itself is one thread that feeds candidate paths through a bounded queue to a
-- small pool of grep workers (at most 4, one per capability) so the
-- read+decode+match cost runs in parallel without saturating the box.
runScan :: ScanSpec -> IO ()
runScan spec = do
  scannedRef <- newIORef (0 :: Int)
  capRef     <- newIORef (0 :: Int, 0 :: Int)   -- (matches, files with matches)
  truncRef   <- newIORef False
  pathQ      <- newTBQueueIO 512                -- bounds the walker's lead
  doneVar    <- newTVarIO (0 :: Int)
  nWorkers   <- max 1 . min 4 <$> getNumCapabilities
  let alive = scAlive spec

      bumpScanned = do
        n <- atomicModifyIORef' scannedRef (\c -> (c + 1, c + 1))
        when (n `mod` 128 == 0) $ do
          ok <- alive
          when ok $ scProgress spec n

      -- Read a candidate, bailing before the bulk read when the first block
      -- says binary — a media/object-heavy tree costs one small read per file
      -- instead of a full slurp of every blob under the size cap.
      readTextFile path =
        try (withBinaryFile path ReadMode $ \h -> do
               hdr <- BS.hGet h 8192
               if looksBinary hdr
                 then pure Nothing
                 else Just . (hdr <>) <$> BS.hGetContents h)
          :: IO (Either SomeException (Maybe BS.ByteString))

      grepFile path = do
        ebs <- readTextFile path
        case ebs of
          Right (Just bs) | Just matcher <- scMatcherFor spec path -> do
            let lr  = loadFromBytes False Nothing bs
                txt = bufferToText Cmedit.TextBuffer.LF False (lrBuffer lr)
                (ms, ftrunc, cnt) = S.fileMatchesWith matcher txt
            unless (null ms) $ do
              -- Reserve room under the caps atomically (workers race for it).
              -- Stop once the match cap OR the result-file cap is reached, so
              -- both a match-dense and a spread-thin (1-per-file) broad search
              -- terminate promptly instead of scanning the whole tree.
              -- Forcing cnt runs the whole scan HERE, on this worker — never
              -- lazily on the UI thread when the result is rendered.
              won <- cnt `seq` atomicModifyIORef' capRef $ \(total, files) ->
                       if total >= S.maxTotalMatches || files >= S.maxResultFiles
                         then ((total, files), False)
                         else ((total + cnt, files + 1), True)
              if won
                then do
                  ok <- alive
                  when ok $ scEmit spec (FileResult path ms False ftrunc)
                else atomicWriteIORef truncRef True
          _ -> pure ()   -- unreadable, binary, or no matcher for this file

      -- A grep worker: drain candidate paths until the end-of-walk sentinel.
      -- Once superseded or capped it keeps draining (cheaply) so the walker
      -- never blocks forever on a full queue.
      worker = do
        next <- atomically (readTBQueue pathQ)
        case next of
          Nothing -> atomically (modifyTVar' doneVar (+ 1))
          Just path -> do
            ok     <- alive
            capped <- readIORef truncRef
            when (ok && not capped) $
              -- A worker must survive any per-file surprise, or the walker's
              -- end-of-search wait on the pool would never finish.
              void (try (bumpScanned >> grepFile path)
                      :: IO (Either SomeException ()))
            worker

      walk dir = do
        go <- alive
        capped <- readIORef truncRef
        when (go && not capped) $ do
          names <- listDirNames dir
          forM_ names $ \name -> do
            keepGoing <- alive
            stillOK   <- not <$> readIORef truncRef
            when (keepGoing && stillOK) $ do
              let path = dir </> name
              mst <- statEntry path
              case mst of
                Nothing         -> pure ()
                Just EntryOther -> pure ()                             -- skip symlinks (avoid loops)
                Just EntryDir   -> unless (S.dirPruned (scExclude spec) name) (walk path)
                Just (EntryFile sz) -> do
                      let rel = makeRelative (scRoot spec) path
                      when (sz <= S.maxFileBytesToSearch
                            && not (S.binaryExtension name)
                            && S.pathIncluded (scInclude spec) (scExclude spec) rel
                            && path `notElem` scSkip spec) $
                        atomically (writeTBQueue pathQ (Just path))

  forM_ [1 .. nWorkers] $ \_ -> forkIO worker
  walk (scRoot spec)
  forM_ [1 .. nWorkers] $ \_ -> atomically (writeTBQueue pathQ Nothing)
  atomically (readTVar doneVar >>= \d -> check (d == nWorkers))
  ok <- alive
  when ok $ do
    n     <- readIORef scannedRef
    trunc <- readIORef truncRef
    scDone spec n trunc

-- Directory entry names, sorted; unreadable directories yield nothing.
listDirNames :: FilePath -> IO [FilePath]
listDirNames dir = do
  r <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  pure (either (const []) sort r)

------------------------------------------------------------------------------
-- Rendering

renderNow :: Drv -> IORef Editor -> IORef (Maybe Screen) -> IORef String -> IO ()
renderNow drv editorRef prevRef titleRef = do
  ed0 <- readIORef editorRef
  prev <- readIORef prevRef
  caps <- readIORef (drvCaps drv)
  -- Refresh the highlight-state cache and keep the refreshed editor, so the
  -- lexer states computed for this frame carry over to the next one.
  let ed = refreshHighlight ed0
      scr = renderEditor ed
  writeIORef editorRef ed
  -- Theme-matched cursor colour (OSC 12), re-emitted only on theme changes.
  lastTheme <- readIORef (drvTheme drv)
  let theme = resolvedTheme ed
      cursorColorPart
        | lastTheme == Just theme = mempty
        | otherwise               = setCursorColor (themeCursorColor theme)
  writeIORef (drvTheme drv) (Just theme)
  -- Update the window title only when it changes.
  lastTitle <- readIORef titleRef
  let title = windowTitle ed
      titlePart = if title == lastTitle then mempty else setTitle title
  when (title /= lastTitle) (writeIORef titleRef title)
  -- Pixel-image overlay (kitty graphics / sixel), when the terminal supports
  -- one and the image view is unobstructed; re-placed only when its identity
  -- changes or a full redraw invalidated the terminal's copy.
  overlay <- gfxOverlay drv caps ed (isNothing prev)
  let cursorFix = case overlay of
        Nothing -> mempty
        -- Graphics emission moves the terminal cursor; restore the frame's.
        Just _  -> case scrCursor scr of
                     Just (r, c) -> moveTo r c <> showCursor
                     Nothing     -> hideCursor
  -- The whole frame — diff, title, cursor colour, graphics — goes out inside
  -- one synchronized-output block so the terminal commits it atomically.
  emit (beginSync
          <> cursorColorPart
          <> renderFrame (renderCapsOf caps) prev scr
          <> titlePart
          <> fromMaybe mempty overlay
          <> cursorFix
          <> endSync)
  hFlush stdout
  writeIORef prevRef (Just scr)

-- A visible cursor for a dark theme, a dark one for light.
themeCursorColor :: ThemeName -> (Word8, Word8, Word8)
themeCursorColor ThemeLight = (0x20, 0x20, 0x20)
themeCursorColor _          = (0xE8, 0xE8, 0xE8)

-- | The pixel-graphics overlay for this frame, if any output is needed:
-- 'Just' a builder that places (or deletes) the image, 'Nothing' when the
-- terminal state already matches. The cell-grid picture stays underneath as
-- the universal fallback, so this is purely an upgrade.
gfxOverlay :: Drv -> TermCaps -> Editor -> Bool -> IO (Maybe Builder)
gfxOverlay drv caps ed fullRedraw = do
  prevKey <- readIORef (drvGfx drv)
  let want = wantGfx caps ed
  case (prevKey, want) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> do
      -- The picture left the screen (doc switch, overlay opened, zoom drag):
      -- delete any kitty placement; a sixel one is simply painted over by
      -- the cell fallback as the diff repaints those cells.
      writeIORef (drvGfx drv) Nothing
      pure (Just kittyGfxDeleteAll)
    (mprev, Just key)
      | mprev == Just key && not fullRedraw -> pure Nothing
      | otherwise -> do
          writeIORef (drvGfx drv) (Just key)
          pure (Just (placeGfx ed key))

-- Is a pixel placement wanted right now, and under what identity?
wantGfx :: TermCaps -> Editor -> Maybe GfxKey
wantGfx caps ed = do
  idoc <- edImage ed
  kind <- if tcKittyGfx caps then Just GfxKitty
          else if tcSixel caps then Just GfxSixel
          else Nothing
  -- Only when the image view is the unobstructed content: any modal, menu,
  -- panel focus or search view falls back to the cell picture beneath.
  let lo = computeLayout ed
      unobstructed = edFocus ed == FEdit
                       && not (edSearchMode ed)
                       && isNothing (edDialog ed)
                       && isNothing (edLoading ed)
                       && isNothing (idDrag idoc)   -- the drag border is cell-drawn
  if not unobstructed || loTextWidth lo <= 0 || loTextHeight lo <= 0
    then Nothing
    else Just GfxKey
      { gkPath = edPath ed
      , gkCrop = imageCrop idoc
      , gkGeom = (loTextTop lo, loTextLeft lo, loTextWidth lo, loTextHeight lo)
      , gkPx   = cellPxKey ed
      , gkKind = kind
      }

-- Scale the cropped source to the fitted pixel size and emit the placement.
placeGfx :: Editor -> GfxKey -> Builder
placeGfx ed key = fromMaybe mempty $ do
  idoc <- edImage ed
  let img = idImage idoc
      (cx, cy, cw, ch) = gkCrop key
      cellPx = case gkPx key of (0, 0) -> (8, 16); p -> p  -- unknown: assume 2:1
      (top, left, tw, th) = gkGeom key
      (row, col, cols, rows, pxW, pxH) = gfxFit cellPx (top, left, tw, th) (cw, ch)
      rgba = scaleRGBA img (cx, cy, cw, ch) pxW pxH
  pure $ case gkKind key of
    GfxKitty -> kittyPlace (row, col) (cols, rows) (pxW, pxH) rgba
    GfxSixel -> sixelPlace (row, col) (pxW, pxH) rgba
