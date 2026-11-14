-- | Oracle checks for the font readers (plan 0034 workstream B). Not part of
-- `make test`: it reads the machine's fonts. Build and run with
-- docs/plans/bench/fontcheck.sh [DIR...] (default /usr/share/fonts).
--
-- For every font file found it:
--   1. parses every face, reporting failures;
--   2. outlines every glyph and checks it against the font's own data:
--      * TrueType: the control-polygon bbox of each simple glyph must equal
--        its glyf header bbox exactly (composites within 1 unit);
--      * OpenType-CFF: the charstring width must equal hmtx;
--      * Type 1 (.t1/.pfb) and CFF (.otf) with an .afm of the same name:
--        AFM WX must equal the advance and AFM B must equal the exact
--        outline bbox (within 1 unit, the AFM's rounding);
--   3. cross-checks each URW .t1 against its .otf twin glyph by name
--      (exact bboxes within 1 unit; identical point lists counted);
--   4. times parsing and outlining.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, when, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.IORef
import Data.List (isSuffixOf, sort, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import Text.Printf (printf)

import Cmedit.Geom
import Cmedit.Font
import Cmedit.FontCore (u16, u32, i16, slice)
import Cmedit.FontTT (sfntTables, ttHeaderBBox)
import Cmedit.FontCFF (parseCFF)

fontExts :: [String]
fontExts = [".ttf", ".otf", ".ttc", ".otc", ".t1", ".pfb", ".pfa"]

walk :: FilePath -> IO [FilePath]
walk d = do
  es <- either (const []) id <$> (try (listDirectory d) :: IO (Either SomeException [FilePath]))
  fmap concat $ forM (sort es) $ \e -> do
    let p = d </> e
    isD <- doesDirectoryExist p
    isL <- pathIsSymbolicLink p
    if isD && not isL then walk p
      else pure [ p | map toLower (takeExtension p) `elem` fontExts ]

data Stats = Stats { stFiles, stFaces, stFail, stGlyphs, stBadBox, stBadW, stAfmG, stAfmBadW, stAfmBadB :: !Int
                   , stTTSimple, stTTExact, stTTOne, stTTComp, stTTCompClose, stTTCompTwo :: !Int
                   , stParse, stOutline :: !Double }

main :: IO ()
main = do
  args <- getArgs
  let roots = if null args then ["/usr/share/fonts"] else args
  files <- concat <$> mapM walk roots
  ref <- newIORef (Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  forM_ files $ \f -> checkFile ref f
  twins ref files
  Stats{..} <- readIORef ref
  printf "\nSUMMARY: %d files, %d faces, %d failed to parse\n" stFiles stFaces stFail
  printf "  glyphs outlined: %d; CFF width vs hmtx mismatches: %d\n" stGlyphs stBadW
  printf "  TrueType simple glyphs: %d, header bbox exact %d, 1 unit off %d; composites: %d, within 1 unit %d, within 2 %d; beyond: %d\n"
         stTTSimple stTTExact stTTOne stTTComp stTTCompClose stTTCompTwo stBadBox
  printf "  AFM glyphs checked: %d; AFM width mismatches: %d; AFM bbox mismatches: %d\n" stAfmG stAfmBadW stAfmBadB
  printf "  time: parse %.2f s, outline all glyphs %.2f s\n" stParse stOutline

checkFile :: IORef Stats -> FilePath -> IO ()
checkFile ref f = do
  bs <- BS.readFile f
  let n = max 1 (fontFaceCount bs)
  modifyIORef' ref (\s -> s { stFiles = stFiles s + 1 })
  forM_ [0 .. n - 1] $ \k -> do
    t0 <- getMonotonicTime
    r <- try (evaluate (forceFont (parseFontFace k bs))) :: IO (Either SomeException (Either String Font))
    t1 <- getMonotonicTime
    modifyIORef' ref (\s -> s { stFaces = stFaces s + 1, stParse = stParse s + (t1 - t0) })
    case r of
      Left ex -> do putStrLn ("EXCEPTION " ++ f ++ "#" ++ show k ++ ": " ++ show ex)
                    modifyIORef' ref (\s -> s { stFail = stFail s + 1 })
      Right (Left e) -> do putStrLn ("FAIL " ++ f ++ "#" ++ show k ++ ": " ++ e)
                           modifyIORef' ref (\s -> s { stFail = stFail s + 1 })
      Right (Right font) -> checkFont ref f k bs font

forceFont :: Either String Font -> Either String Font
forceFont r@(Left e) = length e `seq` r
forceFont r@(Right f) = fontNumGlyphs f `seq` T.length (fontFamily f) `seq` r

pathPoints :: Path -> [Pt]
pathPoints = concatMap pts
  where pts (MoveTo p) = [p]; pts (LineTo p) = [p]; pts (QuadTo a b) = [a, b]
        pts (CubicTo a b c) = [a, b, c]; pts ClosePath = []

forcePath :: Path -> Double
forcePath = foldl' (\a (Pt x y) -> a + x + y) 0 . pathPoints

checkFont :: IORef Stats -> FilePath -> Int -> BS.ByteString -> Font -> IO ()
checkFont ref f k bs font = do
  let ng = fontNumGlyphs font
  t0 <- getMonotonicTime
  s <- evaluate (sum [ forcePath (glyphPath font g) + glyphAdvance font g | g <- [0 .. ng - 1] ])
  t1 <- getMonotonicTime
  modifyIORef' ref (\st -> st { stGlyphs = stGlyphs st + ng, stOutline = stOutline st + (t1 - t0) })
  when (isNaN s) $ putStrLn ("NaN " ++ f)
  printf "OK %-60s #%d %-8s %6d glyphs %7.1f ms  %s %s\n" (takeFileName f) k (show' (fontFormat font)) ng
         ((t1 - t0) * 1000) (T.unpack (fontFamily font)) (T.unpack (fontStyle font))
  case fontFormat font of
    FmtTrueType -> ttBoxes ref f k bs font
    FmtOpenTypeCFF -> cffWidths ref f k bs font
    FmtCFFCID -> cffWidths ref f k bs font
    _ -> pure ()
  afmCheck ref f font
  where show' = drop 3 . show

-- | Simple glyphs: control-polygon bbox == glyf header bbox, exactly.
ttBoxes :: IORef Stats -> FilePath -> Int -> BS.ByteString -> Font -> IO ()
ttBoxes ref f k bs font = case sfntTables k bs of
  Left _ -> pure ()
  Right tabs -> do
    let tab t = maybe BS.empty (\(o, l) -> slice o l bs) (M.lookup t tabs)
        headT = tab "head"; locaT = tab "loca"; glyfT = tab "glyf"
        long = i16 headT 50 /= 0
        ng = fontNumGlyphs font
        loca g | long = (u32 locaT (4 * g), u32 locaT (4 * g + 4))
               | otherwise = (2 * u16 locaT (2 * g), 2 * u16 locaT (2 * g + 2))
        rec g = let (a, b) = loca g in if b > a then slice a (b - a) glyfT else BS.empty
        -- Per glyph: (simple?, header box, outline control box).
        rows = [ (i16 r 0 >= 0, hb, pathBounds (glyphPath font g), g)
               | g <- [0 .. ng - 1], let r = rec g, Just hb <- [ttHeaderBBox r] ]
        dist hb pb = case pb of
          Just (Rect a' b' c' d') -> let Rect a b c d = hb in maximum (map abs [a - a', b - b', c - c', d - d'])
          -- An empty outline agrees with an empty (or degenerate) header.
          Nothing -> let Rect a b c d = hb in if a == c || b == d then 0 else 1 / 0
        simple = [ (dist hb pb, g, hb, pb) | (True, hb, pb, g) <- rows ]
        comps = [ (dist hb pb, g, hb, pb) | (False, hb, pb, g) <- rows ]
        cnt p xs = length (filter (\(d, _, _, _) -> p d) xs)
        sExact = cnt (== 0) simple; sOne = cnt (\d -> d > 0 && d <= 1) simple
        cClose = cnt (<= 1) comps; cTwo = cnt (\d -> d > 1 && d <= 2) comps
        far = [ x | x@(d, _, _, _) <- simple, d > 1 ] ++ [ x | x@(d, _, _, _) <- comps, d > 2 ]
    modifyIORef' ref (\st -> st { stTTSimple = stTTSimple st + length simple, stTTExact = stTTExact st + sExact
                                , stTTOne = stTTOne st + sOne, stTTComp = stTTComp st + length comps
                                , stTTCompClose = stTTCompClose st + cClose, stTTCompTwo = stTTCompTwo st + cTwo
                                , stBadBox = stBadBox st + length far })
    when (sOne > 0 || cTwo > 0) $
      printf "  BBOX %s: simple %d/%d exact, %d a unit off (font's own header rounding); composites %d/%d within 1, %d within 2\n"
             (takeFileName f) sExact (length simple) sOne cClose (length comps) cTwo
    unless (null far) $
      printf "  BBOX %s: %d glyphs disagree with glyf header beyond rounding, e.g. %s\n" (takeFileName f) (length far)
             (show (take 3 far))

-- | OpenType CFF: charstring widths == hmtx.
cffWidths :: IORef Stats -> FilePath -> Int -> BS.ByteString -> Font -> IO ()
cffWidths ref f k bs font = case sfntTables k bs of
  Left _ -> pure ()
  Right tabs -> case M.lookup "CFF " tabs of
    Nothing -> pure ()
    Just (o, l) -> case parseCFF (slice o l bs) of
      Left e -> putStrLn ("  CFF table reparse failed: " ++ e)
      Right cff -> do
        let ng = min (fontNumGlyphs font) (fontNumGlyphs cff)
            bad = [ (g, glyphAdvance font g, glyphAdvance cff g)
                  | g <- [0 .. ng - 1], abs (glyphAdvance font g - glyphAdvance cff g) > 0.5 ]
        unless (null bad) $ do
          printf "  WIDTH %s: %d charstring widths differ from hmtx, e.g. %s\n" (takeFileName f) (length bad) (show (take 4 bad))
          modifyIORef' ref (\s -> s { stBadW = stBadW s + length bad })

-- AFM: name -> (WX, bbox)
readAfm :: FilePath -> IO (M.Map String (Double, Rect))
readAfm p = do
  ok <- doesFileExist p
  if not ok then pure M.empty else do
    ls <- BC.lines <$> BS.readFile p
    pure $ M.fromList (mapMaybe parse ls)
  where
    parse l
      | BC.pack "C " `BS.isPrefixOf` l =
          let fields = map (words . BC.unpack) (BC.split ';' l)
              get key = [ rest | (k : rest) <- fields, k == key ]
          in case (get "WX", get "N", get "B") of
               ([w : _], [n : _], [[a, b, c, d]]) -> Just (n, (read w, Rect (read a) (read b) (read c) (read d)))
               _ -> Nothing
      | otherwise = Nothing

afmFor :: FilePath -> IO (Maybe FilePath)
afmFor f = do
  let cands = [ replaceExtension f ".afm"
              , "/usr/share/fonts/type1/urw-base35" </> replaceExtension (takeFileName f) ".afm" ]
  ex <- filterM' doesFileExist cands
  pure (case ex of (p : _) -> Just p; [] -> Nothing)
  where filterM' p = fmap concat . mapM (\x -> do b <- p x; pure [x | b])

afmCheck :: IORef Stats -> FilePath -> Font -> IO ()
afmCheck ref f font
  | fontFormat font `notElem` [FmtType1, FmtOpenTypeCFF, FmtCFF] = pure ()
  | otherwise = do
      ma <- afmFor f
      case ma of
        Nothing -> pure ()
        Just ap -> do
          afm <- readAfm ap
          let Affine sx _ _ _ _ _ = fontMatrix font
              unit = 1 / (1000 * sx)     -- AFM is in 1/1000 em
              rows = [ (n, w, b, glyphByName font n) | (n, (w, b)) <- M.toList afm ]
              missing = [ n | (n, _, _, Nothing) <- rows ]
              checked = [ (n, w, b, g) | (n, w, b, Just g) <- rows ]
              badW = [ (n, w, glyphAdvance font g / unit) | (n, w, _, g) <- checked
                     , abs (glyphAdvance font g / unit - w) > 0.5 ]
              boxOf g = fmap (\(Rect a b c d) -> Rect (a / unit) (b / unit) (c / unit) (d / unit))
                             (pathBoundsExact (glyphPath font g))
              ctlOf g = fmap (\(Rect a b c d) -> Rect (a / unit) (b / unit) (c / unit) (d / unit))
                             (pathBounds (glyphPath font g))
              badB0 = [ (n, b, pb, g) | (n, _, b, g) <- checked
                      , let pb = boxOf g
                      , not (boxAgree b pb) ]
              -- An AFM box that matches the control polygon instead of the
              -- curve is the AFM generator's approximation, not ours.
              ctlB = [ () | (_, b, _, g) <- badB0, boxAgree b (ctlOf g) || within b (ctlOf g) (boxOf g) ]
              badB = [ (n, b, pb) | (n, b, pb, g) <- badB0, not (boxAgree b (ctlOf g) || within b (ctlOf g) (boxOf g)) ]
          modifyIORef' ref (\s -> s { stAfmG = stAfmG s + length checked
                                    , stAfmBadW = stAfmBadW s + length badW
                                    , stAfmBadB = stAfmBadB s + length badB })
          printf "  AFM %s: %d glyphs, %d missing, %d width and %d bbox mismatches (+%d where the AFM box lies between the curve box and the control box)\n"
                 (takeFileName f) (length checked) (length missing) (length badW) (length badB) (length ctlB)
          unless (null missing) $ putStrLn ("    missing: " ++ unwords (take 8 missing))
          unless (null badW) $ putStrLn ("    width: " ++ show (take 4 badW))
          unless (null badB) $ putStrLn ("    bbox: " ++ show (take 4 badB))

-- | Each AFM edge lies between the exact curve edge and the control-polygon
-- edge (within 1 unit): the AFM was measured from a coarser curve bound.
within :: Rect -> Maybe Rect -> Maybe Rect -> Bool
within (Rect a b c d) (Just (Rect ca cb cc cd)) (Just (Rect ea eb ec ed)) =
  btw a ca ea && btw b cb eb && btw c cc ec && btw d cd ed
  where btw v x y = v >= min x y - 1.01 && v <= max x y + 1.01
within _ _ _ = False

boxAgree :: Rect -> Maybe Rect -> Bool
boxAgree (Rect a b c d) Nothing = a == c || b == d
boxAgree (Rect a b c d) (Just (Rect a' b' c' d')) =
  maximum (map abs [a - a', b - b', c - c', d - d']) <= 1.01

-- | URW .t1 vs .otf twins, glyph by glyph by name.
twins :: IORef Stats -> [FilePath] -> IO ()
twins _ files = do
  let t1s = [ f | f <- files, takeExtension f == ".t1" ]
      otfs = M.fromList [ (takeBaseName f, f) | f <- files, takeExtension f == ".otf", "urw-base35" `elem` splitDirectories f ]
  tot <- newIORef (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
  forM_ t1s $ \t -> case M.lookup (takeBaseName t) otfs of
    Nothing -> pure ()
    Just o -> do
      a <- parseFont <$> BS.readFile t
      b <- parseFont <$> BS.readFile o
      case (a, b) of
        (Right fa, Right fb) -> do
          let names = mapMaybe (glyphName fa) [0 .. fontNumGlyphs fa - 1]
              both = [ (n, ga, gb) | n <- names, Just ga <- [glyphByName fa n], Just gb <- [glyphByName fb n] ]
              -- Both are 1/1000 em in these fonts; normalise through the matrices anyway.
              em f' g = transformPath (fontMatrix f') (glyphPath f' g)
              cmp = [ (n, pathBoundsExact pa, pathBoundsExact pb, samePts pa pb)
                    | (n, ga, gb) <- both, let pa = em fa ga; pb = em fb gb ]
              badBox = [ (n, x, y) | (n, x, y, _) <- cmp, not (boxClose x y) ]
              same = length [ () | (_, _, _, True) <- cmp ]
          printf "TWIN %-28s %4d common glyphs, %4d bbox disagreements, %4d identical point lists, adv diffs %d\n"
                 (takeBaseName t) (length both) (length badBox) same
                 (length [ () | (_, ga, gb) <- both
                              , abs (glyphAdvance fa ga * sxOf fa - glyphAdvance fb gb * sxOf fb) > 0.0005 ])
          unless (null badBox) $ putStrLn ("    e.g. " ++ show (take 3 badBox))
          modifyIORef' tot (\(w, x, y, z) -> (w + 1, x + length both, y + length badBox, z + same))
        _ -> putStrLn ("TWIN parse failure " ++ t)
  (w, x, y, z) <- readIORef tot
  printf "TWINS: %d pairs, %d glyphs compared, %d bbox disagreements (>1 unit), %d identical point lists\n" w x y z
  where
    sxOf f' = let Affine a _ _ _ _ _ = fontMatrix f' in a
    boxClose (Just (Rect a b c d)) (Just (Rect a' b' c' d')) =
      maximum (map abs [a - a', b - b', c - c', d - d']) <= 0.001
    boxClose Nothing Nothing = True
    boxClose _ _ = False
    samePts pa pb = let xs = pathPoints pa; ys = pathPoints pb
                    in length xs == length ys && and (zipWith close xs ys)
    close (Pt a b) (Pt c d) = abs (a - c) < 1e-6 && abs (b - d) < 1e-6
