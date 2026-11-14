-- | Oracle driver for plan 0034 workstream D: render SVGs through
-- Cmedit.Svg + Cmedit.Raster and write binary PAM (RGBA) files for
-- svgcheck.sh to compare against resvg / rsvg-convert.
--
-- Reads job lines from stdin, "<svg path> <width> <height> <out.pam>", and
-- prints one result line per job:
--
--   OK <path> <parse ms> <render ms> <notes|->
--   LEFT <path> <message>
--   EXC <path> <exception>
--
-- Build (from the repository root):
--   ghc -O2 -isrc -outputdir /tmp/svgcheck-o docs/plans/bench/SvgCheck.hs -o /tmp/svgcheck
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, filterM, forM)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Array.Unboxed (elems)
import Data.List (intercalate)
import GHC.Clock (getMonotonicTime)
import System.IO

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.Raster (renderScene, defaultRenderOpts)
import Cmedit.Scene
import Cmedit.Svg
import Cmedit.Font (Font, parseFont, glyphPath, glyphAdvance, glyphForChar, glyphKerning, fontMatrix, fontAscent, fontDescent)
import Cmedit.FontSys (FontRequest(..), fontRequest, substituteFor)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  jobs <- map words . lines <$> getContents
  forM_ jobs $ \j -> case j of
    [path, ws, hs, out] -> run path (read ws) (read hs) out
    _ -> pure ()

run :: FilePath -> Int -> Int -> FilePath -> IO ()
run path w h out = do
  bs <- BS.readFile path
  t0 <- getMonotonicTime
  r <- try (evaluate (forceScene (parseSvg env bs)))
  t1 <- getMonotonicTime
  case r of
    Left (e :: SomeException) -> putStrLn ("EXC " ++ path ++ " " ++ oneLine (show e))
    Right (Left msg) -> putStrLn ("LEFT " ++ path ++ " " ++ oneLine msg)
    Right (Right sc) -> do
      let Rect x0 y0 x1 y1 = scBounds sc
          -- resvg first rounds the document size to whole pixels, then
          -- scales that uniformly to the requested width; match it rather
          -- than stretching the scene onto the rounded pixel box.
          sx = fromIntegral w / fromIntegral (max 1 (round (x1 - x0) :: Int))
          sy = sx
          _ = h
          view = Affine sx 0 0 sy (-x0 * sx) (-y0 * sy)
      ri <- try (evaluate (let (img, _) = renderScene (defaultRenderOpts w h view) sc
                           in sum (elems (imgPix img)) `seq` img))
      t2 <- getMonotonicTime
      case ri of
        Left (e :: SomeException) -> putStrLn ("EXC " ++ path ++ " render: " ++ oneLine (show e))
        Right img -> do
          writePam out img
          putStrLn (intercalate " " [ "OK", path, ms (t1 - t0), ms (t2 - t1)
                                    , if null (scNotes sc) then "-" else oneLine (intercalate "; " (scNotes sc)) ])
  where
    ms t = show (round (t * 1000) :: Int)
    oneLine = map (\c -> if c == '\n' then ' ' else c)
    forceScene e = case e of
      Left m -> length m `seq` e
      Right sc -> length (show (scCmds sc)) `seq` e

env :: SvgEnv
env = defaultSvgEnv { seFontSize = 12, seFonts = benchFonts }   -- resvg's default font size

------------------------------------------------------------------------------
-- A font provider for the bench: Cmedit.FontSys picks a system file for the
-- first family of the query, Cmedit.Font reads it. (The editor's real
-- provider is the integration step's; this only has to be good enough to
-- compare text against resvg, which matches families its own way.)

{-# NOINLINE fontFiles #-}
fontFiles :: [FilePath]
fontFiles = unsafePerformIO (concat <$> mapM walk ["/usr/share/fonts", "/usr/local/share/fonts"])
  where
    walk d = do
      ok <- doesDirectoryExist d
      if not ok then pure [] else do
        es <- listDirectory d
        concat <$> forM es (\e -> do
          let p = d </> e
          isD <- doesDirectoryExist p
          if isD then walk p else pure [p | any (`T.isSuffixOf` T.toLower (T.pack p)) [".ttf", ".otf", ".pfb", ".ttc"]])

{-# NOINLINE fontCache #-}
fontCache :: IORef (M.Map ([T.Text], Int, Bool) (Maybe Face))
fontCache = unsafePerformIO (newIORef M.empty)

benchFonts :: FontQuery -> Maybe Face
benchFonts q = unsafePerformIO $ do
  let key = (fqFamilies q, fqWeight q, fqStyle q /= FontNormal)
  m <- readIORef fontCache
  case M.lookup key m of
    Just r -> pure r
    Nothing -> do
      let fam = case fqFamilies q of (f : _) -> f; [] -> "serif"
          req = (fontRequest fam) { frBold = fqWeight q >= 600, frItalic = fqStyle q /= FontNormal }
          cands = take 3 (substituteFor fontFiles req)
      r <- firstFont cands
      let face = fmap (\(i, f) -> mkFace (M.size m + 1 + i) f) r
      modifyIORef' fontCache (M.insert key face)
      pure face
  where
    firstFont [] = pure Nothing
    firstFont (p : ps) = do
      bs <- BS.readFile p
      case parseFont bs of
        Right f -> pure (Just (0, f))
        Left _ -> firstFont ps

mkFace :: Int -> Font -> Face
mkFace k f =
  let Affine a _ _ _ _ _ = fontMatrix f
      upem = if a > 0 then 1 / a else 1000
  in Face { faceKey = k, faceUnitsPerEm = upem
          , faceAscent = fontAscent f, faceDescent = fontDescent f
          , faceGlyph = \c -> glyphForChar f c >>= \g -> Just (g, glyphPath f g, glyphAdvance f g)
          , faceKern = glyphKerning f }

writePam :: FilePath -> Image -> IO ()
writePam f img = do
  let hdr = BC.pack ("P7\nWIDTH " ++ show (imgW img) ++ "\nHEIGHT " ++ show (imgH img)
                     ++ "\nDEPTH 4\nMAXVAL 255\nTUPLTYPE RGB_ALPHA\nENDHDR\n")
  BS.writeFile f (BS.append hdr (BS.pack (elems (imgPix img))))
