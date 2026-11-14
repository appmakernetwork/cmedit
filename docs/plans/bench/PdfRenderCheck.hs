-- | Oracle driver for plan 0034 workstream E: render PDF pages through
-- Cmedit.PdfRender + Cmedit.Raster and write binary PAM (RGBA) files for
-- pdfrendercheck.sh to compare against pdftoppm.
--
-- Reads tab-separated job lines from stdin, "<pdf path> <page (1-based)>
-- <dpi> <out.pam>", and prints one tab-separated result line per job:
--
--   OK <out.pam> <scene ms> <raster ms> <cmds> <notes|->
--   LEFT <out.pam> <message>
--   EXC <out.pam> <exception>
--
-- Fonts come from Cmedit.FontProvider, exactly as the editor's page view
-- will get them: the page's 'pageFontNeeds' are answered by 'fontFor'
-- before the (pure) render runs. The page's own scene time includes
-- nothing of that (fonts are cached across pages by the provider).
--
-- Build (from the repository root):
--   ghc -O2 -isrc -outputdir /tmp/pdfrendercheck-o docs/plans/bench/PdfRenderCheck.hs -o /tmp/pdfrendercheck
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Array.Unboxed (elems)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.IO

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.PdfObj
import Cmedit.PdfRender
import Cmedit.Raster (renderScene, defaultRenderOpts, RenderOpts(..), RenderStats(..))
import Cmedit.Scene
import Cmedit.FontProvider (newFontProvider, startFontDiscovery, providerFiles, fontFor)
import Cmedit.Font (Font)
import Cmedit.FontSys (FontRequest)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  fp <- newFontProvider
  startFontDiscovery fp (pure ())
  files <- maybe [] id <$> providerFiles 60000000 fp
  docs <- newIORef (M.empty :: M.Map FilePath (Either String (PdfFile, [Dict], Fuel)))
  jobs <- map (splitOn '\t') . lines <$> getContents
  let noFonts = "nofonts" `elem` args
  forM_ jobs $ \j -> case j of
    [path, pg, dpi, out] -> do
      m <- readIORef docs
      doc <- case M.lookup path m of
        Just d -> pure d
        Nothing -> do
          r <- try (do bs <- BS.readFile path
                       evaluate (case openPdfFile maxPdfDecoded bs of
                                   Left e -> Left e
                                   Right (f, left) -> let ps = pdfPages f
                                                      in length ps `seq` Right (f, ps, fuel0 left maxPdfTokens)))
          let d = either (\(e :: SomeException) -> Left (show e)) id r
          -- Keep one document at a time.
          writeIORef docs (M.singleton path d)
          pure d
      case doc of
        Left e -> putStrLn (tabs ["LEFT", out, oneLine e])
        Right (f, pages, fu) -> do
          let k = read pg - 1
          if k < 0 || k >= length pages then putStrLn (tabs ["LEFT", out, "no such page"]) else do
            let page = pages !! k
                (needs, _) = pageFontNeeds f fu page
            answered <- if noFonts then pure [] else forM needs (\rq -> do
                              r <- fontFor fp files rq
                              pure (rq, r))
            let env = defaultRenderEnv { reFonts = \rq -> maybe Nothing id (lookup rq answered) }
            t0 <- getMonotonicTime
            r <- try (evaluate (let (sc, fu') = renderPage env f fu page
                                in forceCmds (scCmds sc) `seq` length (scNotes sc) `seq` fu' `seq` sc))
            t1 <- getMonotonicTime
            case r of
              Left (e :: SomeException) -> putStrLn (tabs ["EXC", out, oneLine (show e)])
              Right sc -> do
                let Rect x0 y0 x1 y1 = scBounds sc
                    s = read dpi / 72 :: Double
                    -- pdftoppm's size: ceiling of the page size at the dpi.
                    w = max 1 (ceiling ((x1 - x0) * s - 0.001)) :: Int
                    h = max 1 (ceiling ((y1 - y0) * s - 0.001)) :: Int
                    view = Affine s 0 0 s (-x0 * s) (-y0 * s)
                    opts0 = (defaultRenderOpts w h view) { roBackground = white }
                    -- "mb=N" overrides the rasteriser's memory cap (MB).
                    opts = case [ read (drop 3 a) | a <- args, take 3 a == "mb=" ] of
                      (mb : _) -> opts0 { roMaxLayerBytes = mb * 1024 * 1024 }
                      [] -> opts0
                ri <- try (evaluate (let (img, st) = renderScene opts sc
                                     in sum (elems (imgPix img)) `seq` (img, st)))
                t2 <- getMonotonicTime
                case ri of
                  Left (e :: SomeException) -> putStrLn (tabs ["EXC", out, "render: " ++ oneLine (show e)])
                  Right (img, st) -> do
                    writePam out img
                    let notes = scNotes sc ++ [ "raster budget exhausted" | rsExhausted st ] ++ [ "raster skipped " ++ show (rsSkipped st) | rsSkipped st > 0 ]
                    putStrLn (tabs [ "OK", out, ms (t1 - t0), ms (t2 - t1), show (countCmds (scCmds sc))
                                              , if null notes then "-" else oneLine (intercalate "; " notes) ])
    _ -> pure ()
  where
    ms t = show (round (t * 1000) :: Int)
    tabs = intercalate "\t"
    splitOn ch str = case break (== ch) str of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn ch rest
    oneLine = map (\c -> if c == '\n' || c == '\t' then ' ' else c)

-- Force a scene's structure (paths, glyph outlines, paints) without
-- rendering images as text, which is what 'show' would cost.
forceCmds :: [Cmd] -> Double
forceCmds = foldr (\c acc -> one c + acc) 0
  where
    one c = case c of
      CFill m _ p path -> aff m + paint p + pathF path
      CStroke m _ p path -> aff m + paint p + pathF path
      CGlyphs m p gs -> aff m + paint p + sum [ aff ga + fromIntegral k + pathF gp | Glyph ga k gp <- gs ]
      CImage m img _ -> aff m + fromIntegral (imgW img) + (imgPix img `seq` 0)
      CStencil m mk p _ -> aff m + fromIntegral (mkW mk) + (mkPix mk `seq` 0) + paint p
      CGouraud m ts -> aff m + fromIntegral (length ts)
      CGouraudRamp m st ts -> aff m + fromIntegral (length st + length ts)
      CClip sh k -> sum [ aff m + pathF p | ClipShape m _ p <- sh ] + forceCmds k
      CGroup o k -> goOpacity o + maybe 0 (forceCmds . smCmds) (goMask o) + forceCmds k
      CTransform m k -> aff m + forceCmds k
    aff (Affine a b c d e f) = a + b + c + d + e + f
    paint p = case p of
      PTiling _ m _ _ _ k -> aff m + forceCmds k
      PSolid (Color r _ _ _) -> r
      PLinear m _ _ st _ -> aff m + fromIntegral (length st)
      PRadial m _ _ _ _ st _ -> aff m + fromIntegral (length st)
      PSampled m img _ -> aff m + (imgPix img `seq` 0)
    pathF = foldr (\sg acc -> seg sg + acc) 0
    seg sg = case sg of
      MoveTo (Pt x y) -> x + y
      LineTo (Pt x y) -> x + y
      QuadTo (Pt x y) _ -> x + y
      CubicTo _ _ (Pt x y) -> x + y
      ClosePath -> 0

countCmds :: [Cmd] -> Int
countCmds = sum . map one
  where
    one c = case c of
      CClip _ k -> 1 + countCmds k
      CGroup _ k -> 1 + countCmds k
      CTransform _ k -> 1 + countCmds k
      _ -> 1

writePam :: FilePath -> Image -> IO ()
writePam f img = do
  let hdr = BC.pack ("P7\nWIDTH " ++ show (imgW img) ++ "\nHEIGHT " ++ show (imgH img)
                     ++ "\nDEPTH 4\nMAXVAL 255\nTUPLTYPE RGB_ALPHA\nENDHDR\n")
  BS.writeFile f (BS.append hdr (BS.pack (elems (imgPix img))))
