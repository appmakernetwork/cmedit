-- Dump every image of a PDF the way Cmedit.PdfImage decodes it, for
-- pdfimagecheck.sh to compare against `pdfimages` (poppler).
--
--   PdfImageCheck FILE.pdf OUTDIR
--
-- For each image XObject drawn on a page (object number N) it writes
--   OUTDIR/N.img    the colour image (RGBA)
--   OUTDIR/N.smask  its soft mask decoded on its own (grey in R)
--   OUTDIR/N.mask   its explicit stencil mask (coverage in R: 255 = paints)
--   OUTDIR/N.stencil  an /ImageMask image's coverage
-- and for inline images, OUTDIR/inline-P-K.img / .stencil (page P, K-th
-- inline image on the page's own content stream, in drawing order).
-- Each file is "w h\n" then w*h*4 bytes. One line per image on stdout:
--   N kind ok|unsupported|corrupt  milliseconds  [message]
module Main where

import Control.Exception (evaluate, try, SomeException)
import Control.Monad (forM_, when)
import qualified Data.Array.Unboxed as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.IORef
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

import Cmedit.Image (Image(..))
import Cmedit.PdfImage
import Cmedit.PdfObj
import Cmedit.Scene (Mask(..))

main :: IO ()
main = do
  [path, out] <- getArgs
  bs <- BS.readFile path
  case parsePdfFile bs of
    Left e -> putStrLn ("FILE " ++ e)
    Right f -> do
      seen <- newIORef IS.empty
      forM_ (zip [1 :: Int ..] (pdfPages f)) $ \(pg, pd) -> do
        let res = dictOf f (M.findWithDefault ONull (BC.pack "Resources") pd)
        walkRes f seen out res (0 :: Int)
        -- Inline images on the page's own content.
        let (dat, _) = contentsOf f fuelDefault pd
            inl = [ (d, raw) | TInline d raw <- contentTokensInline dat ]
        forM_ (zip [0 :: Int ..] inl) $ \(k, (d, raw)) ->
          runL (info (expandInlineDict d)) ("inline-" ++ show pg ++ "-" ++ show k) out "img"
              (fst (decodeInlineImage f fuelDefault res d raw))
      -- Images reached some other way (annotation appearances, patterns,
      -- forms inheriting their page's resources): every Image object in the
      -- file, with no resources (named colour spaces then fail, and say so).
      forM_ (IM.keys (pfObjs f)) $ \r ->
        walkRes f seen out (M.singleton (BC.pack "XObject") (ODict (M.singleton (BC.pack "x") (ORef r)))) 7

-- What the comparison needs to know about an image: whether a stencil's
-- Decode inverts it (pdfimages writes stencils raw), and the colour space's
-- family chain (poppler converts CMYK alternates with its own formula).
info :: Dict -> String
info d = "inv=" ++ (case lookDRaw "Decode" of OArr (ONum a : ONum b : _) | a > b -> "1"; _ -> "0")
         ++ " cs=" ++ cs (lookDRaw "ColorSpace")
  where
    lookDRaw k = M.findWithDefault ONull (BC.pack k) d
    cs o = case o of
      OName n -> BC.unpack n
      OArr xs -> concatMap (\x -> case x of OName n -> BC.unpack n ++ ">"; OArr _ -> cs x; _ -> "") xs
      _ -> "?"

-- Every image in a resource dictionary, and in its forms' resources.
walkRes :: PdfFile -> IORef IS.IntSet -> FilePath -> Dict -> Int -> IO ()
walkRes f seen out res depth = when (depth < 8) $ do
  let xobjs = dictOf f (lookD f res "XObject")
  forM_ (M.elems xobjs) $ \ref -> case ref of
    ORef r -> do
      s <- readIORef seen
      if IS.member r s then pure () else do
        modifyIORef' seen (IS.insert r)
        case resolve f ref of
          OStream d raw
            | nameOf (lookD f d "Subtype") == BC.pack "Image" -> do
                -- The colour image without its masks: what pdfimages writes
                -- (it neither applies an alpha nor un-premultiplies a /Matte,
                -- and it keeps the image's own size).
                let bare = M.delete (BC.pack "SMask") (M.delete (BC.pack "Mask") d)
                runL (info d) (show r) out "img" (fst (decodePdfImage f fuelDefault res bare raw))
                case lookD f d "SMask" of
                  OStream sd sraw -> run (show r) out "smask" (fst (decodePdfImage f fuelDefault res sd sraw))
                  _ -> pure ()
                case lookD f d "Mask" of
                  OStream md mraw -> runL (info md) (show r) out "mask"
                    (fst (decodePdfImage f fuelDefault res (M.insert (BC.pack "ImageMask") (OBool True) md) mraw))
                  _ -> pure ()
            | nameOf (lookD f d "Subtype") == BC.pack "Form" ->
                walkRes f seen out (dictOf f (lookD f d "Resources")) (depth + 1)
          _ -> pure ()
    _ -> pure ()

run :: String -> FilePath -> String -> Either ImageFailure PdfImg -> IO ()
run = runL ""

runL :: String -> String -> FilePath -> String -> Either ImageFailure PdfImg -> IO ()
runL extra name out kind r0 = do
  t0 <- getMonotonicTime
  r <- try (evaluate (force r0)) :: IO (Either SomeException (Either ImageFailure PdfImg))
  t1 <- getMonotonicTime
  let ms = (t1 - t0) * 1000 :: Double
  case r of
    Left e -> printf "%s %s exception %.1f %s\n" name kind ms (show e)
    Right (Left (ImgUnsupported m)) -> printf "%s %s unsupported %.1f %s\n" name kind ms m
    Right (Left (ImgCorrupt m)) -> printf "%s %s corrupt %.1f %s\n" name kind ms m
    Right (Right img) -> do
      let (w, h, px, ext) = case img of
            PdfColorImage im _ _ -> (imgW im, imgH im, U.elems (imgPix im), if kind == "img" then "img" else kind)
            PdfStencil m _ -> (mkW m, mkH m, concatMap (\v -> [v, v, v, 255]) (U.elems (mkPix m)),
                               if kind == "img" then "stencil" else kind)
      BS.writeFile (out </> (name ++ "." ++ ext)) (BC.pack (show w ++ " " ++ show h ++ "\n") <> BS.pack px)
      printf "%s %s ok %.1f %dx%d %s\n" name ext ms w h extra
  where
    force x = case x of
      Right (PdfColorImage im _ _) -> U.bounds (imgPix im) `seq` (imgPix im U.! 0) `seq` x
      Right (PdfStencil m _) -> mkPix m `seq` x
      _ -> x
