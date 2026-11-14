-- | Tests for the vector stack (plan 0034): the PDF object layer
-- ("Cmedit.PdfObj"), functions ("Cmedit.PdfFunc"), colour spaces and images
-- ("Cmedit.PdfImage"), CCITT fax ("Cmedit.Ccitt") and CMYK/YCCK JPEGs
-- ("Cmedit.Image").
--
-- Spec.hs calls 'pdfImageSpec' with its own @check@, so a failure here is
-- counted and reported exactly like every other test. Fixtures are built
-- in code or checked in as bytes: the suite never shells out. The CCITT
-- fixtures are Ghostscript's encodings (faxg3, faxg32d, faxg4) of the
-- pattern 'faxPattern' computes; the JPEGs are a 16x16 four-quadrant CMYK
-- image written by Pillow (Adobe, transform 0, inverted) and by ImageMagick
-- (Adobe, transform 2: YCCK).
module PdfImageSpec (pdfImageSpec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Char (digitToInt)
import Data.List (foldl', isPrefixOf)
import qualified Data.Array.Unboxed as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Word (Word8)

import Cmedit.Ccitt
import Cmedit.Geom (Affine(..), Rect(..))
import Cmedit.Image (Image(..), JpegSamples(..), decodeJPEG, decodeJPEGSamples)
import qualified Cmedit.Pdf as Pdf
import Cmedit.PdfFunc
import Cmedit.PdfImage
import Cmedit.PdfObj
import Cmedit.Scene (Mask(..))

pdfImageSpec :: (String -> Bool -> IO ()) -> IO ()
pdfImageSpec check = do
  objSpec check
  funcSpec check
  colourSpec check
  ccittSpec check
  jpegSpec check
  imageSpec check
  fuzzSpec check

------------------------------------------------------------------------------
-- Fixture helpers

bs :: String -> BS.ByteString
bs = BC.pack

hexBS :: String -> BS.ByteString
hexBS = BS.pack . go
  where go (a : b : r) = fromIntegral (digitToInt a * 16 + digitToInt b) : go r
        go _ = []

-- A PDF of numbered objects (the scan needs no xref) whose catalog is 1.
pdfOf :: [(Int, BS.ByteString)] -> BS.ByteString
pdfOf objs = BS.concat $
  [bs "%PDF-1.7\n"]
  ++ [ BS.concat [bs (show n ++ " 0 obj\n"), body, bs "\nendobj\n"] | (n, body) <- objs ]
  ++ [bs "trailer\n<< /Root 1 0 R >>\n%%EOF\n"]

streamObj :: String -> BS.ByteString -> BS.ByteString
streamObj dict dat = BS.concat [bs ("<< " ++ dict ++ " /Length " ++ show (BS.length dat) ++ " >>\nstream\n"), dat, bs "\nendstream"]

-- A minimal one-page document, plus extra objects from 10 up.
docWith :: String -> [(Int, BS.ByteString)] -> BS.ByteString
docWith pageExtra extra = pdfOf $
  [ (1, bs "<< /Type /Catalog /Pages 2 0 R >>")
  , (2, bs "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
  , (3, bs ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 100] " ++ pageExtra ++ " >>")) ]
  ++ extra

fileOf :: BS.ByteString -> PdfFile
fileOf b = either (const (PdfFile IM.empty M.empty)) id (parsePdfFile b)

-- zlib with stored (uncompressed) DEFLATE blocks: FlateDecode input with no
-- compressor in the boot libraries.
zlibStored :: BS.ByteString -> BS.ByteString
zlibStored dat = BS.concat ([BS.pack [0x78, 0x01]] ++ blocks dat ++ [be32 (adler dat)])
  where
    blocks d
      | BS.length d <= 65535 = [block True d]
      | otherwise = block False (BS.take 65535 d) : blocks (BS.drop 65535 d)
    block final d =
      let n = BS.length d
      in BS.concat [BS.pack [if final then 1 else 0, fromIntegral n, fromIntegral (n `shiftR` 8),
                             fromIntegral (complementW n), fromIntegral (complementW n `shiftR` 8)], d]
    complementW n = 0xffff `xor` n
    adler d = let (a, b) = BS.foldl' (\(x, y) c -> let x' = (x + fromIntegral c) `mod` 65521 in (x', (y + x') `mod` 65521)) (1, 0) d
              in (b `shiftL` 16) .|. a :: Int
    be32 v = BS.pack [fromIntegral (v `shiftR` s) | s <- [24, 16, 8, 0]]

-- Pack rows of samples at a bit depth, each row padded to a byte.
packRows :: Int -> [[Int]] -> BS.ByteString
packRows bpc rows = BS.pack (concatMap row rows)
  where
    row ss
      | bpc == 16 = concatMap (\v -> [fromIntegral (v `shiftR` 8), fromIntegral v]) ss
      | otherwise = toBytes (concatMap (\v -> [ testBit v k | k <- [bpc - 1, bpc - 2 .. 0] ]) ss)
    toBytes [] = []
    toBytes bits = let (a, r) = splitAt 8 bits
                   in foldl' (\acc b -> acc * 2 + (if b then 1 else 0)) 0 (take 8 (a ++ repeat False)) : toBytes r

-- Decode the image in object 10 of a one-page document.
imageIn :: String -> BS.ByteString -> [(Int, BS.ByteString)] -> Either ImageFailure PdfImg
imageIn dict dat extra =
  let f = fileOf (docWith "" ((10, streamObj dict dat) : extra))
  in case IM.lookup 10 (pfObjs f) of
       Just (OStream d raw) -> fst (decodePdfImage f fuelDefault M.empty d raw)
       _ -> Left (ImgCorrupt "fixture did not parse")

rgbaOf :: Either ImageFailure PdfImg -> Maybe (Int, Int, [Int])
rgbaOf (Right (PdfColorImage im _ _)) = Just (imgW im, imgH im, map fromIntegral (U.elems (imgPix im)))
rgbaOf _ = Nothing

pixelAt :: (Int, Int, [Int]) -> Int -> Int -> [Int]
pixelAt (w, _, px) x y = take 4 (drop ((y * w + x) * 4) px)

near :: Int -> [Int] -> [Int] -> Bool
near tol a b = length a == length b && and (zipWith (\x y -> abs (x - y) <= tol) a b)

nearD :: Double -> Double -> Double -> Bool
nearD tol a b = abs (a - b) <= tol

nearL :: Double -> [Double] -> [Double] -> Bool
nearL tol a b = length a == length b && and (zipWith (nearD tol) a b)

-- Does evaluating this (deeply, via the function given) throw?
noThrow :: (a -> Int) -> a -> IO Bool
noThrow force x = do
  r <- try (evaluate (force x)) :: IO (Either SomeException Int)
  pure (either (const False) (const True) r)

sumArr :: Int -> U.UArray Int Word8 -> Int
sumArr = foldl' (\a v -> a + fromIntegral v) `flip2` U.elems
  where flip2 g h z arr = g z (h arr)

forceImg :: Either ImageFailure PdfImg -> Int
forceImg r = case r of
  Right (PdfColorImage im _ _) -> sumArr (imgW im) (imgPix im)
  Right (PdfStencil m _) -> sumArr (mkW m) (mkPix m)
  Left (ImgUnsupported s) -> length s
  Left (ImgCorrupt s) -> length s

------------------------------------------------------------------------------
-- The object layer

objSpec :: (String -> Bool -> IO ()) -> IO ()
objSpec check = do
  let doc = docWith "/CropBox [10 -5 300 90] /Rotate 450 /Contents 10 0 R"
              [(10, streamObj "" (bs "q 1 0 0 1 5 5 cm Q"))]
      f = fileOf doc
      pages = pdfPages f
  check "pdfobj: parsePdfFile finds the page tree" (length pages == 1)
  check "pdfobj: parsePdfFile refuses a non-PDF" (either (const True) (const False) (parsePdfFile (bs "hello")))
  check "pdfobj: an encrypted file is refused with the text view's message" $
    case parsePdfFile (pdfOf [(1, bs "<< /Type /Catalog >>")] <> bs "trailer << /Root 1 0 R /Encrypt 5 0 R >>") of
      Left e -> "encrypted" `isPrefixOf` e
      Right _ -> False
  case pages of
    (pd : _) -> do
      check "pdfobj: media box" (pageMediaBox f pd == Rect 0 0 200 100)
      check "pdfobj: crop box is clipped to the media box" (pageCropBox f pd == Rect 10 0 200 90)
      check "pdfobj: rotation is normalised to 0..270" (pageRotation f pd == 90)
      check "pdfobj: contents are decoded"
        (fst (contentsOf f fuelDefault pd) == bs "q 1 0 0 1 5 5 cm Q")
    [] -> check "pdfobj: page present" False
  let f2 = fileOf (docWith "/Rotate 45 /CropBox [500 500 600 600]" [])
  case pdfPages f2 of
    (pd : _) -> do
      check "pdfobj: a rotation that is not a multiple of 90 is ignored" (pageRotation f2 pd == 0)
      check "pdfobj: a crop box outside the media box falls back to it" (pageCropBox f2 pd == Rect 0 0 200 100)
    [] -> check "pdfobj: page present (2)" False
  check "pdfobj: matrixOf" (matrixOf f (OArr (map ONum [1, 2, 3, 4, 5, 6])) == Just (Affine 1 2 3 4 5 6))
  check "pdfobj: matrixOf wants six numbers" (matrixOf f (OArr (map ONum [1, 2, 3])) == Nothing)
  check "pdfobj: rectOf normalises corners" (rectOf f (OArr (map ONum [10, 20, 0, 5])) == Just (Rect 0 5 10 20))
  check "pdfobj: lookDAny reads abbreviated keys"
    (lookDAny f (M.fromList [(bs "BPC", ONum 4)]) ["BitsPerComponent", "BPC"] == ONum 4)
  check "pdfobj: the PdfDoc keeps its source pages" $ case Pdf.parsePdf doc of
    Right pd -> case Pdf.pdfPageSource pd 1 of
      Just (_, d) -> M.member (bs "MediaBox") d && Seq.length (Pdf.srcPages (Pdf.pdSource pd)) == 1
      Nothing -> False
    Left _ -> False
  check "pdfobj: pdfPageSource is Nothing past the last page" $ case Pdf.parsePdf doc of
    Right pd -> maybe True (const False) (Pdf.pdfPageSource pd 2)
    Left _ -> False

  -- Encodings and glyph names.
  check "pdfobj: StandardEncoding by name" (IM.lookup 65 standardEncodingNames == Just (bs "A")
                                           && IM.lookup 0xA4 standardEncodingNames == Just (bs "fraction"))
  check "pdfobj: WinAnsiEncoding by name" (IM.lookup 0x80 winAnsiEncodingNames == Just (bs "Euro")
                                          && IM.lookup 0xAD winAnsiEncodingNames == Just (bs "hyphen"))
  check "pdfobj: MacRomanEncoding by name" (IM.lookup 0xDB macRomanEncodingNames == Just (bs "currency"))
  check "pdfobj: MacExpertEncoding by name" (IM.lookup 0x30 macExpertEncodingNames == Just (bs "zerooldstyle"))
  check "pdfobj: Symbol encoding" (IM.lookup 0x61 symbolEncodingNames == Just (bs "alpha"))
  check "pdfobj: ZapfDingbats encoding" (IM.lookup 0x21 zapfDingbatsEncodingNames == Just (bs "a1"))
  check "pdfobj: encodingByName" (fmap IM.size (encodingByName (bs "WinAnsiEncoding")) == Just (IM.size winAnsiEncodingNames)
                                  && encodingByName (bs "Nonsense") == Nothing)
  check "pdfobj: differences over a name encoding" $
    let d = differences f (ODict (M.fromList [(bs "Differences", OArr [ONum 65, OName (bs "B"), OName (bs "C"), ONum 300, OName (bs "x")])])) standardEncodingNames
    in IM.lookup 65 d == Just (bs "B") && IM.lookup 66 d == Just (bs "C") && IM.notMember 300 d
  check "pdfobj: glyphUnicode" (glyphUnicode (bs "Euro") == Just 0x20ac && glyphUnicode (bs "uni0041") == Just 0x41
                                && glyphUnicode (bs "alpha") == Just 0x3b1 && glyphUnicode (bs "a.sc") == Just 0x61)
  r <- try (evaluate (glyphUnicode (bs "u110000"), glyphChar "uFFFFFFF")) :: IO (Either SomeException (Maybe Int, Maybe Char))
  check "pdfobj: an out-of-range uXXXXXX glyph name is Nothing, not an exception"
    (either (const False) (\(a, b) -> a == Nothing && b == Nothing) r)

  -- Streams.
  let chainFile = fileOf (docWith "" [(10, streamObj "/Filter [/ASCIIHexDecode /DCTDecode]" (bs "414243>"))])
  check "pdfobj: decodeStreamPrefix stops in front of a codec" $ case IM.lookup 10 (pfObjs chainFile) of
    Just (OStream d raw) -> case fst (decodeStreamPrefix chainFile fuelDefault d raw) of
      Right (b, [(c, _)]) -> b == bs "ABC" && c == bs "DCTDecode"
      _ -> False
    _ -> False
  check "pdfobj: canonicalFilter expands abbreviations"
    (map (canonicalFilter . bs) ["AHx", "A85", "LZW", "Fl", "RL", "CCF", "DCT", "Other"]
       == map bs ["ASCIIHexDecode", "ASCII85Decode", "LZWDecode", "FlateDecode", "RunLengthDecode", "CCITTFaxDecode", "DCTDecode", "Other"])
  let rawRows = [ [ (x * 7 + y * 13) `mod` 256 | x <- [0 .. 17] ] | y <- [0 .. 4 :: Int] ]
      subRows = BS.pack (concat [ 1 : [ fromIntegral ((v - (if i >= 3 then r !! (i - 3) else 0)) `mod` 256) | (i, v) <- zip [0 ..] r ] | r <- rawRows ])
      predFile = fileOf (docWith "" [(10, streamObj "/Filter /FlateDecode /DecodeParms << /Predictor 11 /Colors 3 /Columns 6 >>" (zlibStored subRows))])
  check "pdfobj: a PNG predictor defaults BitsPerComponent to 8" $ case IM.lookup 10 (pfObjs predFile) of
    Just (OStream d raw) -> fst (decodeStreamF predFile fuelDefault d raw) == Right (BS.pack (map fromIntegral (concat rawRows)))
    _ -> False
  let lzwData = BS.pack [ fromIntegral ((i * i + i `div` 3) `mod` 251) | i <- [0 .. 3000 :: Int] ]
  forM_ [(True, ""), (False, " /DecodeParms << /EarlyChange 0 >>")] $ \(early, parm) -> do
    let lf = fileOf (docWith "" [(10, streamObj ("/Filter /LZWDecode" ++ parm) (lzwEncode early lzwData))])
    check ("pdfobj: LZW round trip, EarlyChange " ++ (if early then "1" else "0")) $ case IM.lookup 10 (pfObjs lf) of
      Just (OStream d raw) -> fst (decodeStreamF lf fuelDefault d raw) == Right lzwData
      _ -> False
  check "pdfobj: CCITTFaxDecode is an ordinary filter" $
    let cf = fileOf (docWith "" [(10, streamObj "/Filter /CCITTFaxDecode /DecodeParms << /K -1 /Columns 72 /Rows 20 >>" faxG4)])
    in case IM.lookup 10 (pfObjs cf) of
         Just (OStream d raw) -> fst (decodeStreamF cf fuelDefault d raw) == Right (faxPacked False)
         _ -> False

  -- Content tokens with inline images.
  let tricky = BS.pack [32, 69, 73, 32, 0, 0]       -- " EI \0\0": an EI inside the samples
      content = BS.concat [bs "q BI /W 2 /H 1 /CS /RGB /BPC 8 ID ", tricky, bs "\nEI Q BI /W 2 /H 1 /CS /G /BPC 8 /F /AHx ID 00ff> EI 1 0 0 RG"]
      toks = contentTokensInline content
      inl = [ (d, raw) | TInline d raw <- toks ]
      ops = [ o | TOp o <- toks ]
  check "pdfobj: an unfiltered inline image is read by its size, past an EI in its data"
    (map snd inl == [tricky, bs "00ff>"])
  check "pdfobj: tokens resume after an inline image" (ops == map bs ["q", "Q", "RG"])
  -- (The text view's skip keeps its old heuristic, which this data fools:
  -- it only needs to be sure never to produce an image token.)
  check "pdfobj: contentTokens still skips inline images" (null [ () | TInline _ _ <- contentTokens content ])

------------------------------------------------------------------------------
-- Functions

funcSpec :: (String -> Bool -> IO ()) -> IO ()
funcSpec check = do
  let objs = [ (10, bs "<< /FunctionType 2 /Domain [0 1] /C0 [0 0.5] /C1 [1 1] /N 2 >>")
             , (11, bs "<< /FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1 >>")
             , (12, bs "<< /FunctionType 3 /Domain [0 1] /Functions [11 0 R 11 0 R] /Bounds [0.5] /Encode [0 1 1 0] >>")
             , (13, streamObj "/FunctionType 0 /Domain [0 1] /Range [0 1 0 1] /Size [3] /BitsPerSample 8"
                              (BS.pack [0, 255, 128, 0, 255, 128]))
             , (14, streamObj "/FunctionType 0 /Domain [0 1 0 1] /Range [0 1] /Size [2 2] /BitsPerSample 8"
                              (BS.pack [0, 255, 255, 0]))
             , (15, streamObj "/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 4 /Decode [1 0]"
                              (BS.pack [0x0f]))
             , (16, streamObj "/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 12 /Encode [1 0]"
                              (BS.pack [0x00, 0x0f, 0xff]))
             , (17, bs "<< /FunctionType 3 /Domain [0 1] /Functions [17 0 R] /Bounds [] /Encode [0 1] >>")
             , (18, streamObj "/FunctionType 0 /Domain [0 1] /Range [0 1] /Size [2] /BitsPerSample 32"
                              (BS.pack [0, 0, 0, 0, 255, 255, 255, 255]))
             ]
      f = fileOf (docWith "" objs)
      fn k = fst (parseFunction f fuelDefault (ORef k))
      ev k xs = either (const []) (`evalFunction` xs) (fn k)
      ps src dom rng = fst (parseFunction (fileOf (docWith "" [(10, streamObj ("/FunctionType 4 /Domain " ++ dom ++ " /Range " ++ rng) (bs src))])) fuelDefault (ORef 10))
      evps src dom rng xs = either (const []) (`evalFunction` xs) (ps src dom rng)
  check "pdffunc: exponential" (nearL 1e-9 (ev 10 [0.5]) [0.25, 0.625])
  check "pdffunc: exponential clips its input to the domain" (nearL 1e-9 (ev 10 [3]) [1, 1])
  check "pdffunc: stitching with a reversing Encode"
    (nearL 1e-9 (ev 12 [0.25]) [0.5] && nearL 1e-9 (ev 12 [0.75]) [0.5] && nearL 1e-9 (ev 12 [0.6]) [0.2])
  check "pdffunc: sampled, one input, linear interpolation" (nearL 1e-6 (ev 13 [0.25]) [64 / 255, 127.5 / 255])
  check "pdffunc: sampled hits its grid points exactly" (nearL 1e-9 (ev 13 [1]) [1, 128 / 255])
  check "pdffunc: sampled, two inputs, bilinear" (nearL 1e-6 (ev 14 [0.5, 0.5]) [0.5] && nearL 1e-6 (ev 14 [0.25, 0]) [0.25])
  check "pdffunc: sampled Decode inverts" (nearL 1e-9 (ev 15 [0]) [1] && nearL 1e-9 (ev 15 [1]) [0])
  check "pdffunc: sampled 12-bit with a reversed Encode" (nearL 1e-9 (ev 16 [0]) [1] && nearL 1e-9 (ev 16 [1]) [0])
  check "pdffunc: sampled 32-bit" (nearL 1e-9 (ev 18 [1]) [1] && nearL 1e-9 (ev 18 [0.5]) [0.5])
  check "pdffunc: a function that contains itself is refused" (either (const True) (const False) (fn 17))
  check "pdffunc: an array of functions concatenates outputs" $
    case fst (parseFunction f fuelDefault (OArr [ORef 11, ORef 10])) of
      Right fa -> nearL 1e-9 (evalFunction fa [0.5]) [0.5, 0.25, 0.625] && funcOutputs fa == 3
      Left _ -> False
  check "pdffunc: postscript arithmetic and roll"
    (nearL 1e-9 (evps "{ 2 copy add 3 1 roll mul }" "[0 1 0 1]" "[0 2 0 1]" [0.5, 0.25]) [0.75, 0.125])
  check "pdffunc: postscript ifelse"
    (nearL 1e-9 (evps "{ dup 0.5 gt { 1 } { 0 } ifelse }" "[0 1]" "[0 1 0 1]" [0.7]) [0.7, 1]
     && nearL 1e-9 (evps "{ dup 0.5 gt { 1 } { 0 } ifelse }" "[0 1]" "[0 1 0 1]" [0.2]) [0.2, 0])
  check "pdffunc: postscript if" (nearL 1e-9 (evps "{ dup 0.5 lt { pop 0 } if }" "[0 1]" "[0 1]" [0.3]) [0])
  check "pdffunc: postscript integers" (nearL 1e-9 (evps "{ cvi 3 idiv 7 mod }" "[0 100]" "[0 100]" [50]) [2])
  check "pdffunc: postscript bitshift" (nearL 1e-9 (evps "{ cvi 2 bitshift }" "[0 10]" "[0 100]" [3]) [12])
  check "pdffunc: postscript roll order"
    (nearL 1e-9 (evps "{ 1 2 3 3 1 roll }" "[0 10]" "[0 10 0 10 0 10 0 10]" [5]) [5, 3, 1, 2])
  check "pdffunc: postscript index and copy" (nearL 1e-9 (evps "{ 0 index add 1 copy pop }" "[0 1]" "[0 2]" [0.3]) [0.6])
  check "pdffunc: postscript degrees" (nearL 1e-9 (evps "{ pop 90 sin 1 1 atan }" "[0 1]" "[0 1 0 360]" [0]) [1, 45])
  check "pdffunc: postscript comparisons and booleans"
    (nearL 1e-9 (evps "{ 0.5 ge true and { 1 } { 0 } ifelse }" "[0 1]" "[0 1]" [0.5]) [1])
  check "pdffunc: postscript run-time error gives the range's low end"
    (nearL 1e-9 (evps "{ 0 div }" "[0 1]" "[0.25 1]" [0.5]) [0.25])
  check "pdffunc: postscript stack limit"
    (nearL 1e-9 (evps ("{ " ++ unwords (replicate 150 "1") ++ " }") "[0 1]" "[0 1]" [0.5]) [0])
  check "pdffunc: postscript outputs are clipped to the range" (nearL 1e-9 (evps "{ 10 mul }" "[0 1]" "[0 1]" [0.5]) [1])
  check "pdffunc: postscript unknown operator is refused" (either (const True) (const False) (ps "{ 1 frobnicate }" "[0 1]" "[0 1]"))
  check "pdffunc: postscript reals with exponents and radix integers"
    (nearL 1e-9 (evps "{ pop 2.5e-1 16#10 add }" "[0 1]" "[0 100]" [0]) [16.25])
  check "pdffunc: evaluation never yields NaN"
    (all (not . isNaN) (evps "{ -1 exp }" "[-1 1]" "[0 1]" [0]) && all (not . isNaN) (ev 10 [0 / 0]))

------------------------------------------------------------------------------
-- Colour spaces

colourSpec :: (String -> Bool -> IO ()) -> IO ()
colourSpec check = do
  let objs = [ (10, bs "<< /FunctionType 2 /Domain [0 1] /C0 [1 1 1] /C1 [1 0 0] /N 1 >>")
             , (11, streamObj "/FunctionType 4 /Domain [0 1 0 1] /Range [0 1]" (bs "{ add 2 div }"))
             , (12, streamObj "/N 4" (bs "profile"))
             , (13, streamObj "/N 3 /Alternate /DeviceGray" (bs "profile"))
             , (14, streamObj "" (BS.pack [255, 0, 0, 0, 0, 255]))
             ]
      f = fileOf (docWith "" objs)
      res = M.fromList [(bs "ColorSpace", ODict (M.fromList
              [ (bs "DefaultRGB", OArr [OName (bs "CalRGB"), ODict (M.fromList [(bs "WhitePoint", OArr (map ONum [0.9505, 1, 1.089]))])])
              , (bs "CS0", OArr [OName (bs "Indexed"), OName (bs "DeviceRGB"), ONum 1, OStr (BS.pack [255, 0, 0, 0, 255, 0])])
              , (bs "Loop", OName (bs "Loop")) ]))]
      cs o = fst (parseColorSpace f fuelDefault res o)
      csNoRes o = fst (parseColorSpace f fuelDefault M.empty o)
      rgbOf e xs = either (const (-1, -1, -1)) (`toRGB` xs) e
      near3 tol (a, b, c) (x, y, z) = nearD tol a x && nearD tol b y && nearD tol c z
      nm = OName . bs
  check "colour: CMYK is poppler's sixteen-corner blend"
    (near3 1e-9 (toRGB CSCMYK [0, 0, 0, 0]) (1, 1, 1) && near3 1e-9 (toRGB CSCMYK [0, 0, 0, 1]) (0.1373, 0.1216, 0.1255)
     && near3 1e-9 (toRGB CSCMYK [1, 0, 0, 0]) (0, 0.6784, 0.9373) && near3 1e-9 (toRGB CSCMYK [1, 1, 1, 1]) (0, 0, 0)
     && near3 1e-9 (toRGB CSCMYK [0.5, 0, 0, 0]) (0.5, 0.8392, 0.96865)
     && near3 1e-9 (toRGB CSCMYK [2, -1, 0, 0]) (toRGB CSCMYK [1, 0, 0, 0]))
  check "colour: CMYK through an alternate uses the same conversion"
    (near3 1e-9 (rgbOf (csNoRes (OArr [nm "ICCBased", ORef 12])) [0.3, 0.2, 0.1, 0.4]) (cmykToRGB 0.3 0.2 0.1 0.4))
  check "colour: gray and RGB clamp" (near3 1e-9 (toRGB CSGray [2]) (1, 1, 1) && near3 1e-9 (toRGB CSRGB [0.1, -1]) (0.1, 0, 0))
  check "colour: Lab white and black, D65 and D50"
    (near3 0.01 (rgbOf (csNoRes (OArr [nm "Lab", ODict (M.fromList [(bs "WhitePoint", OArr (map ONum [0.9505, 1, 1.089]))])])) [100, 0, 0]) (1, 1, 1)
     && near3 0.01 (rgbOf (csNoRes (OArr [nm "Lab", ODict (M.fromList [(bs "WhitePoint", OArr (map ONum [0.9642, 1, 0.8249]))])])) [100, 0, 0]) (1, 1, 1)
     && near3 0.01 (rgbOf (csNoRes (OArr [nm "Lab", ODict M.empty])) [0, 0, 0]) (0, 0, 0))
  check "colour: Lab red is red" $ case rgbOf (csNoRes (OArr [nm "Lab", ODict M.empty])) [54, 80, 67] of
    (r, g, b) -> r > 0.9 && g < 0.25 && b < 0.25
  check "colour: CalGray gamma 1 is lighter than DeviceGray"
    (case rgbOf (csNoRes (OArr [nm "CalGray", ODict (M.fromList [(bs "WhitePoint", OArr (map ONum [0.9505, 1, 1.089]))])])) [0.5] of
       (g, _, _) -> nearD 0.01 g 0.7354)
  check "colour: Indexed from a string" (near3 1e-9 (rgbOf (cs (nm "CS0")) [1]) (0, 1, 0) && near3 1e-9 (rgbOf (cs (nm "CS0")) [7]) (0, 1, 0))
  check "colour: Indexed from a stream" (near3 1e-9 (rgbOf (csNoRes (OArr [nm "Indexed", nm "DeviceRGB", ONum 1, ORef 14])) [0]) (1, 0, 0))
  check "colour: Separation through its tint transform"
    (near3 1e-9 (rgbOf (csNoRes (OArr [nm "Separation", nm "Spot", nm "DeviceRGB", ORef 10])) [0.5]) (1, 0.5, 0.5))
  check "colour: Separation All is drawn through its tint transform too"
    (near3 1e-9 (rgbOf (csNoRes (OArr [nm "Separation", nm "All", nm "DeviceRGB", ORef 10])) [1]) (1, 0, 0))
  check "colour: DeviceN with a PostScript tint transform"
    (near3 1e-9 (rgbOf (csNoRes (OArr [nm "DeviceN", OArr [nm "A", nm "B"], nm "DeviceGray", ORef 11])) [0.2, 0.6]) (0.4, 0.4, 0.4))
  check "colour: None is invisible"
    (either (const False) csInvisible (csNoRes (OArr [nm "Separation", nm "None", nm "DeviceGray", ORef 10]))
     && either (const False) csInvisible (csNoRes (OArr [nm "DeviceN", OArr [nm "None", nm "None"], nm "DeviceGray", ORef 11])))
  check "colour: ICCBased by /N" (either (const False) ((== 4) . csComponents) (csNoRes (OArr [nm "ICCBased", ORef 12]))
                                   && near3 1e-9 (rgbOf (csNoRes (OArr [nm "ICCBased", ORef 12])) [0, 0, 0, 1]) (cmykToRGB 0 0 0 1))
  check "colour: an ICCBased alternate with the wrong count is replaced"
    (either (const False) ((== 3) . csComponents) (csNoRes (OArr [nm "ICCBased", ORef 13])))
  check "colour: DefaultRGB replaces DeviceRGB in its scope" $ case cs (nm "DeviceRGB") of
    Right (CSCalRGB {}) -> True
    _ -> False
  check "colour: a named space that names itself ends" (either (const True) (const False) (cs (nm "Loop")))
  check "colour: an unknown name is an error" (either (const True) (const False) (cs (nm "Nope")))
  check "colour: Pattern with an underlying space" $ case csNoRes (OArr [nm "Pattern", nm "DeviceRGB"]) of
    Right (CSPattern (Just CSRGB)) -> True
    _ -> False
  check "colour: initialColor" (initialColor CSCMYK == [0, 0, 0, 1] && initialColor CSRGB == [0, 0, 0]
                                && either (const False) ((== [1]) . initialColor) (csNoRes (OArr [nm "Separation", nm "S", nm "DeviceRGB", ORef 10])))
  check "colour: defaultDecode" (defaultDecode CSRGB 8 == replicate 3 (0, 1)
                                 && either (const False) (\c -> defaultDecode c 4 == [(0, 15)]) (cs (nm "CS0"))
                                 && defaultDecode (CSLab (1, 1, 1) (-10, 10, -20, 20)) 8 == [(0, 100), (-10, 10), (-20, 20)])

------------------------------------------------------------------------------
-- CCITT

-- The pattern Ghostscript encoded for the fax fixtures: 72x20, 1 = black.
faxPattern :: [[Bool]]
faxPattern = [ [ (x - 30) ^ (2 :: Int) + (y - 10) ^ (2 :: Int) < 64 || x `mod` 17 == 3 || (y == 2 && x > 40) || (x > 60 && y `mod` 3 == 0)
               | x <- [0 .. 71 :: Int] ] | y <- [0 .. 19 :: Int] ]

-- The pattern as PDF's default output (0 = black), or BlackIs1.
faxPacked :: Bool -> BS.ByteString
faxPacked blackIs1 = BS.pack (concatMap row faxPattern)
  where row r = [ foldl' (\a b -> a * 2 + (if b == blackIs1 then 1 else 0)) 0 (take 8 (drop (8 * k) r)) | k <- [0 .. 8] ]

faxG3, faxG32D, faxG4 :: BS.ByteString
faxG3 = hexBS "0018552a954b82800c2a954aa5520018552a94034800c2a970e154b82800c2a95851d54aa40030aa500f52a9000c2a9403d4b82800c2a93862a552001855270c54aa40030aa4e18a9705001855270c54aa40030aa4e18a95480061549c3152e0a0030aa4e18a9548006154a01ea5520018552807a97050018552b0a3aa9548006154b870aa5520018552a954b82800c2a954aa5520"
faxG32D = hexBS "001c2a954aa5c14005fe6a40038552a94034800bcf0f3523c140070aa56147552a9000bd0fc0070aa500f52e0a002f5cd480070aa4e18a9548005fe782800e1549c3152a9000bff001c2a93862a5c14005fe6a40038552807a9548005fe782800e154ac28eaa55200178617e0038552a954b82800bfcd480"
faxG4 = hexBS "308d48d48d48f05ff352fcc034f9e1e6a4782f841f9a97a1fff9e0beb9a97fffe782ff9a97fffe782ff9a97bfff9e0bec223acd4bc30bff1f3c17fcd48"

-- A Modified Huffman (G3 1-D) encoder over the module's own code tables,
-- for rows given as run lengths (white first): with or without EOLs, and
-- with or without byte alignment of every row.
mhEncode :: Bool -> Bool -> [[Int]] -> BS.ByteString
mhEncode eols align rows = bitsToBytes (concatMap row rows)
  where
    row runs = pad (eolBits ++ concat (zipWith run (cycle [False, True]) runs))
      where pad b = if align then b ++ replicate ((8 - length b `mod` 8) `mod` 8) False else b
            eolBits = if eols then map (== '1') "000000000001" else []
    -- (The fill goes at the end of each row: without EOLs the next row then
    -- starts on a byte, and with them the fill is the zero run the decoder
    -- skips on its way to the EOL, which is where T.4 puts fill bits.)
    run black n = concatMap (code black) (pieces n)
    pieces n | n >= 2560 = 2560 : pieces (n - 2560)
             | n >= 64 = (n `div` 64) * 64 : [n `mod` 64]
             | otherwise = [n]
    code black v = maybe [] (map (== '1')) (lookup v (if black then blackCodes else whiteCodes))
    bitsToBytes [] = BS.empty
    bitsToBytes b = BS.pack (go b)
      where go [] = []
            go xs = let (a, r) = splitAt 8 xs
                    in foldl' (\acc x -> acc * 2 + (if x then 1 else 0)) 0 (take 8 (a ++ repeat False)) : go r

ccittSpec :: (String -> Bool -> IO ()) -> IO ()
ccittSpec check = do
  let p k = defaultCcitt { ccK = k, ccColumns = 72 }
      dec prm d = decodeCcitt prm (1024 * 1024) d
  check "ccitt: Group 3 1-D (ghostscript faxg3)" (crData (dec (p 0) faxG3) == faxPacked False && crRows (dec (p 0) faxG3) == 20)
  check "ccitt: Group 3 1-D with EndOfLine" (crData (dec (p 0) { ccEndOfLine = True } faxG3) == faxPacked False)
  check "ccitt: Group 3 2-D (ghostscript faxg32d)" (crData (dec (p 1) faxG32D) == faxPacked False)
  check "ccitt: Group 4 (ghostscript faxg4)" (crData (dec (p (-1)) faxG4) == faxPacked False && crDamaged (dec (p (-1)) faxG4) == 0)
  check "ccitt: BlackIs1" (crData (dec (p (-1)) { ccBlackIs1 = True } faxG4) == faxPacked True)
  check "ccitt: Rows limits the output" (crRows (dec (p (-1)) { ccRows = 7 } faxG4) == 7)
  check "ccitt: the byte limit bounds the output" (crRows (decodeCcitt (p (-1)) 50 faxG4) == 5)
  check "ccitt: no columns, no output" (crRows (dec (p (-1)) { ccColumns = 0 } faxG4) == 0)
  check "ccitt: code tables are prefix-free" $
    let free cs = and [ not (a `isPrefixOf` b) | (i, (_, a)) <- zip [0 :: Int ..] cs, (j, (_, b)) <- zip [0 ..] cs, i /= j ]
    in free whiteCodes && free blackCodes
  -- Our own MH encodings: long runs (make-up and extended make-up codes),
  -- byte alignment, EOLs, Rows = 0 (decode until the data ends).
  let width = 3000
      rows = [ [5, 70, 1, 2924], [0, 2000, 1000], [3000], [1, 1, 64, 2934], [2561, 439] ]
      expect = BS.pack (concatMap (packRun width) rows)
      prm a e = defaultCcitt { ccK = 0, ccColumns = width, ccByteAlign = a, ccEndOfLine = e }
  forM_ [(False, False), (False, True), (True, False), (True, True)] $ \(a, e) ->
    check ("ccitt: MH runs over 2560, align=" ++ show a ++ " eol=" ++ show e)
      (crData (dec (prm a e) (mhEncode e a rows)) == expect && crRows (dec (prm a e) (mhEncode e a rows)) == 5)
  where
    packRun width runs =
      let px = concat (zipWith replicate runs (cycle [False, True]))
          bits = take width (px ++ repeat False)
      in [ foldl' (\a b -> a * 2 + (if b then 0 else 1)) 0 (take 8 (drop (8 * k) bits ++ repeat False))
         | k <- [0 .. (width + 7) `div` 8 - 1] ]

------------------------------------------------------------------------------
-- CMYK and YCCK JPEGs

-- 16x16, quadrants (ink) C200 M30 / M180 Y60 K20 / C10 M10 Y200 / K220.
jpegCmyk, jpegYcck :: BS.ByteString
jpegCmyk = hexBS "ffd8ffee000e41646f626500640000000000ffdb0043000201010101010201010102020202020403020202020504040304060506060605060606070908060709070606080b08090a0a0a0a0a06080b0c0b0a0c090a0a0affc000140800100010044311004d11005911004b1100ffc4001f0000010501010101010100000000000000000102030405060708090a0bffc400b5100002010303020403050504040000017d01020300041105122131410613516107227114328191a1082342b1c11552d1f02433627282090a161718191a25262728292a3435363738393a434445464748494a535455565758595a636465666768696a737475767778797a838485868788898a92939495969798999aa2a3a4a5a6a7a8a9aab2b3b4b5b6b7b8b9bac2c3c4c5c6c7c8c9cad2d3d4d5d6d7d8d9dae1e2e3e4e5e6e7e8e9eaf1f2f3f4f5f6f7f8f9faffda000e0443004d0059004b00003f00fcdbafd84afdfcafdfcafec82bf9a7af83ebcbeb97afeaa2bf9cfaf50aea2ba8afec82bf88faffd9"
jpegYcck = hexBS "ffd8ffee000e41646f626500640000000002ffdb0043000201010101010201010102020202020403020202020504040304060506060605060606070908060709070606080b08090a0a0a0a0a06080b0c0b0a0c090a0a0affdb004301020202020202050303050a0706070a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0affc00014080010001004011100021101031101041100ffc4001700000301000000000000000000000000000708090affc40014100100000000000000000000000000000000ffc4001701010101010000000000000000000000000906080affc40014110100000000000000000000000000000000ffda000e040100021103110400003f004cd93cae2fe1905c39630bd2ecfa104140bf8835e311efffd9"

-- The quadrants' ink, and their expected RGB in the image view (ImageMagick's
-- multiplicative conversion) and in a PDF (poppler's, 'cmykToRGB').
quadrantInk :: [[Double]]
quadrantInk = [[200, 30, 0, 0], [0, 180, 60, 20], [10, 10, 200, 0], [0, 0, 0, 220]]

quadrantRGB :: [[Int]]
quadrantRGB = [[55, 225, 255, 255], [235, 69, 180, 255], [245, 245, 55, 255], [35, 35, 35, 255]]

quadrantPdfRGB :: [[Int]]
quadrantPdfRGB = [ let (r, g, b) = cmykToRGB (c / 255) (m / 255) (y / 255) (k / 255)
                   in [round (r * 255), round (g * 255), round (b * 255), 255]
                 | [c, m, y, k] <- quadrantInk ]

jpegSpec :: (String -> Bool -> IO ()) -> IO ()
jpegSpec check = do
  let quads im = [ take 4 (drop ((y * imgW im + x) * 4) (map fromIntegral (U.elems (imgPix im)))) | (x, y) <- [(3, 3), (12, 3), (3, 12), (12, 12)] ]
  forM_ [("CMYK (Adobe, inverted)", jpegCmyk), ("YCCK", jpegYcck)] $ \(label, j) ->
    check ("jpeg: " ++ label ++ " decodes for display") $ case decodeJPEG j of
      Right im -> imgW im == 16 && imgH im == 16 && and (zipWith (near 4) (quads im) quadrantRGB)
      Left _ -> False
  check "jpeg: raw CMYK samples and the Adobe flag" $ case decodeJPEGSamples Nothing jpegCmyk of
    Right js -> jsComps js == 4 && jsAdobe js == Just 0
                && near 4 (take 4 (map fromIntegral (U.elems (jsData js)))) [55, 225, 255, 255]   -- stored inverted
    Left _ -> False
  check "jpeg: YCCK samples come back as CMYK" $ case (decodeJPEGSamples Nothing jpegYcck, decodeJPEGSamples Nothing jpegCmyk) of
    (Right a, Right b) -> jsAdobe a == Just 2
                          && near 6 (take 4 (map fromIntegral (U.elems (jsData a)))) (take 4 (map fromIntegral (U.elems (jsData b))))
    _ -> False
  check "jpeg: a DCT CMYK image in a PDF honours its Decode" $
    case rgbaOf (imageIn "/Type /XObject /Subtype /Image /Width 16 /Height 16 /ColorSpace /DeviceCMYK /BitsPerComponent 8 /Filter /DCTDecode /Decode [1 0 1 0 1 0 1 0]" jpegCmyk []) of
      Just img -> and (zipWith (near 4) [ pixelAt img x y | (x, y) <- [(3, 3), (12, 3), (3, 12), (12, 12)] ] quadrantPdfRGB)
      Nothing -> False

------------------------------------------------------------------------------
-- Images

imageSpec :: (String -> Bool -> IO ()) -> IO ()
imageSpec check = do
  let hdr extra = "/Type /XObject /Subtype /Image " ++ extra
      gray2 = [[0, 1, 2, 3, 0], [3, 3, 0, 1, 2]]
  check "image: 2-bit gray with row padding" $ case rgbaOf (imageIn (hdr "/Width 5 /Height 2 /ColorSpace /DeviceGray /BitsPerComponent 2") (packRows 2 gray2) []) of
    Just img -> [ head (pixelAt img x y) | y <- [0, 1], x <- [0 .. 4] ] == map (* 85) (concat gray2)
    Nothing -> False
  check "image: 1-bit and 4-bit gray" $
    let one = rgbaOf (imageIn (hdr "/Width 9 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 1") (packRows 1 [[1, 0, 1, 1, 0, 0, 0, 0, 1]]) [])
        four = rgbaOf (imageIn (hdr "/Width 3 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 4") (packRows 4 [[0, 15, 5]]) [])
    in fmap (\i -> [ head (pixelAt i x 0) | x <- [0 .. 8] ]) one == Just [255, 0, 255, 255, 0, 0, 0, 0, 255]
       && fmap (\i -> [ head (pixelAt i x 0) | x <- [0 .. 2] ]) four == Just [0, 255, 85]
  check "image: 16-bit gray" $ case rgbaOf (imageIn (hdr "/Width 3 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 16") (packRows 16 [[0, 65535, 32768]]) []) of
    Just img -> [ head (pixelAt img x 0) | x <- [0 .. 2] ] == [0, 255, 128]
    Nothing -> False
  check "image: RGB at 4 bits and Decode [1 0]" $
    let a = rgbaOf (imageIn (hdr "/Width 1 /Height 1 /ColorSpace /DeviceRGB /BitsPerComponent 4") (packRows 4 [[15, 0, 5]]) [])
        b = rgbaOf (imageIn (hdr "/Width 1 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 8 /Decode [1 0]") (BS.pack [0]) [])
    in fmap (\i -> pixelAt i 0 0) a == Just [255, 0, 85, 255] && fmap (\i -> pixelAt i 0 0) b == Just [255, 255, 255, 255]
  check "image: CMYK raw uses the same conversion as a vector colour" $
    let px = [ [c, m, y, k] | c <- [0, 37, 128, 255], m <- [0, 200], y <- [0, 90, 255], k <- [0, 64, 255] ]
        expect [c, m, y, k] = let f v = fromIntegral v / 255
                                  (r, g, b) = cmykToRGB (f c) (f m) (f y) (f k)
                              in [round (r * 255), round (g * 255), round (b * 255), 255]
        expect _ = []
        n = length px
    in case rgbaOf (imageIn (hdr ("/Width " ++ show n ++ " /Height 1 /ColorSpace /DeviceCMYK /BitsPerComponent 8"))
                            (BS.pack (map fromIntegral (concat px))) []) of
         Just img -> and [ near 1 (pixelAt img x 0) (expect p) | (x, p) <- zip [0 ..] px ]
         Nothing -> False
  check "image: Indexed with a reversed Decode" $
    case rgbaOf (imageIn (hdr "/Width 2 /Height 1 /ColorSpace [/Indexed /DeviceRGB 1 <ff000000ff00>] /BitsPerComponent 1 /Decode [1 0]") (packRows 1 [[0, 1]]) []) of
      Just img -> pixelAt img 0 0 == [0, 255, 0, 255] && pixelAt img 1 0 == [255, 0, 0, 255]
      Nothing -> False
  check "image: Lab through the generic path" $
    case rgbaOf (imageIn (hdr "/Width 1 /Height 1 /ColorSpace [/Lab << /WhitePoint [0.9505 1 1.089] >>] /BitsPerComponent 8") (BS.pack [255, 128, 128]) []) of
      Just img -> near 3 (pixelAt img 0 0) [255, 255, 255, 255]
      Nothing -> False
  check "image: short data reads as zero, not an error" $
    case rgbaOf (imageIn (hdr "/Width 4 /Height 4 /ColorSpace /DeviceGray /BitsPerComponent 8") (BS.pack [255, 255, 255]) []) of
      Just img -> pixelAt img 2 0 == [255, 255, 255, 255] && pixelAt img 3 3 == [0, 0, 0, 255]
      Nothing -> False
  check "image: colour-key mask" $
    case imageIn (hdr "/Width 2 /Height 1 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Mask [0 10 0 255 0 255]") (BS.pack [5, 200, 9, 50, 0, 0]) [] of
      r@(Right (PdfColorImage _ _ True)) -> fmap (\i -> (pixelAt i 0 0 !! 3, pixelAt i 1 0 !! 3)) (rgbaOf r) == Just (0, 255)
      _ -> False
  check "image: soft mask with Matte un-premultiplies" $
    let sm = streamObj "/Type /XObject /Subtype /Image /Width 1 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 8 /Matte [0.5 0.5 0.5]" (BS.pack [128])
    in case rgbaOf (imageIn (hdr "/Width 1 /Height 1 /ColorSpace /DeviceRGB /BitsPerComponent 8 /SMask 11 0 R") (BS.pack [164, 128, 91]) [(11, sm)]) of
         Just img -> near 1 (pixelAt img 0 0) [200, 128, 54, 128]
         Nothing -> False
  check "image: an explicit stencil mask of another size" $
    let m = streamObj "/Type /XObject /Subtype /Image /Width 4 /Height 2 /ImageMask true /BitsPerComponent 1" (packRows 1 [[0, 1, 0, 1], [1, 1, 0, 0]])
    in case rgbaOf (imageIn (hdr "/Width 2 /Height 1 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Mask 11 0 R") (BS.pack [255, 0, 0, 0, 0, 255]) [(11, m)]) of
         Just img@(w, h, _) -> w == 4 && h == 2 && map (!! 3) [ pixelAt img x y | y <- [0, 1], x <- [0 .. 3] ] == [255, 0, 255, 0, 0, 0, 255, 255]
                               && take 3 (pixelAt img 3 1) == [0, 0, 255]
         Nothing -> False
  check "image: image masks, both Decodes" $
    let st d = case imageIn (hdr ("/Width 8 /Height 1 /ImageMask true" ++ d)) (BS.pack [0xa0]) [] of
          Right (PdfStencil m _) -> U.elems (mkPix m)
          _ -> []
    in st "" == [0, 255, 0, 255, 255, 255, 255, 255] && st " /Decode [1 0]" == [255, 0, 255, 0, 0, 0, 0, 0]
  check "image: a fax image through CCITTFaxDecode" $
    case rgbaOf (imageIn (hdr "/Width 72 /Height 20 /ColorSpace /DeviceGray /BitsPerComponent 1 /Filter /CCITTFaxDecode /DecodeParms << /K -1 /Columns 72 /Rows 20 >>") faxG4 []) of
      Just img -> and [ (head (pixelAt img x y) == 0) == (faxPattern !! y !! x) | y <- [0 .. 19], x <- [0 .. 71] ]
      Nothing -> False
  check "image: a fax stencil paints the black pixels" $
    case imageIn (hdr "/Width 72 /Height 20 /ImageMask true /Filter /CCF /DecodeParms << /K -1 /Columns 72 >>") faxG4 [] of
      Right (PdfStencil m _) -> and [ (mkPix m U.! (y * 72 + x) == 255) == (faxPattern !! y !! x) | y <- [0 .. 19], x <- [0 .. 71] ]
      _ -> False
  check "image: JPEG 2000 and JBIG2 are unsupported, not corrupt" $
    failureOf (imageIn (hdr "/Width 4 /Height 4 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /JPXDecode") (bs "xx") []) == Just (ImgUnsupported "JPEG 2000")
      && failureOf (imageIn (hdr "/Width 4 /Height 4 /ColorSpace /DeviceGray /BitsPerComponent 1 /Filter /JBIG2Decode") (bs "xx") []) == Just (ImgUnsupported "JBIG2")
  check "image: an unknown filter is unsupported" $
    case imageIn (hdr "/Width 4 /Height 4 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /Bogus") (bs "xx") [] of
      Left (ImgUnsupported _) -> True
      _ -> False
  check "image: no size, absurd size and a bad depth are corrupt" $
    and [ case imageIn (hdr e) (bs "xx") [] of Left (ImgCorrupt _) -> True; _ -> False
        | e <- [ "/Width 0 /Height 4 /ColorSpace /DeviceGray /BitsPerComponent 8"
               , "/Width 100000 /Height 100000 /ColorSpace /DeviceGray /BitsPerComponent 8"
               , "/Width 4 /Height 4 /ColorSpace /DeviceGray /BitsPerComponent 3" ] ]
  check "image: a None separation is fully transparent" $
    case rgbaOf (imageIn (hdr "/Width 1 /Height 1 /ColorSpace [/Separation /None /DeviceGray 11 0 R] /BitsPerComponent 8") (BS.pack [200]) [(11, bs "<< /FunctionType 2 /Domain [0 1] /N 1 >>")]) of
      Just img -> pixelAt img 0 0 !! 3 == 0
      Nothing -> False
  check "image: /Interpolate is passed through" $ case imageIn (hdr "/Width 1 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 8 /Interpolate true") (BS.pack [1]) [] of
    Right (PdfColorImage _ True _) -> True
    _ -> False
  -- Inline images.
  let f = fileOf (docWith "" [])
      inline content = [ fst (decodeInlineImage f fuelDefault M.empty d raw) | TInline d raw <- contentTokensInline (bs content) ]
  check "image: an inline image with abbreviations" $
    case map rgbaOf (inline "BI /W 2 /H 1 /CS /RGB /BPC 8 /F /AHx ID ff000000ff00> EI") of
      [Just img] -> pixelAt img 0 0 == [255, 0, 0, 255] && pixelAt img 1 0 == [0, 255, 0, 255]
      _ -> False
  check "image: an inline Indexed image with an abbreviated base" $
    case map rgbaOf (inline "BI /W 2 /H 1 /CS [/I /RGB 1 <ff0000 0000ff>] /BPC 8 /F /AHx ID 0001> EI") of
      [Just img] -> pixelAt img 0 0 == [255, 0, 0, 255] && pixelAt img 1 0 == [0, 0, 255, 255]
      _ -> False
  check "image: an inline image mask" $
    case inline "BI /W 8 /H 1 /IM true /D [1 0] /F /AHx ID a0> EI" of
      [Right (PdfStencil m _)] -> U.elems (mkPix m) == [255, 0, 255, 0, 0, 0, 0, 0]
      _ -> False
  check "image: expandInlineDict" $
    let d = expandInlineDict (M.fromList [(bs "BPC", ONum 8), (bs "F", OArr [OName (bs "A85"), OName (bs "Fl")]), (bs "CS", OName (bs "CMYK"))])
    in M.lookup (bs "BitsPerComponent") d == Just (ONum 8)
       && M.lookup (bs "Filter") d == Just (OArr [OName (bs "ASCII85Decode"), OName (bs "FlateDecode")])
       && M.lookup (bs "ColorSpace") d == Just (OName (bs "DeviceCMYK"))
  where failureOf r = either Just (const Nothing) r

------------------------------------------------------------------------------
-- Fuzzing: truncations at every offset and bit flips, never an exception.

fuzzSpec :: (String -> Bool -> IO ()) -> IO ()
fuzzSpec check = do
  let mutants b = [ BS.take k b | k <- [0 .. BS.length b] ]
                  ++ [ flipBit b i | i <- [0, 3 .. 8 * BS.length b - 1] ]
      flipBit b i = let (q, r) = i `divMod` 8
                    in BS.concat [BS.take q b, BS.singleton (BS.index b q `xor` (0x80 `shiftR` r)), BS.drop (q + 1) b]
      allOk xs = and <$> sequence xs
  ok1 <- allOk [ noThrow (\r -> BS.length (crData r) + crRows r) (decodeCcitt defaultCcitt { ccK = k, ccColumns = 72 } 100000 m)
               | (k, fx) <- [(-1, faxG4), (0, faxG3), (1, faxG32D)], m <- mutants fx ]
  check "fuzz: CCITT G3/G4 truncations and bit flips" ok1
  ok1b <- allOk [ noThrow (\r -> BS.length (crData r)) (decodeCcitt defaultCcitt { ccK = k, ccColumns = c, ccEndOfLine = e, ccByteAlign = a } 200000 g)
                | k <- [-1, 0, 2], c <- [1, 7, 1728], e <- [False, True], a <- [False, True]
                , g <- [BS.pack (map fromIntegral [ (i * 37 + 11) `mod` 256 | i <- [0 .. 300 :: Int] ]), BS.replicate 50 0, BS.replicate 50 0xff] ]
  check "fuzz: CCITT on garbage, every mode" ok1b
  ok2 <- allOk [ noThrow (either length (\i -> sumArr 0 (imgPix i))) (decodeJPEG m)
               | j <- [jpegCmyk, jpegYcck], m <- mutants j ]
  check "fuzz: CMYK/YCCK JPEG truncations and bit flips (display)" ok2
  ok3 <- allOk [ noThrow (either length (\s -> sumArr 0 (jsData s))) (decodeJPEGSamples o m)
               | j <- [jpegCmyk, jpegYcck], o <- [Nothing, Just True], m <- mutants j ]
  check "fuzz: CMYK/YCCK JPEG truncations and bit flips (samples)" ok3
  let hdr extra = "/Type /XObject /Subtype /Image " ++ extra
      cases = [ (hdr "/Width 72 /Height 20 /ColorSpace /DeviceGray /BitsPerComponent 1 /Filter /CCITTFaxDecode /DecodeParms << /K -1 /Columns 72 >>", faxG4)
              , (hdr "/Width 16 /Height 16 /ColorSpace /DeviceCMYK /BitsPerComponent 8 /Filter /DCTDecode /Decode [1 0 1 0 1 0 1 0]", jpegCmyk)
              , (hdr "/Width 5 /Height 3 /ColorSpace [/Indexed /DeviceRGB 3 <ff000000ff000000ffffffff>] /BitsPerComponent 2 /Filter /FlateDecode", zlibStored (BS.pack [0x1b, 0xc0, 0x6c, 0x40, 0xe4, 0x80]))
              , (hdr "/Width 4 /Height 3 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /ASCIIHexDecode /Mask [0 100 0 255 0 255]", bs "ff00ff00ff00aa00bb00cc00dd00ee0011223344556677889900aabbccddeeff>")
              , (hdr "/Width 4 /Height 2 /ImageMask true /Filter /RunLengthDecode", BS.pack [1, 0xa0, 0x50, 128]) ]
  ok4 <- allOk [ noThrow forceImg (imageIn d m []) | (d, raw) <- cases, m <- mutants raw ]
  check "fuzz: decodePdfImage truncations and bit flips of the data" ok4
  -- The dictionary text too: a damaged dictionary must never throw either.
  let dictMutants s = [ take k s | k <- [0, 7 .. length s] ] ++ [ take k s ++ "%" ++ drop (k + 1) s | k <- [0, 5 .. length s - 1] ]
  ok5 <- allOk [ noThrow forceImg (imageIn d' raw []) | (d, raw) <- take 3 cases, d' <- dictMutants d ]
  check "fuzz: decodePdfImage with damaged dictionaries" ok5
  ok6 <- allOk [ noThrow (\(r, _) -> either length (\fn -> length (show (evalFunction fn [0.3, 0.7]))) r)
                   (parseFunction (fileOf (docWith "" [(10, streamObj "/FunctionType 4 /Domain [0 1 0 1] /Range [0 1 0 1 0 1]" m)])) fuelDefault (ORef 10))
               | m <- mutants (bs "{ 2 copy add 3 1 roll mul dup 0.5 gt { exch } { 1 index } ifelse }") ]
  check "fuzz: PostScript functions, truncated and flipped" ok6

------------------------------------------------------------------------------
-- A PDF LZW encoder (for the EarlyChange round trip).

lzwEncode :: Bool -> BS.ByteString -> BS.ByteString
lzwEncode early dat = bitsOut (go (BS.unpack dat) initTable 258 9 [] [(256, 9)])
  where
    e = if early then 1 else 0 :: Int
    initTable = M.fromList [ (BS.singleton (fromIntegral i), i) | i <- [0 .. 255 :: Int] ]
    -- Codes are accumulated reversed as (code, width).
    go :: [Word8] -> M.Map BS.ByteString Int -> Int -> Int -> [Word8] -> [(Int, Int)] -> [(Int, Int)]
    go [] table nxt w cur acc
      | null cur = reverse ((257, w) : acc)
      | otherwise =
          let acc' = (table M.! BS.pack (reverse cur), w) : acc
              w' = if nxt + e >= (1 `shiftL` w) && w < 12 then w + 1 else w
          in reverse ((257, w') : acc')
    go (c : cs) table nxt w cur acc =
      let wc = BS.pack (reverse (c : cur))
      in if null cur || M.member wc table
           then go cs table nxt w (c : cur) acc
           else
             let acc' = (table M.! BS.pack (reverse cur), w) : acc
                 table' = M.insert wc nxt table
                 nxt' = nxt + 1
             in if nxt' - 1 + e >= 4095
                  then go cs initTable 258 9 [c] ((256, w) : acc')
                  else go cs table' nxt' (if nxt' - 1 + e >= (1 `shiftL` w) then w + 1 else w) [c] acc'
    bitsOut codes = BS.pack (pack8 (concat [ [ testBit v k | k <- [w - 1, w - 2 .. 0] ] | (v, w) <- codes ]))
    pack8 [] = []
    pack8 b = let (a, r) = splitAt 8 b in foldl' (\x y -> x * 2 + (if y then 1 else 0)) 0 (take 8 (a ++ repeat False)) : pack8 r
