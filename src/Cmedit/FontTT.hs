{-# LANGUAGE BangPatterns #-}
-- | The sfnt container: TrueType (@glyf@) outlines, OpenType's @OTTO@
-- wrapper around a CFF (handed to "Cmedit.FontCFF"), and TrueType
-- collections.
--
-- Decisions that carry weight here:
--
-- * __Tables are read in place.__ @cmap@ subtables, @hmtx@, @loca@ and
--   @kern@ pairs are binary-searched or indexed straight out of the input
--   'BS.ByteString' with the total readers of "Cmedit.FontCore"; nothing is
--   copied into a map at load time. A 65 000-glyph CJK face therefore opens
--   in the time it takes to read its table directory, and a lookup is
--   a handful of byte reads.
--
-- * __A glyph is decoded to points before it is a path.__ TrueType's
--   composite glyphs may position a component by /point matching/ (anchor
--   point p1 of what is built so far meets point p2 of the component), which
--   only makes sense against the composed point list; so 'glyphContours'
--   returns contours of @(x, y, on-curve)@ points, composites concatenate
--   them, and only 'contoursToPath' turns them into quadratic segments. The
--   same list is what @glyf@'s per-glyph bounding box is defined over, which
--   is what lets the bench oracle demand exact agreement.
--
-- * __Composites are bounded twice:__ by nesting depth (a component cycle
--   would otherwise recurse forever) and by a total point budget (a wide
--   tree of large components would otherwise be exponential).
--
-- * __Component offsets are unscaled unless the font says otherwise__
--   (@SCALED_COMPONENT_OFFSET@): Microsoft's reading, which is what every
--   shipping font is built for. @ROUND_XY_TO_GRID@ is a hinting concern and
--   is ignored, since outlines here are unhinted.
module Cmedit.FontTT
  ( parseSfnt
  , sfntFaceCount
  , sfntTables
  , glyphContours
  , contoursToPath
  , ttHeaderBBox
  ) where

import Data.Bits (testBit, shiftR, (.&.), shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, ord)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Array as A
import qualified Data.Sequence as Seq

import Cmedit.Geom
import Cmedit.FontCore
import Cmedit.FontCFF (parseCFF)

------------------------------------------------------------------------------
-- Container

-- | Faces in a collection (1 for a plain sfnt, 0 for anything else).
sfntFaceCount :: BS.ByteString -> Int
sfntFaceCount bs
  | BS.take 4 bs == BC.pack "ttcf" = min 4096 (u32 bs 8)
  | isSfntTag (BS.take 4 bs) = 1
  | otherwise = 0

isSfntTag :: BS.ByteString -> Bool
isSfntTag t = t `elem` [BS.pack [0, 1, 0, 0], BC.pack "OTTO", BC.pack "true", BC.pack "typ1"]

-- | The table directory of face @k@: tag → (offset, length), every entry
-- clamped to the file.
sfntTables :: Int -> BS.ByteString -> Either String (M.Map String (Int, Int))
sfntTables k bs
  | BS.take 4 bs == BC.pack "ttcf" =
      if k < 0 || k >= sfntFaceCount bs then Left "collection face out of range"
      else dirAt (u32 bs (12 + 4 * k))
  | isSfntTag (BS.take 4 bs) = if k == 0 then dirAt 0 else Left "not a collection"
  | otherwise = Left "not an sfnt font"
  where
    len = BS.length bs
    dirAt off
      | off < 0 || off + 12 > len = Left "table directory out of range"
      | not (isSfntTag (slice off 4 bs)) = Left "bad sfnt version in collection"
      | otherwise =
          let n = min 1024 (u16 bs (off + 4))
              recs = [ (BC.unpack (slice r 4 bs), (o, min l (len - o)))
                     | i <- [0 .. n - 1]
                     , let r = off + 12 + 16 * i
                     , r + 16 <= len
                     , let o = u32 bs (r + 8); l = u32 bs (r + 12)
                     , o >= 0, o < len ]
          in Right (M.fromList recs)

-- | Parse face @k@ of an sfnt or collection.
parseSfnt :: Int -> BS.ByteString -> Either String Font
parseSfnt k bs = do
  tabs <- sfntTables k bs
  let tab t = fmap (\(o, l) -> slice o l bs) (M.lookup t tabs)
      headT = fromMaybe BS.empty (tab "head")
      maxpT = fromMaybe BS.empty (tab "maxp")
      hheaT = fromMaybe BS.empty (tab "hhea")
      hmtxT = fromMaybe BS.empty (tab "hmtx")
      nameT = fromMaybe BS.empty (tab "name")
      postT = fromMaybe BS.empty (tab "post")
      os2T  = fromMaybe BS.empty (tab "OS/2")
      cmapT = fromMaybe BS.empty (tab "cmap")
      kernT = fromMaybe BS.empty (tab "kern")
      upem0 = u16 headT 18
      upem = if upem0 >= 16 && upem0 <= 16384 then upem0 else 1000
      numG = u16 maxpT 4
      nhm = u16 hheaT 34
      advanceTT g
        | nhm <= 0 = 0
        | otherwise = fromIntegral (u16 hmtxT (4 * min (max 0 g) (nhm - 1)))
      names = nameTable nameT
      nm i = fromMaybe T.empty (lookup i names)
      family = firstNonEmpty [nm 16, nm 1]
      sub = firstNonEmpty [nm 17, nm 2]
      fsSel = u16 os2T 62
      weightC = if BS.length os2T >= 6 then u16 os2T 4 else 0
      macStyle = u16 headT 44
      styleTxt = T.toLower sub
      bold = (BS.length os2T >= 64 && testBit fsSel 5) || testBit macStyle 0
             || weightC >= 600 || "bold" `T.isInfixOf` styleTxt
      italic = (BS.length os2T >= 64 && testBit fsSel 0) || testBit macStyle 1
               || "italic" `T.isInfixOf` styleTxt || "oblique" `T.isInfixOf` styleTxt
      (asc, desc)
        | BS.length hheaT >= 8 && (i16 hheaT 4 /= 0 || i16 hheaT 6 /= 0) =
            (fromIntegral (i16 hheaT 4), fromIntegral (i16 hheaT 6))
        | BS.length os2T >= 72 = (fromIntegral (i16 os2T 68), fromIntegral (i16 os2T 70))
        | otherwise = (0.8 * fromIntegral upem, -0.2 * fromIntegral upem)
      em = fromIntegral upem :: Double
      cmaps = cmapTables numG cmapT
      postNames = postTable numG postT
      italicAngle = if BS.length postT >= 8 then fixed1616 postT 4 else 0
      fixedPitch = BS.length postT >= 16 && u32 postT 12 /= 0
      kern = kernTable kernT
      common f = f
        { fontNumGlyphs = numG
        , fontFamily = family, fontStyle = sub, fontFullName = nm 4
        , fontPSName = nm 6
        , fontBold = bold, fontItalic = italic, fontFixedPitch = fixedPitch
        , fontWeight = if weightC >= 1 && weightC <= 1000 then weightC else if bold then 700 else 400
        , fontItalicAngle = italicAngle
        , fontCmaps = cmaps
        , fnForChar = charLookup cmaps
        , fontKerning = kern
        }
  case () of
    _ | Just (go, gl) <- M.lookup "glyf" tabs, Just _ <- M.lookup "loca" tabs -> do
          let locaT = fromMaybe BS.empty (tab "loca")
              glyfT = slice go gl bs
              longLoca = i16 headT 50 /= 0
              loca g | g < 0 || g >= numG = (0, 0)
                     | longLoca = (u32 locaT (4 * g), u32 locaT (4 * g + 4))
                     | otherwise = (2 * u16 locaT (2 * g), 2 * u16 locaT (2 * g + 2))
              glyphAt g = let (a, b) = loca g
                          in if b > a && a >= 0 then slice a (b - a) glyfT else BS.empty
              byName = nameIndex postNames numG
          Right $ common (emptyFontWith FmtTrueType)
            { fontMatrix = scaleA (1 / em) (1 / em)
            , fontUnitsPerEm = em
            , fontAscent = asc / em, fontDescent = desc / em
            , fontBBox = Rect (fromIntegral (i16 headT 36)) (fromIntegral (i16 headT 38))
                              (fromIntegral (i16 headT 40)) (fromIntegral (i16 headT 42))
            , fnGlyph = \g -> contoursToPath (glyphContours glyphAt g)
            , fnAdvance = advanceTT
            , fnName = \g -> if g >= 0 && g < numG then postNames g else Nothing
            , fnByName = byName
            , fnForCID = \c -> if c >= 0 && c < numG then Just c else Nothing
            }
      | Just (co, cl) <- M.lookup "CFF " tabs -> do
          cff <- parseCFF (slice co cl bs)
          let Affine ma _ _ _ _ _ = fontMatrix cff
              -- hmtx is in sfnt units; the outlines are in CFF glyph units.
              -- They agree (1000 and 1/1000) in every font seen, but a
              -- mismatch would otherwise scale every advance.
              hscale = if ma /= 0 && nhm > 0 then 1 / (em * ma) else 1
              byPost = nameIndex postNames numG
          Right $ common cff
            { fontFormat = if fontIsCID cff then FmtCFFCID else FmtOpenTypeCFF
            , fontNumGlyphs = if numG > 0 then min numG (fontNumGlyphs cff) else fontNumGlyphs cff
            , fontAscent = asc / em, fontDescent = desc / em
            , fnAdvance = if nhm > 0 then \g -> advanceTT g * hscale else fnAdvance cff
            , fnName = \g -> case fnName cff g of
                               Nothing | g >= 0 && g < numG -> postNames g
                               r -> r
            , fnByName = \n -> case fnByName cff n of
                                 Nothing -> byPost n
                                 r -> r
            }
      | M.member "CFF2" tabs -> Left "CFF2 (variable OpenType) outlines are not supported"
      | otherwise -> Left "no outline table (bitmap-only or unsupported sfnt)"
  where
    firstNonEmpty xs = fromMaybe T.empty (listToMaybe (filter (not . T.null) xs))

------------------------------------------------------------------------------
-- glyf

-- | The declared bounding box in a glyph's @glyf@ header, if it has one
-- (for the bench oracle).
ttHeaderBBox :: BS.ByteString -> Maybe Rect
ttHeaderBBox g
  | BS.length g < 10 = Nothing
  | otherwise = Just (Rect (fromIntegral (i16 g 2)) (fromIntegral (i16 g 4))
                           (fromIntegral (i16 g 6)) (fromIntegral (i16 g 8)))

type Contour = [(Double, Double, Bool)]

maxCompositeDepth :: Int
maxCompositeDepth = 10

-- | The work one glyph may do, in points: real glyphs have hundreds, CJK
-- composites a few thousand. Visiting a component costs 'visitCost' on top
-- of its points, so at most a few thousand components are ever visited.
maxGlyphPoints, visitCost :: Int
maxGlyphPoints = 200000
visitCost = 64

-- | The contours of glyph @g@, composites resolved. @glyphAt@ returns the
-- glyph's @glyf@ record (empty for a missing or blank glyph).
glyphContours :: (Int -> BS.ByteString) -> Int -> [Contour]
glyphContours glyphAt g0 = snd (go 0 maxGlyphPoints g0)
  where
    -- Returns (points left in the budget, contours).
    go :: Int -> Int -> Int -> (Int, [Contour])
    -- Every visit costs 'visitCost' as well as its points, so a composite
    -- naming itself a thousand times cannot fan out 1000^depth ways.
    go depth budget0 g
      | depth > maxCompositeDepth || budget0 <= 0 = (max 0 budget0, [])
      | otherwise =
          let budget = budget0 - visitCost
              gb = glyphAt g
              nC = i16 gb 0
          in if BS.length gb < 10 then (budget, [])
             else if nC >= 0 then
               let cs = simpleGlyph gb nC
                   used = sum (map length cs)
               in if used > budget then (0, []) else (budget - used, cs)
             else composite depth budget gb

    -- @pts@ holds every point placed so far, in order: point matching
    -- indexes it, so it is a 'Seq' (O(log n)) rather than a list re-flattened
    -- per component, which made 1024 matched components quadratic.
    composite depth budget0 gb = loop 10 budget0 [] Seq.empty (0 :: Int)
      where
        loop off budget acc pts count
          | count >= 1024 || budget <= 0 || off + 4 > BS.length gb = (budget, concat (reverse acc))
          | otherwise =
              let flags = u16 gb off
                  cg = u16 gb (off + 2)
                  words' = testBit flags 0
                  xy = testBit flags 1
                  (a1, a2, o1)
                    | words' && xy = (i16 gb (off + 4), i16 gb (off + 6), off + 8)
                    | words'       = (u16 gb (off + 4), u16 gb (off + 6), off + 8)
                    | xy           = (sb (u8 gb (off + 4)), sb (u8 gb (off + 5)), off + 6)
                    | otherwise    = (u8 gb (off + 4), u8 gb (off + 5), off + 6)
                  (m, o2)
                    | testBit flags 3 = let s = f2dot14 gb o1 in (Affine s 0 0 s 0 0, o1 + 2)
                    | testBit flags 6 = (Affine (f2dot14 gb o1) 0 0 (f2dot14 gb (o1 + 2)) 0 0, o1 + 4)
                    | testBit flags 7 = (Affine (f2dot14 gb o1) (f2dot14 gb (o1 + 2))
                                                (f2dot14 gb (o1 + 4)) (f2dot14 gb (o1 + 6)) 0 0, o1 + 8)
                    | otherwise = (identityA, o1)
                  (budget', child) = go (depth + 1) budget cg
                  childT = map (map (\(x, y, on) -> let Pt x' y' = applyA m (Pt x y) in (x', y', on))) child
                  (dx, dy)
                    | xy = let v = Pt (fromIntegral a1) (fromIntegral a2)
                               Pt ox oy = if testBit flags 11 && not (testBit flags 12)
                                            then applyLinearA m v else v
                           in (ox, oy)
                    | otherwise =
                        -- Point matching: the parent's point a1 meets the
                        -- (transformed) component's point a2.
                        case (nth a1 pts, nth a2 (Seq.fromList (concat childT))) of
                          (Just (px, py, _), Just (cx, cy, _)) -> (px - cx, py - cy)
                          _ -> (0, 0)
                  placed = map (map (\(x, y, on) -> (x + dx, y + dy, on))) childT
                  acc' = placed : acc
                  pts' = pts Seq.>< Seq.fromList (concat placed)
              in if testBit flags 5 then loop o2 budget' acc' pts' (count + 1)
                 else (budget', concat (reverse acc'))
        sb v = if v >= 128 then v - 256 else v
        nth i xs = Seq.lookup i xs

-- | Decode a simple glyph's contours.
simpleGlyph :: BS.ByteString -> Int -> [Contour]
simpleGlyph gb nC
  | nC == 0 = []
  | not ok = []
  | otherwise = splitContours ends pts
  where
    len = BS.length gb
    ends = [ u16 gb (10 + 2 * i) | i <- [0 .. nC - 1] ]
    nPts = last ends + 1
    ok = 10 + 2 * nC <= len && and (zipWith (<) ends (tail ends)) && nPts <= 65536
    instrLen = u16 gb (10 + 2 * nC)
    flagStart = 12 + 2 * nC + instrLen
    (flags, xStart) = readFlags flagStart nPts
    (xs, yStart) = readCoords xStart flags 1 4
    (ys, _) = readCoords yStart flags 2 5
    pts = zip3 (map fromIntegral xs) (map fromIntegral ys) (map (`testBit` 0) flags)

    readFlags :: Int -> Int -> ([Int], Int)
    readFlags off0 n0 = goF off0 n0 []
      where
        goF off n acc
          | n <= 0 = (reverse acc, off)
          | off >= len = (reverse acc ++ replicate n 1, off)   -- truncated: pad on-curve
          | otherwise =
              let f = u8 gb off
              in if testBit f 3
                   then let r = min n (1 + u8 gb (off + 1))
                        in goF (off + 2) (n - r) (replicate r f ++ acc)
                   else goF (off + 1) (n - 1) (f : acc)

    readCoords :: Int -> [Int] -> Int -> Int -> ([Int], Int)
    readCoords off0 fs shortBit sameBit = goC off0 0 fs []
      where
        goC off !v [] acc = v `seq` (reverse acc, off)
        goC off !v (f : rest) acc
          | testBit f shortBit =
              let d = u8 gb off
                  v' = if testBit f sameBit then v + d else v - d
              in goC (off + 1) v' rest (v' : acc)
          | testBit f sameBit = goC off v rest (v : acc)
          | otherwise = let v' = v + i16 gb off in goC (off + 2) v' rest (v' : acc)

    splitContours es ps = goS 0 es ps
      where
        goS _ [] _ = []
        goS start (e : more) rest =
          let (c, rest') = splitAt (e - start + 1) rest
          in c : goS (e + 1) more rest'

-- | Quadratic contours to a path: consecutive off-curve points imply an
-- on-curve midpoint, and a contour that starts off-curve starts at its last
-- point (if on) or at the implied midpoint of its first and last.
contoursToPath :: [Contour] -> Path
contoursToPath = concatMap one
  where
    one [] = []
    one ps@((x0, y0, on0) : _) =
      let (xl, yl, onl) = last ps
          (start, seq')
            | on0 = (Pt x0 y0, tail ps)
            | onl = (Pt xl yl, init ps)
            | otherwise = (Pt ((x0 + xl) / 2) ((y0 + yl) / 2), ps)
      in MoveTo start : walk start Nothing seq'
    walk start pending [] = case pending of
      Just c -> [QuadTo c start, ClosePath]
      Nothing -> [ClosePath]
    walk start pending ((x, y, on) : rest)
      | on = case pending of
          Just c -> QuadTo c p : walk start Nothing rest
          Nothing -> LineTo p : walk start Nothing rest
      | otherwise = case pending of
          Just c -> QuadTo c (ptLerp 0.5 c p) : walk start (Just p) rest
          Nothing -> walk start (Just p) rest
      where p = Pt x y

------------------------------------------------------------------------------
-- cmap

cmapTables :: Int -> BS.ByteString -> [((Int, Int), Int -> Maybe Int)]
cmapTables numG t =
  [ ((pid, eid), \c -> case look c of
                          Just g | g > 0 && g < numG -> Just g
                          _ -> Nothing)
  | i <- [0 .. min 256 (u16 t 2) - 1]
  , let r = 4 + 8 * i
        pid = u16 t r
        eid = u16 t (r + 2)
        off = u32 t (r + 4)
  , off > 0, off < BS.length t
  , Just look <- [subtable (BS.drop off t)] ]

subtable :: BS.ByteString -> Maybe (Int -> Maybe Int)
subtable s = case u16 s 0 of
  0 -> Just $ \c -> if c >= 0 && c < 256 then nz (u8 s (6 + c)) else Nothing
  2 -> Just (format2 s)
  4 -> Just (format4 s)
  6 -> Just $ \c -> let first = u16 s 6; cnt = u16 s 8
                    in if c >= first && c < first + cnt then nz (u16 s (10 + 2 * (c - first))) else Nothing
  10 -> Just $ \c -> let first = u32 s 12; cnt = u32 s 16
                     in if c >= first && c < first + cnt then nz (u16 s (20 + 2 * (c - first))) else Nothing
  12 -> Just (groups False)
  13 -> Just (groups True)
  _ -> Nothing
  where
    groups constant c =
      let n = min (u32 s 12) ((BS.length s - 16) `div` 12)
          grp i = 16 + 12 * i
          search lo hi
            | lo > hi = Nothing
            | otherwise =
                let mid = (lo + hi) `div` 2
                    st = u32 s (grp mid); en = u32 s (grp mid + 4)
                in if c < st then search lo (mid - 1)
                   else if c > en then search (mid + 1) hi
                   else let g0 = u32 s (grp mid + 8)
                        in nz (if constant then g0 else g0 + (c - st))
      in search 0 (n - 1)

nz :: Int -> Maybe Int
nz 0 = Nothing
nz g = Just g

format4 :: BS.ByteString -> Int -> Maybe Int
format4 s c
  | c < 0 || c > 0xFFFF = Nothing
  | otherwise = search 0 (segN - 1)
  where
    segX2 = u16 s 6
    segN = min (segX2 `div` 2) ((BS.length s - 16) `div` 8)
    endAt i = u16 s (14 + 2 * i)
    startAt i = u16 s (16 + segX2 + 2 * i)
    deltaAt i = u16 s (16 + 2 * segX2 + 2 * i)
    roPos i = 16 + 3 * segX2 + 2 * i
    -- first segment whose end >= c
    search lo hi
      | lo > hi = if lo < segN then hit lo else Nothing
      | otherwise = let mid = (lo + hi) `div` 2
                    in if endAt mid >= c then search lo (mid - 1) else search (mid + 1) hi
    hit i
      | startAt i > c = Nothing
      | ro == 0 = nz ((c + deltaAt i) .&. 0xFFFF)
      | otherwise = case u16 s (roPos i + ro + 2 * (c - startAt i)) of
          0 -> Nothing
          g -> nz ((g + deltaAt i) .&. 0xFFFF)
      where ro = u16 s (roPos i)

format2 :: BS.ByteString -> Int -> Maybe Int
format2 s c
  | c < 0 || c > 0xFFFF = Nothing
  | hi == 0 = if key lo /= 0 then Nothing else sub 0 lo
  | otherwise = let k = key hi in if k == 0 then Nothing else sub (k `div` 8) lo
  where
    hi = c `shiftR` 8
    lo = c .&. 0xFF
    key b = u16 s (6 + 2 * b)
    sub k b =
      let sh = 6 + 512 + 8 * k
          first = u16 s sh; cnt = u16 s (sh + 2)
          delta = u16 s (sh + 4); ro = u16 s (sh + 6)
      in if b < first || b >= first + cnt then Nothing
         else case u16 s (sh + 6 + ro + 2 * (b - first)) of
                0 -> Nothing
                g -> nz ((g + delta) .&. 0xFFFF)

-- | The best Unicode lookup the font offers, then the symbol and Mac Roman
-- tables (a symbol font maps its glyphs at U+F000 + code).
charLookup :: [((Int, Int), Int -> Maybe Int)] -> Char -> Maybe Int
charLookup cmaps ch = listToMaybe (mapMaybe id attempts)
  where
    c = ord ch
    tbl k = lookup k cmaps
    uni = [ f c | k <- [(3, 10), (0, 6), (0, 4), (3, 1), (0, 3), (0, 2), (0, 1), (0, 0)]
                , Just f <- [tbl k] ]
    sym = case tbl (3, 0) of
      Just f -> [f c, f (0xF000 + c), if c >= 0xF000 && c <= 0xF0FF then f (c - 0xF000) else Nothing]
      Nothing -> []
    mac = case tbl (1, 0) of
      Just f | c < 128 -> [f c]
             | Just b <- M.lookup ch macRomanInverse -> [f b]
      _ -> []
    attempts = uni ++ sym ++ mac

macRomanInverse :: M.Map Char Int
macRomanInverse = M.fromList [ (ch, b) | (b, ch) <- A.assocs macRomanChars ]

------------------------------------------------------------------------------
-- post

-- | Glyph names from @post@ formats 1, 2 and 2.5 (format 3 has none).
postTable :: Int -> BS.ByteString -> Int -> Maybe String
postTable numG t = case u32 t 0 of
  0x00010000 -> \g -> if g >= 0 && g < 258 then Just (macGlyphNames A.! g) else Nothing
  0x00020000 ->
    let n = min numG (u16 t 32)
        strStart = 34 + 2 * n
        -- Pascal strings, parsed once on first use.
        strs = A.listArray (0, length ss - 1) ss
        ss = pascals strStart (0 :: Int)
        pascals off k
          | off >= BS.length t || k >= 65536 = []
          | otherwise = let l = u8 t off
                        in BC.unpack (slice (off + 1) l t) : pascals (off + 1 + l) (k + 1)
        nStr = length ss
    in \g -> if g < 0 || g >= n then Nothing else
               let ix = u16 t (34 + 2 * g)
               in if ix < 258 then Just (macGlyphNames A.! ix)
                  else if ix - 258 < nStr then Just (strs A.! (ix - 258)) else Nothing
  0x00025000 ->
    \g -> let n = u16 t 32
              ix = g + sb (u8 t (34 + g))
          in if g >= 0 && g < n && ix >= 0 && ix < 258 then Just (macGlyphNames A.! ix) else Nothing
  _ -> const Nothing
  where sb v = if v >= 128 then v - 256 else v

-- | Name → GID over a name function, built once on first use. The first
-- glyph with a name wins (fonts occasionally repeat a name).
nameIndex :: (Int -> Maybe String) -> Int -> String -> Maybe Int
nameIndex names numG = \n -> M.lookup n table
  where
    table = M.fromListWith (\_new old -> old)
              [ (nm, g) | g <- [0 .. numG - 1], Just nm <- [names g], not (null nm) ]

------------------------------------------------------------------------------
-- name

-- | (nameID, text), best record per ID: Windows Unicode English first, then
-- any Windows Unicode, then Unicode platform, then Mac Roman.
nameTable :: BS.ByteString -> [(Int, Text)]
nameTable t =
  [ (nid, best) | nid <- [1, 2, 4, 6, 16, 17]
  , let cands = [ (rank r, txt) | r@(_, _, _, i, _) <- recs, i == nid
                                , let txt = decodeName r, not (T.null txt) ]
  , not (null cands)
  , let best = snd (foldl1 (\a b -> if fst b < fst a then b else a) cands) ]
  where
    count = min 4096 (u16 t 2)
    strOff = u16 t 4
    recs = [ (u16 t r, u16 t (r + 2), u16 t (r + 4), u16 t (r + 6), (u16 t (r + 8), u16 t (r + 10)))
           | i <- [0 .. count - 1], let r = 6 + 12 * i, r + 12 <= BS.length t ]
    rank :: (Int, Int, Int, Int, (Int, Int)) -> Int
    rank (p, e, l, _, _)
      | p == 3 && (e == 1 || e == 10) && l == 0x409 = 0
      | p == 3 && (e == 1 || e == 10) = 1
      | p == 0 = 2
      | p == 1 && e == 0 && l == 0 = 3
      | p == 1 && e == 0 = 4
      | p == 3 && e == 0 = 5
      | otherwise = 9
    decodeName (p, e, _, _, (l, o)) =
      let raw = slice (strOff + o) l t
      in if p == 0 || p == 3 then utf16be raw
         else if p == 1 && e == 0 then T.pack (map macChar (BS.unpack raw))
         else T.empty
    macChar w = let b = fromIntegral w :: Int
                in if b < 128 then chr b else macRomanChars A.! b

utf16be :: BS.ByteString -> Text
utf16be raw = T.pack (go (units raw))
  where
    units b | BS.length b < 2 = []
            | otherwise = u16 b 0 : units (BS.drop 2 b)
    go (h : l : rest)
      | h >= 0xD800 && h < 0xDC00 && l >= 0xDC00 && l < 0xE000 =
          chr (0x10000 + ((h - 0xD800) `shiftL` 10) .|. (l - 0xDC00)) : go rest
    go (x : rest)
      | x >= 0xD800 && x < 0xE000 = '\xFFFD' : go rest
      | x == 0 = go rest
      | otherwise = chr x : go rest
    go [] = []

------------------------------------------------------------------------------
-- kern

-- | Horizontal format-0 kerning from a Microsoft-style @kern@ table, summed
-- over its subtables.
kernTable :: BS.ByteString -> Int -> Int -> Double
kernTable t
  | BS.length t < 4 || u16 t 0 /= 0 || null subs = \_ _ -> 0
  | otherwise = \l r -> fromIntegral (sum [ look s l r | s <- subs ])
  where
    subs = collect 4 (min 64 (u16 t 2))
    collect _ 0 = []
    collect off n
      | off + 6 > BS.length t = []
      | otherwise =
          let len = u16 t (off + 2)
              cov = u16 t (off + 4)
              this = [ off | cov `shiftR` 8 == 0, testBit cov 0, not (testBit cov 1), not (testBit cov 2) ]
          in this ++ (if len < 6 then [] else collect (off + len) (n - 1 :: Int))
    look off l r =
      let n = min (u16 t (off + 6)) ((BS.length t - off - 14) `div` 6)
          key = l * 65536 + r
          base = off + 14
          search lo hi
            | lo > hi = 0
            | otherwise =
                let mid = (lo + hi) `div` 2
                    k = u16 t (base + 6 * mid) * 65536 + u16 t (base + 6 * mid + 2)
                in if k == key then i16 t (base + 6 * mid + 4)
                   else if k < key then search (mid + 1) hi else search lo (mid - 1)
      in search 0 (n - 1)

