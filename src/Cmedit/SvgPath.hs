{-# LANGUAGE BangPatterns #-}
-- | The micro-syntaxes of SVG that are not CSS: numbers and number lists,
-- path data, and transform lists. Internal to "Cmedit.Svg".
--
-- __Path data is parsed into commands, not straight into a 'Path'.__ Markers
-- are placed at path /vertices/, and an arc that becomes four cubics is still
-- one vertex with one incoming and one outgoing direction, so the arc keeps
-- its identity ('PArc') until 'cmdsToPath' flattens the list. Every command
-- is absolute by the time it leaves this module; relative forms, smooth
-- reflections and implicit repeats are resolved here once.
--
-- __Errors keep the prefix.__ SVG says a path is rendered up to (but not
-- including) the first command in error, and real files rely on it (a stray
-- trailing token is common), so the parser returns what it had rather than
-- failing. A path that does not begin with a moveto is empty.
--
-- __Numbers are hand-parsed.__ The grammar allows the compact forms
-- optimisers emit (@1.5.5-3e2@ is three numbers; arc flags need no
-- separator: @a1 1 0 00.5.5@), which no 'read' accepts, and a 4 MB icon
-- font's path data must not go through 'String'.
module Cmedit.SvgPath
  ( -- * Numbers
    parseNumber
  , parseNumberList
  , parsePoints
  , skipSep
    -- * Path data
  , PCmd(..)
  , parsePathData
  , cmdsToPath
  , arcToCubics
  , maxPathCmds
    -- * Transforms
  , parseTransform
  ) where

import Data.Char (isDigit, isSpace, ord)
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Geom

------------------------------------------------------------------------------
-- Numbers

-- | One number at the start of the text (after optional whitespace), and the
-- rest. Accepts @[+-]digits[.digits][e[+-]digits]@ and @.digits...@; an @e@
-- not followed by digits is not an exponent (so @3em@ reads as 3 then @em@).
-- Non-finite results are rejected.
parseNumber :: Text -> Maybe (Double, Text)
parseNumber t0 =
  let t = T.dropWhile isSpace t0
      (sgn, t1) = case T.uncons t of
        Just ('-', r) -> (-1, r)
        Just ('+', r) -> (1, r)
        _             -> (1 :: Double, t)
      (ip, t2) = T.span isDigit t1
      (fp, t3) = case T.uncons t2 of
        Just ('.', r) -> let (f, r') = T.span isDigit r in (f, r')
        _             -> (T.empty, t2)
  in if T.null ip && T.null fp
       then Nothing
       else
         let (ex, t4) = expo t3
             -- Up to 18 significant digits accumulate exactly in an Int; the
             -- rest only move the decimal point.
             digits = T.append ip fp
             lead   = T.dropWhile (== '0') digits
             sig    = T.take 18 lead
             dropped = T.length lead - T.length sig
             mant   = T.foldl' (\ !a c -> a * 10 + (ord c - 48)) 0 sig
             e10    = ex - T.length fp + dropped
             v      = sgn * scale10 (fromIntegral mant) e10
         in if isNaN v || isInfinite v then Nothing
            else Just (v, t4)
  where
    expo s = case T.uncons s of
      Just (e, r) | e == 'e' || e == 'E' ->
        let (es, r1) = case T.uncons r of
              Just ('-', r') -> (-1, r')
              Just ('+', r') -> (1, r')
              _              -> (1 :: Int, r)
            (ds, r2) = T.span isDigit r1
        in if T.null ds then (0, s)
           else (es * min 1000 (T.foldl' (\ !a c -> min 100000 (a * 10 + (ord c - 48))) 0 ds), r2)
      _ -> (0, s)

scale10 :: Double -> Int -> Double
scale10 m e
  | m == 0    = 0
  | e > 400   = 1 / 0
  | e < -400  = 0
  | e >= 0    = m * 10 ^^ e
  | otherwise = m / 10 ^^ negate e

-- | Skip whitespace and at most one comma (the SVG list separator).
skipSep :: Text -> Text
skipSep t = let t1 = T.dropWhile isSpace t
            in case T.uncons t1 of
                 Just (',', r) -> T.dropWhile isSpace r
                 _             -> t1

-- | A comma/whitespace separated list of numbers, stopping at the first
-- thing that is not one (what came before is kept).
parseNumberList :: Text -> [Double]
parseNumberList = go (0 :: Int)
  where
    go !n t | n >= maxPathCmds = []
            | otherwise = case parseNumber t of
                Just (v, r) -> v : go (n + 1) (skipSep r)
                Nothing     -> []

-- | A @points@ attribute: pairs of numbers. An odd trailing number is
-- dropped (SVG renders the points before it).
parsePoints :: Text -> [Pt]
parsePoints = pairs . parseNumberList
  where pairs (x : y : r) = Pt x y : pairs r
        pairs _           = []

------------------------------------------------------------------------------
-- Path data

-- | An absolute path command. 'PArc' keeps the cubics an elliptical arc
-- became together, so it stays one marker vertex; an arc that degenerates
-- to a line is a 'PLine'.
data PCmd
  = PMove !Pt
  | PLine !Pt
  | PQuad !Pt !Pt
  | PCubic !Pt !Pt !Pt
  | PArc [(Pt, Pt, Pt)] !Pt     -- ^ The cubics (control, control, end), and the end point.
  | PClose
  deriving (Eq, Show)

-- | Most commands one path may hold (after implicit repeats are expanded).
maxPathCmds :: Int
maxPathCmds = 1000000

data PS = PS
  { psCur   :: !Pt
  , psStart :: !Pt
  , psCtl   :: !Pt      -- ^ Last cubic control point (for S), or the current point.
  , psQCtl  :: !Pt      -- ^ Last quadratic control point (for T), or the current point.
  }

-- | Parse path data into absolute commands. See the module header for the
-- error rule.
parsePathData :: Text -> [PCmd]
parsePathData t0 = case T.uncons (T.dropWhile isSpace t0) of
  Just (c, r) | c == 'M' || c == 'm' -> cmd (0 :: Int) c r (PS o o o o)
  _ -> []
  where
    o = Pt 0 0
    -- After a command letter: read its argument groups (at least one, except
    -- for Z), then the next letter or an implicit repeat.
    cmd !n c t st
      | n >= maxPathCmds = []
      | c == 'Z' || c == 'z' =
          let st' = st { psCur = psStart st, psCtl = psStart st, psQCtl = psStart st }
          in PClose : next n 'Z' (T.dropWhile isSpace t) st' False
      | otherwise = case args c t of
          Nothing -> []
          Just (vals, rest) ->
            let (out, st') = apply c vals st
                -- After a moveto, further pairs are linetos.
                c' = case c of 'M' -> 'L'; 'm' -> 'l'; _ -> c
            in out ++ next (n + 1) c' (skipSep' rest) st' True
    skipSep' = T.dropWhile isSpace
    next !n c t st canRepeat = case T.uncons t of
      Nothing -> []
      Just (ch, r)
        | ch `elem` ("MmLlHhVvCcSsQqTtAaZz" :: String) -> cmd n ch r st
        | ch == ',' && canRepeat -> -- a comma between repeats
            if startsNumber (T.dropWhile isSpace r) then cmd n c (T.dropWhile isSpace r) st else []
        | canRepeat && startsNumber t -> cmd n c t st
        | otherwise -> []
    startsNumber s = case T.uncons s of
      Just (ch, _) -> isDigit ch || ch == '-' || ch == '+' || ch == '.'
      Nothing      -> False

    arity :: Char -> Int
    arity ch = case ch of
      'M' -> 2; 'm' -> 2; 'L' -> 2; 'l' -> 2; 'H' -> 1; 'h' -> 1; 'V' -> 1; 'v' -> 1
      'C' -> 6; 'c' -> 6; 'S' -> 4; 's' -> 4; 'Q' -> 4; 'q' -> 4; 'T' -> 2; 't' -> 2
      _   -> 7   -- A / a

    -- One argument group. Arcs read their two flags as single characters.
    args ch t
      | ch == 'A' || ch == 'a' = do
          (rx, t1) <- parseNumber t
          (ry, t2) <- parseNumber (skipSep t1)
          (ph, t3) <- parseNumber (skipSep t2)
          (fa, t4) <- flag (skipSep t3)
          (fs, t5) <- flag (skipSep t4)
          (x, t6)  <- parseNumber (skipSep t5)
          (y, t7)  <- parseNumber (skipSep t6)
          pure ([rx, ry, ph, fa, fs, x, y], t7)
      | otherwise = nums (arity ch) t
    nums 0 t = Just ([], t)
    nums k t = do
      (v, r) <- parseNumber t
      (vs, r') <- nums (k - 1 :: Int) (if k > 1 then skipSep r else r)
      pure (v : vs, r')
    flag t = case T.uncons (T.dropWhile isSpace t) of
      Just ('0', r) -> Just (0, r)
      Just ('1', r) -> Just (1, r)
      _             -> Nothing

    apply :: Char -> [Double] -> PS -> ([PCmd], PS)
    apply ch vs st@PS{ psCur = cur@(Pt cx cy) } =
      let rel = ch >= 'a'
          pt x y = if rel then Pt (cx + x) (cy + y) else Pt x y
          plain p = st { psCur = p, psCtl = p, psQCtl = p }
      in case (ch, vs) of
        (_, [x, y]) | ch == 'M' || ch == 'm' ->
          let p = pt x y in ([PMove p], (plain p) { psStart = p })
        (_, [x, y]) | ch == 'L' || ch == 'l' -> let p = pt x y in ([PLine p], plain p)
        (_, [x])    | ch == 'H' || ch == 'h' ->
          let p = Pt (if rel then cx + x else x) cy in ([PLine p], plain p)
        (_, [y])    | ch == 'V' || ch == 'v' ->
          let p = Pt cx (if rel then cy + y else y) in ([PLine p], plain p)
        (_, [x1, y1, x2, y2, x, y]) | ch == 'C' || ch == 'c' ->
          let c1 = pt x1 y1; c2 = pt x2 y2; p = pt x y
          in ([PCubic c1 c2 p], (plain p) { psCtl = c2 })
        (_, [x2, y2, x, y]) | ch == 'S' || ch == 's' ->
          let c1 = reflect cur (psCtl st); c2 = pt x2 y2; p = pt x y
          in ([PCubic c1 c2 p], (plain p) { psCtl = c2 })
        (_, [x1, y1, x, y]) | ch == 'Q' || ch == 'q' ->
          let c = pt x1 y1; p = pt x y
          in ([PQuad c p], (plain p) { psQCtl = c })
        (_, [x, y]) | ch == 'T' || ch == 't' ->
          let c = reflect cur (psQCtl st); p = pt x y
          in ([PQuad c p], (plain p) { psQCtl = c })
        (_, [rx, ry, ph, fa, fs, x, y]) ->
          let p = pt x y
          in if p == cur then ([], plain p)
             else case arcToCubics cur rx ry ph (fa /= 0) (fs /= 0) p of
                    Nothing -> ([PLine p], plain p)
                    Just cs -> ([PArc cs p], plain p)
        _ -> ([], st)
    -- The reflection of the previous control point about the current point
    -- (which is the current point itself when the previous command was not
    -- of the same family: psCtl/psQCtl are reset to it by 'plain').
    reflect (Pt cx cy) (Pt px py) = Pt (2 * cx - px) (2 * cy - py)

-- | Flatten commands into a 'Path'. A drawing command straight after a
-- close starts a new subpath at the closed one's start, which SVG implies
-- and the contract wants spelt out.
cmdsToPath :: [PCmd] -> Path
cmdsToPath = go Nothing Nothing False
  where
    go _ _ _ [] = []
    go _ _ _ (PMove p : r) = MoveTo p : go (Just p) (Just p) False r
    go _ st closed (c : r) = case c of
      PClose -> ClosePath : go st st True r
      _ -> let pre = if closed then maybe [] (\s -> [MoveTo s]) st else []
               (segs, end) = case c of
                 PLine p      -> ([LineTo p], p)
                 PQuad a p    -> ([QuadTo a p], p)
                 PCubic a b p -> ([CubicTo a b p], p)
                 PArc cs p    -> ([ CubicTo a b e | (a, b, e) <- cs ], p)
           in pre ++ segs ++ go (Just end) st False r

-- | An SVG elliptical arc as cubic Béziers (one per quarter turn or less),
-- or 'Nothing' when it degenerates to a straight line (a zero radius). The
-- out-of-range radii correction of SVG's implementation notes (F.6.6) is
-- applied, so every arc reaches its end point.
arcToCubics :: Pt -> Double -> Double -> Double -> Bool -> Bool -> Pt -> Maybe [(Pt, Pt, Pt)]
arcToCubics p1@(Pt x1 y1) rx0 ry0 phiDeg large sweep p2@(Pt x2 y2)
  | rx0 == 0 || ry0 == 0 = Nothing
  | p1 == p2 = Just []
  | otherwise =
      let phi = phiDeg * pi / 180
          cphi = cos phi; sphi = sin phi
          dx = (x1 - x2) / 2; dy = (y1 - y2) / 2
          x1' = cphi * dx + sphi * dy
          y1' = -sphi * dx + cphi * dy
          rxa = abs rx0; rya = abs ry0
          lam = (x1' * x1') / (rxa * rxa) + (y1' * y1') / (rya * rya)
          (rx, ry) = if lam > 1 then (sqrt lam * rxa, sqrt lam * rya) else (rxa, rya)
          num = rx * rx * ry * ry - rx * rx * y1' * y1' - ry * ry * x1' * x1'
          den = rx * rx * y1' * y1' + ry * ry * x1' * x1'
          co0 = if den == 0 then 0 else sqrt (max 0 (num / den))
          co = if large == sweep then -co0 else co0
          cx' = co * rx * y1' / ry
          cy' = -co * ry * x1' / rx
          cx = cphi * cx' - sphi * cy' + (x1 + x2) / 2
          cy = sphi * cx' + cphi * cy' + (y1 + y2) / 2
          ang (ux, uy) (vx, vy) =
            let a = atan2 (ux * vy - uy * vx) (ux * vx + uy * vy) in a
          u0 = ((x1' - cx') / rx, (y1' - cy') / ry)
          v0 = ((-x1' - cx') / rx, (-y1' - cy') / ry)
          th1 = ang (1, 0) u0
          dth0 = ang u0 v0
          dth | not sweep && dth0 > 0 = dth0 - 2 * pi
              | sweep && dth0 < 0     = dth0 + 2 * pi
              | otherwise             = dth0
          nseg = max 1 (ceiling (abs dth / (pi / 2) - 1e-9)) :: Int
          step = dth / fromIntegral nseg
          k = 4 / 3 * tan (step / 4)
          onE t = let ct = cos t; st = sin t
                  in ( Pt (cx + rx * cphi * ct - ry * sphi * st) (cy + rx * sphi * ct + ry * cphi * st)
                     , Pt (-rx * cphi * st - ry * sphi * ct) (-rx * sphi * st + ry * cphi * ct) )
          seg i =
            let ta = th1 + fromIntegral i * step
                tb = ta + step
                (pa, da) = onE ta
                (pb0, db) = onE tb
                pb = if i == nseg - 1 then p2 else pb0
            in (ptAdd pa (ptScale k da), ptSub pb (ptScale k db), pb)
      in if any bad [cx, cy, rx, ry, dth] then Nothing else Just (map seg [0 .. nseg - 1])
  where bad v = isNaN v || isInfinite v

------------------------------------------------------------------------------
-- Transforms

-- | A @transform@ list (@matrix translate scale rotate skewX skewY@,
-- separated by whitespace and/or commas). 'Nothing' for a malformed list:
-- SVG treats an invalid attribute as absent, and so does the caller.
parseTransform :: Text -> Maybe Affine
parseTransform = go identityA . T.dropWhile isSpace
  where
    go !acc t
      | T.null t = Just acc
      | otherwise = do
          let (name, r0) = T.span (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') t
          r1 <- T.stripPrefix "(" (T.dropWhile isSpace r0)
          let (inner, r2) = T.break (== ')') r1
          r3 <- T.stripPrefix ")" r2
          vs <- numsAll inner
          m <- op name vs
          let rest = T.dropWhile (\c -> isSpace c || c == ',') r3
          go (composeA acc m) rest
    op name vs = case (name, vs) of
      ("matrix", [a, b, c, d, e, f]) -> Just (Affine a b c d e f)
      ("translate", [x])             -> Just (translateA x 0)
      ("translate", [x, y])          -> Just (translateA x y)
      ("scale", [s])                 -> Just (scaleA s s)
      ("scale", [sx, sy])            -> Just (scaleA sx sy)
      ("rotate", [a])                -> Just (rotateA (rad a))
      ("rotate", [a, x, y])          -> Just (translateA x y `composeA` rotateA (rad a) `composeA` translateA (-x) (-y))
      ("skewX", [a])                 -> Just (skewXA (rad a))
      ("skewY", [a])                 -> Just (skewYA (rad a))
      _                              -> Nothing
    rad a = a * pi / 180
    -- Every number in the parentheses, or Nothing if anything else is there.
    numsAll t = let s = T.dropWhile isSpace t
                in if T.null s then Just [] else case parseNumber s of
                     Nothing     -> Nothing
                     Just (v, r) -> (v :) <$> numsAll (skipSep r)
