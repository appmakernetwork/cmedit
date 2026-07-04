-- | The About-box wordmark animation. Pure frame generation: given a frame
-- number and the canvas width, produce positioned styled cells for the
-- renderer to overlay on the dialog. The choreography: the capitals "CMD"
-- snake in from the left while the lowercase "edit" snakes in from the
-- right; after a beat the big D vaults up over the e and clobbers the little
-- d flat (debris and all), landing to spell the brand wordmark "CMeDit",
-- which then gets a gloss sweep. Driven by the event loop's animation tick
-- while the About dialog is open; once 'aboutTotalFrames' is reached the
-- image is static and the tick stops.
module Cmedit.About
  ( aboutCanvasH
  , aboutCanvasMinW
  , aboutTotalFrames
  , aboutTickUs
  , aboutFrameCells
  ) where

import Cmedit.Types

-- | Rows reserved at the top of the About dialog body for the animation.
aboutCanvasH :: Int
aboutCanvasH = 8

-- | Minimum inner dialog width the wordmark needs (word + entry margins).
aboutCanvasMinW :: Int
aboutCanvasMinW = 44

-- | Animation frame period (µs): ~30 fps.
aboutTickUs :: Int
aboutTickUs = 33000

-- | The frame at which the animation has fully settled; ticking past this is
-- pointless (the picture no longer changes).
aboutTotalFrames :: Int
aboutTotalFrames = 126

------------------------------------------------------------------------------
-- Timeline (frame numbers at ~30 fps)

fBeatEnd, fImpact, fSquashEnd, fDebrisEnd, fShineStart, fShineDur :: Int
fBeatEnd    = 56               -- both words parked; D takes off
fImpact     = 82               -- D lands on the little d
fSquashEnd  = fImpact + 4      -- landing squash recovers
fDebrisEnd  = fImpact + 12     -- crushed-d debris gone
fShineStart = 100              -- gloss sweep across the settled wordmark
fShineDur   = 22

------------------------------------------------------------------------------
-- Glyphs

-- A glyph: equal-width rows, top first. Only block/half-block characters
-- (single display cell each), so cell emission never needs 'contChar'.
type Glyph = [String]

glyphC, glyphM, glyphD, glyphE, glyphLD, glyphI, glyphT :: Glyph
glyphC =
  [ " ▄███▄"
  , "██▀  ▀"
  , "██    "
  , "██▄  ▄"
  , " ▀███▀" ]
glyphM =
  [ "█▄   ▄█"
  , "██▄ ▄██"
  , "██ █ ██"
  , "██   ██"
  , "██   ██" ]
glyphD =
  [ "████▄ "
  , "██  ▀█"
  , "██   █"
  , "██  ▄█"
  , "████▀ " ]
glyphE =
  [ "▄███▄"
  , "█▄▄▄▀"
  , "▀███▀" ]
glyphLD =                       -- the doomed little d
  [ "   ██"
  , "▄▄▄██"
  , "██ ██"
  , "▀▀▀██" ]
glyphI =                        -- dot level with the cap tops, gap, stem
  [ "██"
  , "  "
  , "██"
  , "██"
  , "██" ]
glyphT =
  [ " ██ "
  , "████"
  , " ██ "
  , " ▀██" ]

glyphW :: Glyph -> Int
glyphW g = case g of (r : _) -> length r; [] -> 0

------------------------------------------------------------------------------
-- Layout

-- Final wordmark "CMeDit": per-letter column offsets within the word, and the
-- row each glyph's top sits on (baseline is the canvas bottom row).
wordW :: Int
wordW = 35

oC, oM, oE, oD, oI, oT :: Int
oC = 0; oM = 7; oE = 15; oD = 21; oI = 28; oT = 31

capTop, eTop, ldTop, iTop, tTop :: Int
capTop = 3; eTop = 5; ldTop = 4; iTop = 3; tTop = 4

-- Left margin of the word within a canvas of the given width.
wordLeft :: Int -> Int
wordLeft w = max 0 ((w - wordW) `div` 2)

-- While parked (before the D flies), "CMD" sits this far left of where C and
-- M will finally rest, so the D has a runway over the e. The parked D drops
-- the e-sized hole too, so it sits a further 6 columns left of its slot.
parkShift, dParkShift :: Int
parkShift  = 8
dParkShift = 14

------------------------------------------------------------------------------
-- Colours (drawn on the dialog's white background, like 'drawAboutTitle')

blueC, redC, greyC, goldC :: Color
blueC = ColorRGB 25 80 200
redC  = ColorRGB 205 50 45
greyC = ColorRGB 110 116 128
goldC = ColorRGB 200 150 30

-- Mix a colour towards white (0 = unchanged, 1 = white).
mixWhite :: Double -> Color -> Color
mixWhite k (ColorRGB r g b) = ColorRGB (m r) (m g) (m b)
  where m v = round (fromIntegral v + (255 - fromIntegral v) * k :: Double)
mixWhite _ c = c

------------------------------------------------------------------------------
-- Easing

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

easeOutCubic :: Double -> Double
easeOutCubic p = 1 - (1 - p) ** 3

easeInOutQuad :: Double -> Double
easeInOutQuad p = if p < 0.5 then 2 * p * p else 1 - 2 * (1 - p) ** 2

------------------------------------------------------------------------------
-- Cell emission

putGlyph :: Int -> Int -> Color -> Double -> Glyph -> [((Int, Int), Cell)]
putGlyph = putGlyphWith False

-- Opaque variant: blank glyph cells are emitted too, so an airborne letter
-- occludes whatever it passes in front of instead of interleaving with it.
putGlyphOpaque :: Int -> Int -> Color -> Double -> Glyph -> [((Int, Int), Cell)]
putGlyphOpaque = putGlyphWith True

putGlyphWith :: Bool -> Int -> Int -> Color -> Double -> Glyph -> [((Int, Int), Cell)]
putGlyphWith opaque y x fg extraMix g =
  [ ((y + r, x + c), Cell ch (Style (mixWhite (shine r) fg) White attrBold))
  | (r, row) <- zip [0 ..] g
  , (c, ch)  <- zip [0 ..] row
  , opaque || ch /= ' ' ]
  where
    n = length g
    -- Subtle vertical gradient: a touch lighter towards the top.
    shine r = min 0.85 (extraMix + 0.18 * (1 - fromIntegral r / fromIntegral (max 1 (n - 1))))

------------------------------------------------------------------------------
-- Choreography

-- A word group enters rigidly (letters keep their spacing, so they can never
-- collide) while every letter rides the same damped spatial sine wave — a
-- ripple travels through the word as it moves, which is the snake.
groupProgress :: Int -> Int -> Double
groupProgress f t0 = easeOutCubic (clamp01 (fromIntegral (f - t0) / 34))

-- Lift (rows, <= 0) for letter @i@ of a group at local frame @fl@: a damped
-- sine whose phase advances with time and is offset per letter, so a ripple
-- runs through the word about twice during the entrance.
waveLift :: Double -> Double -> Int -> Int -> Int
waveLift amp damp i fl =
  negate (round (amp * damp * (0.5 + 0.5 * sin (0.9 * fromIntegral i - 0.35 * fromIntegral fl))))

-- The D's flight from its parked spot onto the little d: eased x that swings
-- a couple of columns past the target and comes back ("around"), an arc in y
-- that hangs at the apex and then drops sharply (the clobber), plus a small
-- serpentine wobble on the way up.
flightPos :: Int -> Int -> (Int, Int)
flightPos w f =
  let m  = wordLeft w
      x0 = fromIntegral (m + oD - dParkShift) :: Double
      x1 = fromIntegral (m + oD)
      s  = clamp01 (fromIntegral (f - fBeatEnd) / fromIntegral (fImpact - fBeatEnd))
      x  = x0 + easeInOutQuad s * (x1 - x0)
             + 2.2 * sin (pi * clamp01 ((s - 0.55) / 0.45))
      a  = 0.45                          -- apex position along the flight
      lift | s < a     = 3.0 * (1 - ((s - a) / a) ** 2)
           | otherwise = 3.0 * (1 - ((s - a) / (1 - a)) ** 3)   -- hangs, then drops
      wob  = 0.5 * sin (7 * s) * (1 - s)
  in (round x, capTop - round (lift + wob))

-- Post-impact shockwave: a one-row hop rippling out through the neighbours.
hopOff :: Int -> Int -> Int
hopOff f t0 = if f >= t0 && f < t0 + 3 then -1 else 0

-- Crushed-d debris: a handful of flecks thrown from the impact point, under
-- gravity, fading as they age.
debrisCells :: Int -> Int -> [((Int, Int), Cell)]
debrisCells w f
  | f < fImpact || f >= fDebrisEnd = []
  | otherwise =
      [ ((y, x), Cell ch (Style fg White attrBold))
      | (life, vx, vy, ch) <- parts
      , let dt = f - fImpact
      , dt < life
      , let t = fromIntegral dt * 0.9 :: Double
            x = wordLeft w + oD + (if vx < 0 then -1 else 6) + round (vx * t)
            y = 6 + round (vy * t + 0.22 * t * t)
      , y <= aboutCanvasH - 1
      , let fg = if dt * 2 >= life then mixWhite 0.55 greyC else greyC ]
  where
    parts =
      [ (10, -1.1, -0.9, '▪'), ( 8, -0.7, -1.2, '·'), (11, -0.3, -0.5, '·')
      , ( 9,  0.5, -1.1, '·'), (11,  1.0, -0.8, '▪'), ( 8,  1.4, -0.4, '·') ]

-- Sparkles that pop over the wordmark during the gloss sweep.
sparkleCells :: Int -> Int -> [((Int, Int), Cell)]
sparkleCells w f =
  [ ((r, wordLeft w + c), Cell '✦' (Style goldC White attrBold))
  | (c, r, t0) <- [ (3, 1, fShineStart + 3)
                  , (19, 0, fShineStart + 9)
                  , (33, 1, fShineStart + 15) ]
  , f >= t0, f < t0 + 4 ]

-- The moving gloss band: lighten any cell it covers.
applyShine :: Int -> Int -> [((Int, Int), Cell)] -> [((Int, Int), Cell)]
applyShine w f cs
  | f < fShineStart || f >= fShineStart + fShineDur = cs
  | otherwise = map light cs
  where
    p      = fromIntegral (f - fShineStart) / fromIntegral fShineDur :: Double
    center = fromIntegral (wordLeft w) - 4 + p * fromIntegral (wordW + 8)
    light cell@((r, c), Cell ch (Style fg bg at))
      | abs (fromIntegral c - center) <= 2.5 = ((r, c), Cell ch (Style (mixWhite 0.5 fg) bg at))
      | otherwise = cell

------------------------------------------------------------------------------
-- Frame assembly

-- | All non-blank cells of the animation at the given frame, positioned
-- within (and clipped to) a canvas of @w@ columns by 'aboutCanvasH' rows.
aboutFrameCells :: Int -> Int -> [((Int, Int), Cell)]
aboutFrameCells w frame =
  clip (applyShine w f (concat [trail, leftWord, rightWord, bigD, debrisCells w f, sparkleCells w f]))
  where
    f = min frame (aboutTotalFrames + 1)   -- everything is static past the end
    m = wordLeft w

    -- Shared entrance state: "CMD" comes in from the left, "edit" from the
    -- right, each as a rigid word riding the terrain wave.
    pL    = groupProgress f 0
    dampL = (1 - pL) ** 0.7
    xoffL = round (fromIntegral (m + dParkShift + glyphW glyphD + 1) * (pL - 1))
    pR    = groupProgress f 6
    dampR = (1 - pR) ** 0.7
    xoffR = round (fromIntegral (w - (m + oE)) * (1 - pR))

    -- C and M: snake in to the parked spot, then slide right into place
    -- while the D is airborne; M hops as the landing shockwave passes.
    slideP   = easeInOutQuad (clamp01 (fromIntegral (f - (fBeatEnd + 4)) / fromIntegral (fImpact - fBeatEnd - 4)))
    slideOff = round (slideP * fromIntegral parkShift) - parkShift
    leftWord = concat
      [ leftLetter oC 0 0 glyphC
      , leftLetter oM 1 (hopOff f (fImpact + 2)) glyphM ]
    leftLetter o i hop g
      | f >= fImpact  = putGlyph (capTop + hop) (m + o) blueC 0 g
      | f >= fBeatEnd = putGlyph capTop (m + o + slideOff) blueC 0 g
      | otherwise =
          let x = m + o - parkShift + xoffL
          in putGlyph (capTop + waveLift 2.0 dampL i f) x blueC 0 g

    -- e, (little d,) i, t: snake in from the right to their final spots; the
    -- little d exists only until impact; e/i/t hop as the shockwave passes.
    rightWord = concat
      [ rightLetter oE 0 eTop redC glyphE (hopOff f (fImpact + 1))
      , if f < fImpact then rightLetter oD 1 ldTop greyC glyphLD 0 else []
      , rightLetter oI 2 iTop greyC glyphI (hopOff f (fImpact + 3))
      , rightLetter oT 3 tTop redC glyphT (hopOff f (fImpact + 5)) ]
    rightLetter o i top fg g hop =
      let x   = m + o + xoffR
          amp = min 2.4 (fromIntegral top)
      in putGlyph (top + waveLift amp dampR i (f - 6) + hop) x fg 0 g

    -- The big D: parked left of the e, a flight with a ghost trail, a squash
    -- flash on landing, then at rest completing the wordmark.
    bigD
      | f < fBeatEnd  =
          let x = m + oD - dParkShift + xoffL
          in putGlyph (capTop + waveLift 2.0 dampL 2 f) x blueC 0 glyphD
      | f < fImpact   = let (x, y) = flightPos w f in putGlyphOpaque y x blueC 0 glyphD
      | f < fSquashEnd = putGlyph (capTop + 1) (m + oD) blueC 0.3 glyphD  -- squash into the floor
      | otherwise      = putGlyph capTop (m + oD) blueC 0 glyphD
    trail
      | f > fBeatEnd + 2 && f < fImpact =
          [ ((y + 2, x + 2), Cell '·' (Style (mixWhite k blueC) White attrBold))
          | (back, k) <- [(2, 0.45), (4, 0.62), (6, 0.78)]
          , f - back > fBeatEnd
          , let (x, y) = flightPos w (f - back) ]
      | otherwise = []

    clip = filter (\((r, c), _) -> r >= 0 && r < aboutCanvasH && c >= 0 && c < w)
