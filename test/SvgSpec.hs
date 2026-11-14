{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
-- | Tests for the vector stack (plan 0034): Svg.
--
-- Spec.hs calls 'svgSpec' with its own @check@, so a failure here is
-- counted and reported exactly like every other test. Fixtures are built
-- in code or checked in as bytes: the suite never shells out.
--
-- Without the rasteriser these are structural: the parsers' outputs, the
-- cascade's chosen values, and golden checks on the commands, transforms,
-- paints and bounds a document produces. Pixel comparison against resvg
-- lives in docs/plans/bench/svgcheck.sh.
module SvgSpec (svgSpec) where

import Control.Exception (SomeException, evaluate, try)
import Data.Bits (xor, shiftL, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTime)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

import Cmedit.Geom
import Cmedit.Scene
import Cmedit.Svg
import Cmedit.SvgCss
import Cmedit.SvgPath
import qualified Cmedit.Xml as X

svgSpec :: (String -> Bool -> IO ()) -> IO ()
svgSpec check = do
  numberTests check
  pathTests check
  transformTests check
  colourTests check
  cssTests check
  documentTests check
  paintTests check
  structureTests check
  textTests check
  filterTests check
  robustnessTests check

------------------------------------------------------------------------------
-- Helpers

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 1e-6

approxPt :: Pt -> Pt -> Bool
approxPt (Pt a b) (Pt c d) = approx a c && approx b d

approxA :: Affine -> Affine -> Bool
approxA (Affine a b c d e f) (Affine a' b' c' d' e' f') =
  and (zipWith approx [a, b, c, d, e, f] [a', b', c', d', e', f'])

svgDoc :: String -> BS.ByteString
svgDoc = TE.encodeUtf8 . T.pack

parse :: String -> Either String Scene
parse = parseSvg defaultSvgEnv . svgDoc

parseWith :: SvgEnv -> String -> Either String Scene
parseWith env = parseSvg env . svgDoc

-- Every drawing leaf with its accumulated transform (user → scene),
-- descending into transforms, clips and groups.
leaves :: [Cmd] -> [(Affine, Cmd)]
leaves = go identityA
  where
    go m = concatMap (one m)
    one m c = case c of
      CTransform t cs -> go (composeA m t) cs
      CClip _ cs -> go m cs
      CGroup _ cs -> go m cs
      _ -> [(composeA m (cmdA c), c)]
    cmdA c = case c of
      CFill a _ _ _ -> a; CStroke a _ _ _ -> a; CGlyphs a _ _ -> a; CImage a _ _ -> a
      CStencil a _ _ _ -> a; CGouraud a _ -> a; CGouraudRamp a _ _ -> a
      _ -> identityA

fills :: Scene -> [(Affine, FillRule, Paint, Path)]
fills sc = [ (m, r, p, path) | (m, CFill _ r p path) <- leaves (scCmds sc) ]

strokes :: Scene -> [(Affine, StrokeStyle, Paint, Path)]
strokes sc = [ (m, s, p, path) | (m, CStroke _ s p path) <- leaves (scCmds sc) ]

solidOf :: Paint -> Maybe Color
solidOf (PSolid c) = Just c
solidOf _ = Nothing

approxC :: Color -> Color -> Bool
approxC (Color a b c d) (Color a' b' c' d') = and (zipWith (\x y -> abs (x - y) < 0.002) [a, b, c, d] [a', b', c', d'])

red, green, blue :: Color
red = Color 1 0 0 1
green = Color 0 (128 / 255) 0 1
blue = Color 0 0 1 1

-- Pointer identity: the property sharing promises (equal-by-value would
-- not show that instances share one value).
sameObj :: a -> a -> Bool
sameObj a b = a `seq` b `seq` isTrue# (reallyUnsafePtrEquality# a b)

sameList :: [a] -> [a] -> Bool
sameList = sameObj

-- Every group's options anywhere in the tree.
groups :: [Cmd] -> [GroupOpts]
groups = concatMap g
  where g c = case c of
          CGroup o cs -> o : groups cs
          CTransform _ cs -> groups cs
          CClip _ cs -> groups cs
          _ -> []

clips :: [Cmd] -> [[ClipShape]]
clips = concatMap g
  where g c = case c of
          CClip ss cs -> ss : clips cs
          CGroup _ cs -> clips cs
          CTransform _ cs -> clips cs
          _ -> []

-- The scene's commands with the root viewport clip and viewBox stripped.
body :: Scene -> [Cmd]
body sc = case scCmds sc of
  [CClip _ [CTransform _ cs]] -> cs
  [CClip _ cs] -> cs
  cs -> cs

wrap :: String -> String
wrap inner = "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' width='100' height='100'>" ++ inner ++ "</svg>"

------------------------------------------------------------------------------
-- Numbers

numberTests :: (String -> Bool -> IO ()) -> IO ()
numberTests check = do
  check "svg: compact numbers 1.5.5-3e2" (parseNumberList "1.5.5-3e2" == [1.5, 0.5, -300])
  check "svg: number list with commas and signs" (parseNumberList " 1, -2 +3,.5e1 " == [1, -2, 3, 5])
  check "svg: exponent needs digits (3em is 3 then em)" (fmap fst (parseNumber "3em") == Just 3)
  check "svg: trailing junk stops a list" (parseNumberList "1 2 x 3" == [1, 2])
  check "svg: huge exponent rejected" (isNothing (parseNumber "1e999"))
  check "svg: precise decimals" (fmap fst (parseNumber "0.1") == Just 0.1 && fmap fst (parseNumber "123.456") == Just 123.456)
  check "svg: points drop an odd trailing number" (parsePoints "0,0 10,0 10" == [Pt 0 0, Pt 10 0])

------------------------------------------------------------------------------
-- Path data

pathTests :: (String -> Bool -> IO ()) -> IO ()
pathTests check = do
  check "svg path: absolute and relative h/v/z"
    (parsePathData "M10 10h5v5H0z" ==
      [PMove (Pt 10 10), PLine (Pt 15 10), PLine (Pt 15 15), PLine (Pt 0 15), PClose])
  check "svg path: implicit lineto after moveto"
    (parsePathData "M0 0 1 1 2 2" == [PMove (Pt 0 0), PLine (Pt 1 1), PLine (Pt 2 2)])
  check "svg path: relative moveto repeats are relative linetos"
    (parsePathData "m1 1 1 1" == [PMove (Pt 1 1), PLine (Pt 2 2)])
  check "svg path: implicit repeated curves"
    (length (parsePathData "M0 0C1 1 2 2 3 3 4 4 5 5 6 6") == 3)
  check "svg path: smooth cubic reflects the control point"
    (parsePathData "M0 0C0 10 10 10 10 0S20 -10 20 0" ==
      [PMove (Pt 0 0), PCubic (Pt 0 10) (Pt 10 10) (Pt 10 0), PCubic (Pt 10 (-10)) (Pt 20 (-10)) (Pt 20 0)])
  check "svg path: smooth cubic without a previous cubic uses the current point"
    (parsePathData "M0 0S5 5 10 0" == [PMove (Pt 0 0), PCubic (Pt 0 0) (Pt 5 5) (Pt 10 0)])
  check "svg path: smooth quadratic chain"
    (parsePathData "M0 0Q5 5 10 0T20 0" == [PMove (Pt 0 0), PQuad (Pt 5 5) (Pt 10 0), PQuad (Pt 15 (-5)) (Pt 20 0)])
  check "svg path: error keeps the prefix"
    (parsePathData "M0 0 L10 10 L 5" == [PMove (Pt 0 0), PLine (Pt 10 10)])
  check "svg path: must start with a moveto" (null (parsePathData "L10 10"))
  check "svg path: after close, relative is from the subpath start"
    (parsePathData "M10 10 l5 0 z l1 1" == [PMove (Pt 10 10), PLine (Pt 15 10), PClose, PLine (Pt 11 11)])
  check "svg path: close then draw starts a new subpath in the Path"
    (cmdsToPath (parsePathData "M10 10 l5 0 z l1 1") ==
      [MoveTo (Pt 10 10), LineTo (Pt 15 10), ClosePath, MoveTo (Pt 10 10), LineTo (Pt 11 11)])
  -- Arcs
  let arcEnd cs = case reverse cs of
        (PArc segs p : _) -> Just (p, case reverse segs of ((_, _, e) : _) -> Just e; _ -> Nothing)
        _ -> Nothing
  check "svg path: compact arc flags (a5 5 0 1010 0)"
    (case parsePathData "M0 0a5 5 0 1010 0" of
       [PMove _, PArc _ p] -> p == Pt 10 0
       _ -> False)
  check "svg path: arc flags with no separators before the end point (a1 1 0 00.5.5)"
    (case parsePathData "M0 0a1 1 0 00.5.5" of
       [PMove _, PArc _ p] -> approxPt p (Pt 0.5 0.5)
       _ -> False)
  check "svg path: arc's last cubic ends exactly at the end point"
    (case arcEnd (parsePathData "M3 4 A7 3 30 1 1 -2 9") of
       Just (p, Just e) -> p == e && p == Pt (-2) 9
       _ -> False)
  -- Too-small radii are scaled up: a semicircle of radius 5 through (5,±5).
  let semi = parsePathData "M0 0 A1 1 0 0 1 10 0"
      semiPts = case semi of
        [PMove _, PArc segs _] -> [ e | (_, _, e) <- segs ]
        _ -> []
  check "svg path: out-of-range radii corrected (semicircle)"
    (length semiPts == 2 && approxPt (head semiPts) (Pt 5 (-5)))
  check "svg path: sweep flag picks the side"
    (case parsePathData "M0 0 A5 5 0 0 0 10 0" of
       [PMove _, PArc ((_, _, e) : _) _] -> approxPt e (Pt 5 5)
       _ -> False)
  check "svg path: zero radius arc is a line"
    (parsePathData "M0 0 A0 5 0 0 1 10 0" == [PMove (Pt 0 0), PLine (Pt 10 0)])
  check "svg path: arc to the current point is dropped"
    (parsePathData "M1 1 A5 5 0 0 1 1 1" == [PMove (Pt 1 1)])
  check "svg path: large arc covers three quarter-turns or more"
    (case arcToCubics (Pt 0 0) 5 5 0 True True (Pt 5 5) of
       Just segs -> length segs >= 3
       Nothing -> False)
  -- A full-circle-ish arc's control points sit on the right radius.
  check "svg path: arc cubic approximates the circle"
    (case arcToCubics (Pt 10 0) 10 10 0 False True (Pt 0 10) of
       Just [(c1, c2, e)] -> approxPt e (Pt 0 10) && abs (ptLen c1 - sqrt (100 + (10 * 0.5523) ^ (2 :: Int))) < 0.01
                            && abs (ptLen c2 - sqrt (100 + (10 * 0.5523) ^ (2 :: Int))) < 0.01
       _ -> False)

------------------------------------------------------------------------------
-- Transforms

transformTests :: (String -> Bool -> IO ()) -> IO ()
transformTests check = do
  check "svg transform: translate then scale"
    (fmap (approxA (Affine 2 0 0 3 10 0)) (parseTransform "translate(10) scale(2,3)") == Just True)
  check "svg transform: commas between and inside"
    (fmap (approxA (Affine 1 0 0 1 5 6)) (parseTransform "translate(2,3),translate(3 3)") == Just True)
  check "svg transform: rotate about a point"
    (case parseTransform "rotate(90 10 10)" of
       Just m -> approxPt (applyA m (Pt 20 10)) (Pt 10 20)
       Nothing -> False)
  check "svg transform: skewX(45)"
    (case parseTransform "skewX(45)" of
       Just m -> approxPt (applyA m (Pt 0 1)) (Pt 1 1)
       Nothing -> False)
  check "svg transform: matrix passes through"
    (parseTransform "matrix(1 2 3 4 5 6)" == Just (Affine 1 2 3 4 5 6))
  check "svg transform: malformed is Nothing"
    (isNothing (parseTransform "translate(1,2") && isNothing (parseTransform "scale()")
      && isNothing (parseTransform "frob(1)") && isNothing (parseTransform "scale(1 x)"))
  check "svg transform: empty is identity" (parseTransform "  " == Just identityA)

------------------------------------------------------------------------------
-- Colours

colourTests :: (String -> Bool -> IO ()) -> IO ()
colourTests check = do
  let col t = case parseColor t of Just (CSColor c) -> Just c; _ -> Nothing
      is t c = maybe False (approxC c) (col t)
  check "svg colour: #rgb" (is "#f00" red)
  check "svg colour: #rgba" (is "#f008" (Color 1 0 0 (136 / 255)))
  check "svg colour: #rrggbb" (is "#0000FF" blue)
  check "svg colour: #rrggbbaa" (is "#ff000080" (Color 1 0 0 (128 / 255)))
  check "svg colour: rgb() numbers" (is "rgb(255, 0, 0)" red)
  check "svg colour: rgb() percentages" (is "rgb(100%, 0%, 0%)" red)
  check "svg colour: CSS4 space form with slash alpha" (is "rgb(255 0 0 / 50%)" (Color 1 0 0 0.5))
  check "svg colour: rgba()" (is "rgba(0,0,255,.5)" (Color 0 0 1 0.5))
  check "svg colour: rgba with percentage alpha" (is "rgba(0,0,255,25%)" (Color 0 0 1 0.25))
  check "svg colour: hsl()" (is "hsl(120, 100%, 50%)" (Color 0 1 0 1))
  check "svg colour: hsla() space form" (is "hsla(240 100% 50% / 0.25)" (Color 0 0 1 0.25))
  check "svg colour: hsl hue in turns" (is "hsl(0.5turn 100% 50%)" (Color 0 1 1 1))
  check "svg colour: names are case-insensitive" (is "RED" red && is "LightGoldenRodYellow" (Color (250/255) (250/255) (210/255) 1))
  check "svg colour: grey spellings" (is "darkslategrey" (Color (47/255) (79/255) (79/255) 1) && is "gray" (Color (128/255) (128/255) (128/255) 1))
  check "svg colour: transparent" (is "transparent" (Color 0 0 0 0))
  check "svg colour: rebeccapurple" (is "rebeccapurple" (Color 0.4 0.2 0.6 1))
  check "svg colour: currentColor" (parseColor "currentColor" == Just CSCurrent)
  check "svg colour: ICC suffix ignored" (is "#00f icc-color(foo, 0.1, 0.2)" blue)
  check "svg colour: invalid forms" (all (isNothing . parseColor) ["#ff", "foo", "rgb(1,2)", "#ggg", "rgb(1,2,3"])
  check "svg colour: all 147 SVG names present"
    (all (isJust . namedColor) (T.words allNames) && length (T.words allNames) == 147)
  where
    allNames = "aliceblue antiquewhite aqua aquamarine azure beige bisque black blanchedalmond blue blueviolet brown burlywood cadetblue chartreuse chocolate coral cornflowerblue cornsilk crimson cyan darkblue darkcyan darkgoldenrod darkgray darkgreen darkgrey darkkhaki darkmagenta darkolivegreen darkorange darkorchid darkred darksalmon darkseagreen darkslateblue darkslategray darkslategrey darkturquoise darkviolet deeppink deepskyblue dimgray dimgrey dodgerblue firebrick floralwhite forestgreen fuchsia gainsboro ghostwhite gold goldenrod gray grey green greenyellow honeydew hotpink indianred indigo ivory khaki lavender lavenderblush lawngreen lemonchiffon lightblue lightcoral lightcyan lightgoldenrodyellow lightgray lightgreen lightgrey lightpink lightsalmon lightseagreen lightskyblue lightslategray lightslategrey lightsteelblue lightyellow lime limegreen linen magenta maroon mediumaquamarine mediumblue mediumorchid mediumpurple mediumseagreen mediumslateblue mediumspringgreen mediumturquoise mediumvioletred midnightblue mintcream mistyrose moccasin navajowhite navy oldlace olive olivedrab orange orangered orchid palegoldenrod palegreen paleturquoise palevioletred papayawhip peachpuff peru pink plum powderblue purple red rosybrown royalblue saddlebrown salmon sandybrown seagreen seashell sienna silver skyblue slateblue slategray slategrey snow springgreen steelblue tan teal thistle tomato turquoise violet wheat white whitesmoke yellow yellowgreen"

------------------------------------------------------------------------------
-- CSS

cssTests :: (String -> Bool -> IO ()) -> IO ()
cssTests check = do
  let spec t = selectorSpecificity <$> parseSelector t
  check "svg css: specificity #a .b c" (spec "#a .b c" == Just (1, 1, 1))
  check "svg css: specificity of attribute and pseudo-class" (spec "rect[x]:first-child" == Just (0, 2, 1))
  check "svg css: universal has no specificity" (spec "*" == Just (0, 0, 0))
  check "svg css: bad selector rejected" (isNothing (parseSelector "a ! b") && isNothing (parseSelector ".") )
  check "svg css: declarations with !important and url;base64"
    (parseDecls "fill: red !important; stroke:url(data:a;b) ; ; bad" ==
      [Decl "fill" "red" True, Decl "stroke" "url(data:a;b)" False])
  check "svg css: comments and at-rules skipped"
    (length (parseSheet 0 "/* x */ @import url(a.css); @media screen { rect { fill: red } } rect { fill: blue }") == 1)
  check "svg css: a selector list splits into rules"
    (length (parseSheet 0 "a, b, c { fill: red }") == 3)
  check "svg css: one bad selector drops the rule" (null (parseSheet 0 "a, b!c { fill: red }"))
  -- The cascade, end to end.
  let fillOf src = case parse (wrap src) of
        Right sc -> [ c | (_, _, PSolid c, _) <- fills sc ]
        Left _ -> []
      sheet = "<style>rect { fill: blue } .c { fill: green } #i { fill: red } g > rect.k { fill: yellow }</style>"
  check "svg cascade: type < class < id"
    (fillOf (sheet ++ "<rect width='1' height='1'/><rect class='c' width='1' height='1'/><rect id='i' class='c' width='1' height='1'/>")
      == [blue, green, red])
  check "svg cascade: presentation attribute loses to the sheet"
    (fillOf (sheet ++ "<rect fill='red' width='1' height='1'/>") == [blue])
  check "svg cascade: style attribute beats the sheet"
    (fillOf (sheet ++ "<rect id='i' style='fill:blue' width='1' height='1'/>") == [blue])
  check "svg cascade: !important in the sheet beats the style attribute"
    (fillOf ("<style>rect { fill: green !important }</style><rect style='fill:red' width='1' height='1'/>") == [green])
  check "svg cascade: child combinator"
    (fillOf (sheet ++ "<g><rect class='k' width='1' height='1'/></g><g><a><rect class='k' width='1' height='1'/></a></g>")
      == [Color 1 1 0 1, blue])
  check "svg cascade: later rule of equal specificity wins"
    (fillOf "<style>.a { fill: red } .b { fill: blue }</style><rect class='b a' width='1' height='1'/>" == [blue])
  check "svg cascade: invalid value falls back to the next declaration"
    (fillOf "<rect fill='blue' style='fill: notacolour' width='1' height='1'/>" == [blue])
  check "svg cascade: inheritance through groups"
    (fillOf "<g fill='green'><g><rect width='1' height='1'/></g></g>" == [green])
  check "svg cascade: inherit keyword on a non-inherited property"
    (case parse (wrap "<g opacity='0.5'><rect opacity='inherit' fill='red' width='1' height='1'/></g>") of
       Right sc -> case fills sc of
         [(_, _, PSolid (Color _ _ _ a), _)] -> approx a 0.25
         _ -> False
       Left _ -> False)
  check "svg cascade: :first-child and attribute selectors"
    (fillOf "<style>rect:first-child { fill: red } [data-x=\"1\"] { fill: blue }</style><g><rect width='1' height='1'/><rect data-x='1' width='1' height='1'/></g>"
      == [red, blue])
  check "svg cascade: :hover never matches"
    (fillOf "<style>rect:hover { fill: red }</style><rect width='1' height='1'/>" == [Color 0 0 0 1])
  check "svg cascade: CDATA style sheet"
    (fillOf "<style><![CDATA[ rect > x, rect { fill: green } ]]></style><rect width='1' height='1'/>" == [green])
  check "svg cascade: currentColor follows the element's own color"
    (fillOf "<g fill='currentColor' color='red'><rect color='blue' width='1' height='1'/></g>" == [blue])
  check "svg cascade: the marker shorthand"
    (case parse (wrap "<style>path { marker: url(#m) }</style><marker id='m'><rect width='1' height='1' fill='red'/></marker><path d='M0 0 L10 0 L20 0' stroke='none' fill='none'/>") of
       Right sc -> length (fills sc) == 3
       Left _ -> False)

------------------------------------------------------------------------------
-- Document, units and viewports

documentTests :: (String -> Bool -> IO ()) -> IO ()
documentTests check = do
  let boundsOf s = scBounds <$> parse s
  check "svg size: width/height" (boundsOf (wrap "") == Right (Rect 0 0 100 100))
  check "svg size: units" (boundsOf "<svg xmlns='http://www.w3.org/2000/svg' width='1in' height='72pt'/>" == Right (Rect 0 0 96 96))
  check "svg size: viewBox only" (boundsOf "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 48 24'/>" == Right (Rect 0 0 48 24))
  check "svg size: width with viewBox aspect" (boundsOf "<svg xmlns='http://www.w3.org/2000/svg' width='96' viewBox='0 0 48 24'/>" == Right (Rect 0 0 96 48))
  check "svg size: nothing uses the environment viewport" (boundsOf "<svg xmlns='http://www.w3.org/2000/svg'/>" == Right (Rect 0 0 100 100))
  check "svg size: zero area is Left" (either (const True) (const False) (parse "<svg xmlns='http://www.w3.org/2000/svg' width='0' height='10'/>"))
  check "svg size: not SVG is Left" (either (const True) (const False) (parse "<html><body/></html>"))
  check "svg intrinsic size" (svgIntrinsicSize (svgDoc "<svg xmlns='http://www.w3.org/2000/svg' width='48' height='32'/>") == Just (48, 32)
                              && isNothing (svgIntrinsicSize (svgDoc "<svg xmlns='http://www.w3.org/2000/svg'/>")))
  check "svg sniff" (sniffSvg (svgDoc "<?xml version='1.0'?><!-- c --><svg/>") && not (sniffSvg (svgDoc "<html/>")))
  -- viewBox + preserveAspectRatio
  let rootM s = case parse s of
        Right sc -> case scCmds sc of
          [CClip _ [CTransform m _]] -> Just m
          _ -> Nothing
        Left _ -> Nothing
      vbDoc par = "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='50' viewBox='0 0 10 10' preserveAspectRatio='" ++ par ++ "'><rect width='1' height='1'/></svg>"
  check "svg viewBox: xMidYMid meet" (fmap (approxA (Affine 5 0 0 5 25 0)) (rootM (vbDoc "xMidYMid meet")) == Just True)
  check "svg viewBox: xMinYMin meet" (fmap (approxA (Affine 5 0 0 5 0 0)) (rootM (vbDoc "xMinYMin")) == Just True)
  check "svg viewBox: xMaxYMax meet" (fmap (approxA (Affine 5 0 0 5 50 0)) (rootM (vbDoc "xMaxYMax")) == Just True)
  check "svg viewBox: xMidYMid slice" (fmap (approxA (Affine 10 0 0 10 0 (-25))) (rootM (vbDoc "xMidYMid slice")) == Just True)
  check "svg viewBox: xMinYMax slice" (fmap (approxA (Affine 10 0 0 10 0 (-50))) (rootM (vbDoc "xMinYMax slice")) == Just True)
  check "svg viewBox: none stretches" (fmap (approxA (Affine 10 0 0 5 0 0)) (rootM (vbDoc "none")) == Just True)
  check "svg viewBox: offset origin"
    (fmap (approxA (Affine 1 0 0 1 (-5) (-5))) (rootM "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10' viewBox='5 5 10 10'><rect width='1' height='1'/></svg>") == Just True)
  -- Units and percentages
  let rectOf s = case parse s of
        Right sc -> case fills sc of
          ((_, _, _, path) : _) -> pathBounds path
          _ -> Nothing
        Left _ -> Nothing
  check "svg units: x vs y percentages"
    (rectOf "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100'><rect x='10%' y='10%' width='50%' height='50%'/></svg>" == Just (Rect 20 10 120 60))
  check "svg units: percentages use the viewBox"
    (rectOf "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100' viewBox='0 0 20 10'><rect width='50%' height='50%'/></svg>" == Just (Rect 0 0 10 5))
  check "svg units: mm, cm, pc"
    (fmap (\(Rect a b c d) -> map round [a, b, c, d]) (rectOf (wrap "<rect x='25.4mm' y='2.54cm' width='1pc' height='12pt'/>")) == Just [96, 96, 112, 112 :: Int])
  check "svg units: em and ex follow font-size"
    (rectOf (wrap "<g font-size='10'><rect width='2em' height='2ex' font-size='20'/></g>") == Just (Rect 0 0 40 20))
  check "svg units: circle r percentage uses the normalised diagonal"
    (case rectOf "<svg xmlns='http://www.w3.org/2000/svg' width='300' height='400'><circle r='10%'/></svg>" of
       Just (Rect x0 _ x1 _) -> abs ((x1 - x0) / 2 - 0.1 * sqrt ((300 * 300 + 400 * 400) / 2)) < 1e-6
       Nothing -> False)
  check "svg units: stroke-width percentage"
    (case parse "<svg xmlns='http://www.w3.org/2000/svg' width='300' height='400'><line x2='10' stroke='red' stroke-width='1%'/></svg>" of
       Right sc -> case strokes sc of
         [(_, ss, _, _)] -> approx (ssWidth ss) (0.01 * sqrt ((300 * 300 + 400 * 400) / 2))
         _ -> False
       Left _ -> False)
  -- Namespaces, entities, encodings, gzip
  check "svg xml: Illustrator entity for the namespace"
    (either (const False) (not . null . fills) (parse ("<!DOCTYPE svg [<!ENTITY ns_svg \"http://www.w3.org/2000/svg\">]>"
       ++ "<svg xmlns='&ns_svg;' width='10' height='10'><rect width='1' height='1'/></svg>")))
  check "svg xml: internal entities in Cmedit.Xml"
    (X.parseXml "<!DOCTYPE a [<!ENTITY x \"why\">]><a b='&x;'>&x;!</a>" ==
      [X.XStart "a" [("b", "why")], X.XText "why!", X.XEnd "a"])
  check "svg xml: entity expansion is bounded"
    (let big = replicate 1000 'x'
         doc = "<!DOCTYPE a [<!ENTITY x \"" ++ big ++ "\">]><a>" ++ concat (replicate 5000 "&x;") ++ "</a>"
     in sum [ T.length t | X.XText t <- X.parseXml (T.pack doc) ] <= X.maxXmlTextChars + 1000)
  -- Review item 2: one level of expansion is still amplification, so it is
  -- metered per attribute and per document.
  let ent = replicate (64 * 1024) 'x'
      refs = concat (replicate 17 "&e;")
      ampDoc = "<!DOCTYPE a [<!ENTITY e \"" ++ ent ++ "\">]><a>"
               ++ concat (replicate 300 ("<g id=\"" ++ refs ++ "\"/>")) ++ "</a>"
      attrChars = sum [ T.length v | X.XStart _ as <- X.parseXmlQualified (T.pack ampDoc), (_, v) <- as ]
  check "svg xml: an attribute grows by at most maxXmlAttrExpansion"
    (all (\v -> T.length v <= 17 * 3 + X.maxXmlAttrExpansion)
         [ v | X.XStart _ as <- take 20 (X.parseXml (T.pack ampDoc)), (_, v) <- as ])
  check "svg xml: entity expansion has a document-wide budget"
    (attrChars <= X.maxEntityExpansion + 300 * 17 * 3)
  check "svg xml: past the budget, references are left as written"
    (case reverse [ v | X.XStart _ as <- X.parseXml (T.pack ampDoc), (_, v) <- as ] of
       (v : _) -> "&e;" `T.isInfixOf` v
       [] -> False)
  check "svg xml: text nodes share the budget"
    (let doc = "<!DOCTYPE a [<!ENTITY e \"" ++ ent ++ "\">]><a>" ++ concat (replicate 300 ("<b>" ++ refs ++ "</b>")) ++ "</a>"
     in sum [ T.length t | X.XText t <- X.parseXml (T.pack doc) ] <= X.maxEntityExpansion + 300 * 17 * 3)
  check "svg xml: a long attribute without entities is kept whole"
    (let d = concat (replicate 30000 "l1 1 ") in
       [ T.length v | X.XStart _ as <- X.parseXml (T.pack ("<p d='" ++ d ++ "'/>")), (_, v) <- as ] == [length d])
  check "svg xml: qualified names kept by parseXmlQualified"
    (X.parseXmlQualified "<a:b xlink:href='x'/>" == [X.XStart "a:b" [("xlink:href", "x")], X.XEnd "a:b"])
  check "svg xml: foreign attributes do not answer for SVG ones"
    (case parse (wrap "<circle xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' sodipodi:cx='50' cx='10' r='1'/>") of
       Right sc -> case fills sc of
         [(_, _, _, path)] -> fmap (\(Rect x0 _ x1 _) -> (x0 + x1) / 2) (pathBoundsExact path) == Just 10
         _ -> False
       Left _ -> False)
  check "svg xml: foreign elements are skipped"
    (either (const False) ((== 1) . length . fills) (parse (wrap "<x:rect xmlns:x='urn:other' width='5' height='5'/><rect width='1' height='1'/>")))
  check "svg xml: xlink:href and href (href wins)"
    (case parse (wrap "<defs><rect id='a' width='1' height='1' fill='red'/><rect id='b' width='1' height='1' fill='blue'/></defs><use xlink:href='#a' href='#b'/>") of
       Right sc -> map (\(_, _, p, _) -> solidOf p) (fills sc) == [Just blue]
       Left _ -> False)
  let utf16le s = BS.pack (0xff : 0xfe : concatMap (\c -> [fromIntegral (fromEnum c), 0]) s)
  check "svg encoding: UTF-16 with BOM"
    (either (const False) ((== 1) . length . fills) (parseSvg defaultSvgEnv (utf16le (wrap "<rect width='1' height='1'/>"))))
  check "svg encoding: UTF-16 without BOM"
    (either (const False) ((== 1) . length . fills) (parseSvg defaultSvgEnv (BS.drop 2 (utf16le (wrap "<rect width='1' height='1'/>")))))
  check "svg encoding: UTF-8 BOM"
    (either (const False) ((== 1) . length . fills) (parseSvg defaultSvgEnv (BS.append (BS.pack [0xef, 0xbb, 0xbf]) (svgDoc (wrap "<rect width='1' height='1'/>")))))
  check "svg gzip: .svgz (stored deflate block)"
    (either (const False) ((== 1) . length . fills) (parseSvg defaultSvgEnv (gzipStored (svgDoc (wrap "<rect width='1' height='1'/>")))))
  check "svg gzip: name field skipped"
    (either (const False) ((== 1) . length . fills) (parseSvg defaultSvgEnv (gzipStoredNamed (svgDoc (wrap "<rect width='1' height='1'/>")))))
  check "svg gzip: truncated stream is Left, not an exception"
    (either (const True) (const False) (parseSvg defaultSvgEnv (BS.take 12 (gzipStored (svgDoc (wrap ""))))))

-- A gzip member holding one stored (uncompressed) DEFLATE block. The CRC is
-- not checked by the reader, so it is left zero.
gzipStored :: BS.ByteString -> BS.ByteString
gzipStored d = BS.concat [BS.pack [0x1f, 0x8b, 8, 0, 0, 0, 0, 0, 0, 3], storedBlock d, BS.pack [0, 0, 0, 0, 0, 0, 0, 0]]

gzipStoredNamed :: BS.ByteString -> BS.ByteString
gzipStoredNamed d = BS.concat [BS.pack [0x1f, 0x8b, 8, 8, 0, 0, 0, 0, 0, 3], BC.pack "icon.svg\0", storedBlock d, BS.pack [0, 0, 0, 0, 0, 0, 0, 0]]

storedBlock :: BS.ByteString -> BS.ByteString
storedBlock d =
  let n = BS.length d
      lo x = fromIntegral (x .&. 255); hi x = fromIntegral ((x `div` 256) .&. 255)
      nn = 65535 - n
  in BS.concat [BS.pack [1, lo n, hi n, lo nn, hi nn], d]

------------------------------------------------------------------------------
-- Paints

paintTests :: (String -> Bool -> IO ()) -> IO ()
paintTests check = do
  let firstFill s = case parse (wrap s) of
        Right sc -> case fills sc of ((_, _, p, _) : _) -> Just p; [] -> Nothing
        Left _ -> Nothing
  check "svg paint: none draws nothing" (isNothing (firstFill "<rect fill='none' width='1' height='1'/>"))
  check "svg paint: missing server uses the fallback"
    (fmap solidOf (firstFill "<rect fill='url(#nope) red' width='1' height='1'/>") == Just (Just red))
  check "svg paint: missing server without fallback is none"
    (isNothing (firstFill "<rect fill='url(#nope)' width='1' height='1'/>"))
  check "svg paint: fill-opacity folds into a solid colour"
    (fmap solidOf (firstFill "<rect fill='red' fill-opacity='0.25' width='1' height='1'/>") == Just (Just (Color 1 0 0 0.25)))
  check "svg paint: element opacity on one leaf folds (no group)"
    (case parse (wrap "<rect fill='red' opacity='0.5' width='1' height='1'/>") of
       Right sc -> null (groups (scCmds sc)) && map (\(_, _, p, _) -> solidOf p) (fills sc) == [Just (Color 1 0 0 0.5)]
       Left _ -> False)
  check "svg paint: opacity on fill+stroke groups"
    (case parse (wrap "<rect fill='red' stroke='blue' opacity='0.5' width='1' height='1'/>") of
       Right sc -> map goOpacity (groups (scCmds sc)) == [0.5]
       Left _ -> False)
  -- Gradients
  let lg = "<linearGradient id='g' x1='0' y1='0' x2='1' y2='0'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient>"
  check "svg gradient: objectBoundingBox maps onto the bbox"
    (case firstFill (lg ++ "<rect x='10' y='20' width='30' height='40' fill='url(#g)'/>") of
       Just (PLinear m p0 p1 [Stop 0 c0, Stop 1 c1] SpreadPad) ->
         approxA m (Affine 30 0 0 40 10 20) && p0 == Pt 0 0 && p1 == Pt 1 0 && c0 == red && c1 == blue
       _ -> False)
  check "svg gradient: userSpaceOnUse with percentages and a transform"
    (case firstFill ("<linearGradient id='g' gradientUnits='userSpaceOnUse' x1='10%' x2='50' gradientTransform='rotate(90)' spreadMethod='reflect'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient>"
                    ++ "<rect width='10' height='10' fill='url(#g)'/>") of
       Just (PLinear m p0 p1 _ SpreadReflect) -> approxA m (Affine 0 1 (-1) 0 0 0) && p0 == Pt 10 0 && p1 == Pt 50 0
       _ -> False)
  check "svg gradient: href chain inherits stops and attributes"
    (case firstFill (lg ++ "<linearGradient id='h' href='#g' spreadMethod='repeat' x2='0.5'/><rect width='10' height='10' fill='url(#h)'/>") of
       Just (PLinear _ _ (Pt 0.5 0) [_, _] SpreadRepeat) -> True
       _ -> False)
  check "svg gradient: self-referencing href terminates"
    (isJust (firstFill "<linearGradient id='g' href='#g'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient><rect width='1' height='1' fill='url(#g)'/>"))
  check "svg gradient: offsets clamped and monotonic"
    (case firstFill "<linearGradient id='g'><stop offset='50%' stop-color='red'/><stop offset='0.2' stop-color='blue'/><stop offset='2' stop-color='lime'/></linearGradient><rect width='1' height='1' fill='url(#g)'/>" of
       Just (PLinear _ _ _ ss _) -> [ o | Stop o _ <- ss ] == [0.5, 0.5, 1]
       _ -> False)
  check "svg gradient: one stop is a solid colour"
    (fmap solidOf (firstFill "<linearGradient id='g'><stop stop-color='blue'/></linearGradient><rect width='1' height='1' fill='url(#g)'/>") == Just (Just blue))
  check "svg gradient: no stops is none"
    (isNothing (firstFill "<linearGradient id='g'/><rect width='1' height='1' fill='url(#g)'/>"))
  -- Review item 12: an unusable server falls back to the colour (resvg and
  -- librsvg agree), whether the reference is missing, has no stops, or is in
  -- objectBoundingBox units on a zero-height bounding box.
  check "svg paint: unusable server uses the fallback (no stops)"
    (fmap solidOf (firstFill "<linearGradient id='g'/><rect width='1' height='1' fill='url(#g) red'/>") == Just (Just red))
  let strokeOf src = case parse (wrap src) of
        Right sc -> [ solidOf p | (_, _, p, _) <- strokes sc ]
        Left _ -> []
  check "svg paint: horizontal line with an objectBoundingBox gradient uses the fallback"
    (strokeOf (lg ++ "<line x1='10' y1='10' x2='90' y2='10' stroke='url(#g) blue' stroke-width='6'/>") == [Just blue])
  check "svg paint: horizontal line with an objectBoundingBox gradient and no fallback draws nothing"
    (null (strokeOf (lg ++ "<line x1='10' y1='10' x2='90' y2='10' stroke='url(#g)' stroke-width='6'/>")))
  check "svg paint: empty pattern tile uses the fallback"
    (fmap solidOf (firstFill "<pattern id='p' width='0' height='1'><rect width='1' height='1'/></pattern><rect width='1' height='1' fill='url(#p) red'/>") == Just (Just red))
  check "svg gradient: stop-opacity and fill-opacity multiply"
    (case firstFill "<linearGradient id='g'><stop offset='0' stop-color='red' stop-opacity='0.5'/><stop offset='1' stop-color='blue'/></linearGradient><rect width='1' height='1' fill='url(#g)' fill-opacity='0.5'/>" of
       Just (PLinear _ _ _ [Stop _ (Color _ _ _ a0), Stop _ (Color _ _ _ a1)] _) -> approx a0 0.25 && approx a1 0.5
       _ -> False)
  check "svg gradient: zero-area bbox with objectBoundingBox paints nothing"
    (isNothing (firstFill (lg ++ "<rect width='10' height='0.0' fill='url(#g)'/><path d='M0 0 L10 0' fill='url(#g)'/>")))
  check "svg gradient: radial focal point and radius"
    (case firstFill "<radialGradient id='r' cx='0.5' cy='0.5' r='0.5' fx='0.25' fr='0.1'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='blue'/></radialGradient><rect width='10' height='10' fill='url(#r)'/>" of
       Just (PRadial m f fr c r _ _) -> approxA m (Affine 10 0 0 10 0 0) && f == Pt 0.25 0.5 && approx fr 0.1 && c == Pt 0.5 0.5 && approx r 0.5
       _ -> False)
  check "svg gradient: stop-color currentColor"
    (case firstFill "<linearGradient id='g' color='lime'><stop offset='0' stop-color='currentColor'/><stop offset='1' stop-color='blue'/></linearGradient><rect width='1' height='1' fill='url(#g)'/>" of
       Just (PLinear _ _ _ (Stop _ c : _) _) -> c == Color 0 1 0 1
       _ -> False)
  -- Patterns
  check "svg pattern: tile cell, steps and content"
    (case firstFill "<pattern id='p' width='0.5' height='0.25'><rect width='2' height='2' fill='red'/></pattern><rect x='0' y='0' width='20' height='40' fill='url(#p)'/>" of
       Just (PTiling k m cell xs ys cmds) -> k >= 0 && m == identityA && cell == Rect 0 0 10 10 && xs == 10 && ys == 10 && length cmds == 1
       _ -> False)
  check "svg pattern: viewBox and patternTransform"
    (case firstFill "<pattern id='p' patternUnits='userSpaceOnUse' x='5' width='10' height='10' viewBox='0 0 1 1' patternTransform='scale(2)'><rect width='1' height='1' fill='red'/></pattern><rect width='20' height='40' fill='url(#p)'/>" of
       Just (PTiling _ m cell _ _ [CTransform cm _]) -> m == scaleA 2 2 && cell == Rect 5 0 15 10 && approxA cm (Affine 10 0 0 10 5 0)
       _ -> False)
  check "svg pattern: recursive pattern does not hang"
    (isJust (Just (either (const 0) (length . show) (parse (wrap "<pattern id='p' width='1' height='1'><rect width='1' height='1' fill='url(#p)'/></pattern><rect width='10' height='10' fill='url(#p)'/>")))))
  check "svg pattern: translucent pattern fill is grouped"
    (case parse (wrap "<pattern id='p' width='1' height='1'><rect width='1' height='1' fill='red'/></pattern><rect width='10' height='10' fill='url(#p)' fill-opacity='0.5'/>") of
       Right sc -> 0.5 `elem` map goOpacity (groups (body sc))
       Left _ -> False)

------------------------------------------------------------------------------
-- Structure: use, symbol, svg, switch, clip, mask, markers, images

structureTests :: (String -> Bool -> IO ()) -> IO ()
structureTests check = do
  let sc s = either (const Nothing) Just (parse (wrap s))
  check "svg use: x/y translate the instance"
    (case sc "<defs><rect id='r' width='2' height='2'/></defs><use href='#r' x='10' y='20'/>" of
       Just s -> case fills s of [(m, _, _, _)] -> approxA m (translateA 10 20); _ -> False
       Nothing -> False)
  check "svg use: transform applies before the x/y translate"
    (case sc "<defs><rect id='r' width='2' height='2'/></defs><use href='#r' x='10' transform='scale(2)'/>" of
       Just s -> case fills s of [(m, _, _, _)] -> approxA m (Affine 2 0 0 2 20 0); _ -> False
       Nothing -> False)
  check "svg use: content inherits from the use"
    (case sc "<defs><rect id='r' width='2' height='2'/></defs><use href='#r' fill='blue'/>" of
       Just s -> map (\(_, _, p, _) -> solidOf p) (fills s) == [Just blue]
       Nothing -> False)
  check "svg use: symbol with viewBox scales to the use's size"
    (case sc "<symbol id='s' viewBox='0 0 10 10'><rect width='10' height='10'/></symbol><use href='#s' width='50' height='50'/>" of
       Just s -> case fills s of
         [(m, _, _, _)] -> approxA m (scaleA 5 5)
         _ -> False
       Nothing -> False)
  check "svg use: symbol clips to its viewport"
    (case sc "<symbol id='s' viewBox='0 0 10 10'><rect width='10' height='10'/></symbol><use href='#s' width='50' height='50'/>" of
       Just s -> not (null (clips (body s)))
       Nothing -> False)
  check "svg use: symbols are not drawn in place" (fmap (length . fills) (sc "<symbol id='s'><rect width='10' height='10'/></symbol>") == Just 0)
  check "svg use: self reference terminates" (fmap (length . fills) (sc "<g id='a'><rect width='1' height='1'/><use href='#a'/></g>") == Just 1)
  check "svg use: mutual recursion terminates" (isJust (sc "<g id='a'><use href='#b'/></g><g id='b'><use href='#a'/></g>"))
  t0 <- getMonotonicTime
  let bomb = concat [ "<g id='g" ++ show k ++ "'>" ++ concat (replicate 10 ("<use href='#g" ++ show (k - 1) ++ "'/>")) ++ "</g>" | k <- [1 .. 12 :: Int] ]
      bombDoc = wrap ("<defs><rect id='g0' width='1' height='1'/>" ++ bomb ++ "</defs><use href='#g12'/>")
  r <- try (evaluate (either (const False) (any ("too complex" `isInfixOf`) . scNotes) (parse bombDoc))) :: IO (Either SomeException Bool)
  t1 <- getMonotonicTime
  check "svg use: exponential use fan is bounded (fuel) and noted" (either (const False) id r && t1 - t0 < 60)
  -- Review item 3: instances of one target share one sub-scene (a DAG), and
  -- the logical size is what fuel bounds.
  let fan = wrap ("<defs><path id='p' d='M0 0 L1 0 L1 1 Z'/><g id='a'>" ++ concat (replicate 10 "<use href='#p'/>")
                  ++ "</g><g id='b'>" ++ concat (replicate 10 "<use href='#a' x='1'/>") ++ "</g></defs>"
                  ++ concat (replicate 10 "<use href='#b' y='1'/>"))
  check "svg use: a fan of 1000 instances renders 1000 leaves"
    (either (const False) ((== 1000) . length . fills) (parse fan))
  check "svg use: instances of one target share the same commands"
    (case parse fan of
       Right sc -> case [ cs | CTransform _ cs <- body sc ] of
         (a : b : _) -> sameList a b
         _ -> False
       Left _ -> False)
  check "svg use: paths are parsed once and shared across instances"
    (case parse fan of
       Right sc -> case [ p | (_, _, _, p) <- fills sc ] of
         (a : b : _) -> sameObj a b
         _ -> False
       Left _ -> False)
  check "svg nested svg: viewport translate, clip and viewBox"
    (case sc "<svg x='10' y='10' width='20' height='20' viewBox='0 0 2 2'><rect width='1' height='1'/></svg>" of
       Just s -> case fills s of
         [(m, _, _, _)] -> approxA m (Affine 10 0 0 10 10 10) && length (clips (body s)) == 1
         _ -> False
       Nothing -> False)
  check "svg nested svg: overflow visible skips the clip"
    (fmap (length . clips . body) (sc "<svg width='20' height='20' overflow='visible'><rect width='1' height='1'/></svg>") == Just 0)
  check "svg switch: first passing child only"
    (case sc "<switch><rect requiredExtensions='http://x' width='1' height='1' fill='red'/><rect systemLanguage='de' width='1' height='1' fill='red'/><rect systemLanguage='en-GB, fr' width='1' height='1' fill='blue'/><rect width='1' height='1' fill='red'/></switch>" of
       Just s -> map (\(_, _, p, _) -> solidOf p) (fills s) == [Just blue]
       Nothing -> False)
  check "svg display/visibility: none hides children, hidden does not"
    (case sc "<g display='none'><rect width='1' height='1'/></g><g visibility='hidden'><rect width='1' height='1'/><rect visibility='visible' width='1' height='1' fill='blue'/></g>" of
       Just s -> map (\(_, _, p, _) -> solidOf p) (fills s) == [Just blue]
       Nothing -> False)
  -- Basic shapes
  check "svg rect: rx alone sets ry, and is clamped"
    (case sc "<rect width='10' height='4' rx='100'/>" of
       Just s -> case fills s of
         [(_, _, _, path)] -> pathBoundsExact path == Just (Rect 0 0 10 4) && length path > 6
         _ -> False
       Nothing -> False)
  check "svg shapes: negative or zero sizes are not drawn"
    (fmap (length . fills) (sc "<rect width='-1' height='1'/><circle r='0'/><ellipse rx='1' ry='-1'/><rect width='0' height='5'/>") == Just 0)
  check "svg shapes: ellipse auto radius"
    (case sc "<ellipse rx='5' ry='auto'/>" of
       Just s -> case fills s of [(_, _, _, p)] -> pathBoundsExact p == Just (Rect (-5) (-5) 5 5); _ -> False
       Nothing -> False)
  check "svg shapes: polygon closes, polyline does not"
    (case sc "<polygon points='0,0 10,0 10,10'/><polyline points='0,0 10,0 10,10' fill='none' stroke='red'/>" of
       Just s -> case (fills s, strokes s) of
         ([(_, _, _, p1)], [(_, _, _, p2)]) -> last p1 == ClosePath && last p2 /= ClosePath
         _ -> False
       Nothing -> False)
  check "svg stroke: properties and odd dash array"
    (case sc "<line x2='10' stroke='red' stroke-width='3' stroke-linecap='round' stroke-linejoin='bevel' stroke-dasharray='1,2,3' stroke-dashoffset='1'/>" of
       Just s -> case strokes s of
         [(_, ss, _, _)] -> ssWidth ss == 3 && ssCap ss == CapRound && ssJoin ss == JoinBevel
                            && ssDash ss == [1, 2, 3, 1, 2, 3] && ssDashPhase ss == 1 && ssMiterLimit ss == 4
         _ -> False
       Nothing -> False)
  check "svg stroke: dash percentages, negative dash invalid, zero width none"
    (case sc "<line x2='10' stroke='red' stroke-dasharray='10% 5'/><line x2='10' stroke='red' stroke-dasharray='1 -1'/><line x2='10' stroke='red' stroke-width='0'/>" of
       Just s -> case strokes s of
         [(_, a, _, _), (_, b, _, _)] -> ssDash a == [10, 5] && ssDash b == []
         _ -> False
       Nothing -> False)
  check "svg stroke: paint-order puts stroke first"
    (case sc "<rect width='1' height='1' fill='red' stroke='blue' paint-order='stroke'/>" of
       Just s -> case [ c | (_, c) <- leaves (scCmds s) ] of
         [CStroke {}, CFill {}] -> True
         _ -> False
       Nothing -> False)
  check "svg stroke: non-scaling-stroke strokes in scene space"
    (case sc "<g transform='scale(4)'><line x2='10' stroke='red' stroke-width='2' vector-effect='non-scaling-stroke'/></g>" of
       Just s -> case strokes s of
         [(m, ss, _, p)] -> approxA m identityA && ssWidth ss == 2 && last p == LineTo (Pt 40 0)
         _ -> False
       Nothing -> False)
  -- Clip paths
  check "svg clip: union of children with per-child clip-rule"
    (case sc "<clipPath id='c'><rect width='5' height='5' clip-rule='evenodd'/><circle r='2'/></clipPath><rect width='10' height='10' clip-path='url(#c)'/>" of
       Just s -> case clips (body s) of
         [[ClipShape _ EvenOdd _, ClipShape _ NonZero _]] -> True
         _ -> False
       Nothing -> False)
  check "svg clip: objectBoundingBox units map onto the bbox"
    (case sc "<clipPath id='c' clipPathUnits='objectBoundingBox'><rect width='0.5' height='1'/></clipPath><rect x='10' y='10' width='20' height='20' clip-path='url(#c)'/>" of
       Just s -> case clips (body s) of
         [[ClipShape m _ _]] -> approxA m (Affine 20 0 0 20 10 10)
         _ -> False
       Nothing -> False)
  check "svg clip: clip-path on the clipPath nests"
    (case sc "<clipPath id='a'><rect width='5' height='5'/></clipPath><clipPath id='c' clip-path='url(#a)'><rect width='8' height='8'/></clipPath><rect width='10' height='10' clip-path='url(#c)'/>" of
       Just s -> length (clips (body s)) == 2
       Nothing -> False)
  check "svg clip: clip-path on a child becomes an alpha mask"
    (case sc "<clipPath id='a'><rect width='5' height='5'/></clipPath><clipPath id='c'><rect width='8' height='8' clip-path='url(#a)'/></clipPath><rect width='10' height='10' clip-path='url(#c)'/>" of
       Just s -> any (\g -> fmap smKind (goMask g) == Just MaskAlpha) (groups (body s))
       Nothing -> False)
  check "svg clip: use inside clipPath"
    (case sc "<defs><rect id='r' width='3' height='3'/></defs><clipPath id='c'><use href='#r' x='2'/></clipPath><rect width='10' height='10' clip-path='url(#c)'/>" of
       Just s -> case clips (body s) of
         [[ClipShape m _ _]] -> approxA m (translateA 2 0)
         _ -> False
       Nothing -> False)
  check "svg clip: empty clipPath clips everything"
    (case sc "<clipPath id='c'/><rect width='10' height='10' clip-path='url(#c)'/>" of
       Just s -> map length (clips (body s)) == [0]
       Nothing -> False)
  check "svg clip: missing reference is ignored"
    (fmap (\s -> (length (fills s), length (clips (body s)))) (sc "<rect width='10' height='10' clip-path='url(#nope)'/>") == Just (1, 0))
  -- Masks
  check "svg mask: luminance soft mask with the default region"
    (case sc "<mask id='m'><rect width='10' height='10' fill='white'/></mask><rect x='0' y='0' width='10' height='20' mask='url(#m)'/>" of
       Just s -> case [ m | g <- groups (body s), Just m <- [goMask g] ] of
         [SoftMask _ MaskLuminance (Color 0 0 0 0) [CClip [ClipShape _ _ p] _] Nothing] ->
           pathBounds p == Just (Rect (-1) (-2) 11 22)
         _ -> False
       Nothing -> False)
  check "svg mask: mask-type alpha"
    (case sc "<mask id='m' style='mask-type:alpha'><rect width='10' height='10'/></mask><rect width='10' height='20' mask='url(#m)'/>" of
       Just s -> [ smKind m | g <- groups (body s), Just m <- [goMask g] ] == [MaskAlpha]
       Nothing -> False)
  check "svg mask: element opacity shares the mask's group"
    (case sc "<mask id='m'><rect width='10' height='10' fill='white'/></mask><rect width='10' height='20' mask='url(#m)' opacity='0.5'/>" of
       Just s -> [ goOpacity g | g <- groups (body s), isJust (goMask g) ] == [0.5]
       Nothing -> False)
  -- Markers
  let mk = "<marker id='m' markerWidth='10' markerHeight='10' refX='1' refY='1' orient='auto' markerUnits='strokeWidth'><rect width='2' height='2' fill='red'/></marker>"
  check "svg marker: start/mid/end placed at every vertex"
    (case sc (mk ++ "<polyline points='0,0 10,0 10,10 20,10' fill='none' stroke='black' stroke-width='2' marker-start='url(#m)' marker-mid='url(#m)' marker-end='url(#m)'/>") of
       Just s -> length (fills s) == 4
       Nothing -> False)
  check "svg marker: orient auto, refX/refY and strokeWidth scale"
    (case sc (mk ++ "<line x1='5' y1='5' x2='5' y2='15' stroke='black' stroke-width='2' marker-end='url(#m)'/>") of
       Just s -> case fills s of
         -- end at (5,15) heading down (90°), scaled 2, ref (1,1) to the origin.
         [(m, _, _, _)] -> approxPt (applyA m (Pt 1 1)) (Pt 5 15) && approxPt (applyLinearA m (Pt 1 0)) (Pt 0 2)
         _ -> False
       Nothing -> False)
  check "svg marker: auto-start-reverse flips the start marker"
    (case sc "<marker id='m' orient='auto-start-reverse'><rect width='1' height='1'/></marker><line x2='10' stroke='black' marker-start='url(#m)'/>" of
       Just s -> case fills s of
         [(m, _, _, _)] -> approxPt (applyLinearA m (Pt 1 0)) (Pt (-1) 0)
         _ -> False
       Nothing -> False)
  check "svg marker: bisector at a corner"
    (case sc "<marker id='m' orient='auto' markerUnits='userSpaceOnUse'><rect width='1' height='1'/></marker><polyline points='0,0 10,0 10,10' fill='none' stroke='black' marker-mid='url(#m)'/>" of
       Just s -> case fills s of
         [(m, _, _, _)] -> let Pt x y = applyLinearA m (Pt 1 0) in approx x (sqrt 0.5) && approx y (sqrt 0.5)
         _ -> False
       Nothing -> False)
  check "svg marker: arc counts as one vertex"
    (case sc "<marker id='m' markerUnits='userSpaceOnUse'><rect width='1' height='1'/></marker><path d='M0 0 A10 10 0 1 1 0 20' fill='none' marker-mid='url(#m)' marker-start='url(#m)' marker-end='url(#m)'/>" of
       Just s -> length (fills s) == 2
       Nothing -> False)
  check "svg marker: closed subpath has start, mids and a close vertex"
    (case sc "<marker id='m' markerUnits='userSpaceOnUse'><rect width='1' height='1'/></marker><path d='M0 0 L10 0 L10 10 Z' fill='none' marker-mid='url(#m)'/>" of
       Just s -> length (fills s) == 2
       Nothing -> False)
  -- Images
  check "svg image: data URI PNG with preserveAspectRatio"
    (case sc ("<image x='10' y='0' width='40' height='20' href='data:image/png;base64," ++ png2x1 ++ "'/>") of
       Just s -> case [ (m, i) | (m, CImage _ i _) <- leaves (scCmds s) ] of
         -- 2×1 image into 40×20, meet: scale 20, centred (no x offset needed).
         [(m, _)] -> approxA m (Affine 40 0 0 20 10 0)
         _ -> False
       Nothing -> False)
  check "svg image: auto size from the image and pixelated rendering"
    (case sc ("<image style='image-rendering:pixelated' href='data:image/png;base64," ++ png2x1 ++ "'/>") of
       Just s -> case [ (m, o) | (m, CImage _ _ o) <- leaves (scCmds s) ] of
         [(m, o)] -> approxA m (Affine 2 0 0 1 0 0) && ioInterp o == InterpNearest
         _ -> False
       Nothing -> False)
  check "svg image: nested SVG via a percent-encoded data URI"
    (case sc "<image width='20' height='20' href=\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='10' height='10'%3E%3Crect width='5' height='5' fill='red'/%3E%3C/svg%3E\"/>" of
       Just s -> case fills s of
         [(m, _, p, _)] -> approxA m (scaleA 2 2) && solidOf p == Just red
         _ -> False
       Nothing -> False)
  check "svg image: external reference through the resolver"
    (case parseWith defaultSvgEnv { seResolve = \h -> if h == "pic.png" then Just (b64 png2x1) else Nothing }
                    (wrap "<image width='2' height='1' href='pic.png'/><image width='2' height='1' href='missing.png'/>") of
       Right s -> length [ () | (_, CImage {}) <- leaves (scCmds s) ] == 1 && any ("external" `isInfixOf`) (scNotes s)
       Left _ -> False)
  check "svg image: opacity folds into the image"
    (case sc ("<image width='2' height='1' opacity='0.5' href='data:image/png;base64," ++ png2x1 ++ "'/>") of
       Just s -> [ ioOpacity o | (_, CImage _ _ o) <- leaves (scCmds s) ] == [0.5]
       Nothing -> False)
  check "svg foreignObject is noted"
    (either (const False) (any ("foreignObject" `isInfixOf`) . scNotes) (parse (wrap "<foreignObject width='10' height='10'><div xmlns='http://www.w3.org/1999/xhtml'>hi</div></foreignObject>")))
  where
    b64 = decode64

-- A 2×1 RGBA PNG (red, blue), base64.
png2x1 :: String
png2x1 = "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAYAAAD0In+KAAAADklEQVR4nGP4z8AAQv8BD/kD/YURmXYAAAAASUVORK5CYII="

decode64 :: String -> BS.ByteString
decode64 = BS.pack . go . map val . filter (/= '=')
  where
    val c | c >= 'A' && c <= 'Z' = fromEnum c - 65
          | c >= 'a' && c <= 'z' = fromEnum c - 71
          | c >= '0' && c <= '9' = fromEnum c + 4
          | c == '+' = 62
          | otherwise = 63
    go (a : b : c : d : r) = let n = a * 262144 + b * 4096 + c * 64 + d in map fromIntegral [n `div` 65536, (n `div` 256) `mod` 256, n `mod` 256] ++ go r
    go [a, b, c] = let n = a * 262144 + b * 4096 + c * 64 in map fromIntegral [n `div` 65536, (n `div` 256) `mod` 256]
    go [a, b] = let n = a * 262144 + b * 4096 in [fromIntegral (n `div` 65536)]
    go _ = []

------------------------------------------------------------------------------
-- Text

-- A fake face: every printable character is a unit square glyph 500 units
-- wide in a 1000-unit em, ascent 800, descent -200; "AV" kerns by -100.
squareFace :: Int -> Face
squareFace key = Face
  { faceKey = key, faceUnitsPerEm = 1000, faceAscent = 800, faceDescent = -200
  , faceGlyph = \c -> if c >= ' ' && c <= '~'
                        then Just (fromEnum c, if c == ' ' then [] else rectPath 0 0 500 700, 500)
                        else Nothing
  , faceKern = \a b -> if (a, b) == (fromEnum 'A', fromEnum 'V') then -100 else 0 }

fontEnv :: SvgEnv
fontEnv = defaultSvgEnv { seFonts = \_ -> Just (squareFace 7) }

textTests :: (String -> Bool -> IO ()) -> IO ()
textTests check = do
  let glyphRuns s = case parseWith fontEnv (wrap s) of
        Right sc -> [ (m, p, gs) | (m, CGlyphs _ p gs) <- leaves (scCmds sc) ]
        Left _ -> []
      origins s = [ applyA (composeA m ga) (Pt 0 0) | (m, _, gs) <- glyphRuns s, Glyph ga _ _ <- gs ]
  check "svg text: glyphs advance by font size / em"
    (origins "<text x='10' y='20' font-size='20'>ab</text>" == [Pt 10 20, Pt 20 20])
  check "svg text: glyph space is flipped (y up)"
    (case glyphRuns "<text x='0' y='20' font-size='10'>a</text>" of
       [(m, _, [Glyph ga _ _])] -> approxPt (applyA (composeA m ga) (Pt 0 700)) (Pt 0 13)
       _ -> False)
  check "svg text: text-anchor middle and end over the chunk"
    (origins "<text x='100' font-size='20' text-anchor='middle'>ab</text>" == [Pt 90 0, Pt 100 0]
      && origins "<text x='100' font-size='20' text-anchor='end'>ab</text>" == [Pt 80 0, Pt 90 0])
  check "svg text: x/y/dx/dy lists"
    (origins "<text x='0 50' y='10' dx='1 2 3' dy='0 0 5' font-size='10'>abc</text>" == [Pt 1 10, Pt 52 10, Pt 60 15])
  check "svg text: tspan positions override, chunks re-anchor"
    (origins "<text x='0' font-size='10' text-anchor='middle'>ab<tspan x='100'>cd</tspan></text>" == [Pt (-5) 0, Pt 0 0, Pt 95 0, Pt 100 0])
  check "svg text: whitespace collapses across elements and trims"
    (length (origins "<text font-size='10'>\n   a  \n <tspan> b </tspan>   </text>") == 2
      && origins "<text font-size='10'>\n   a  \n <tspan> b </tspan>   </text>" == [Pt 0 0, Pt 10 0])
  check "svg text: xml:space preserve keeps spaces"
    (origins "<text font-size='10' xml:space='preserve'> a  b</text>" == [Pt 5 0, Pt 20 0])
  check "svg text: letter- and word-spacing"
    (origins "<text font-size='10' letter-spacing='2' word-spacing='10'>a b</text>" == [Pt 0 0, Pt 24 0])
  check "svg text: kerning"
    (origins "<text font-size='10'>AV</text>" == [Pt 0 0, Pt 4 0])
  check "svg text: rotate list, last value repeats"
    (case glyphRuns "<text font-size='10' rotate='90'>ab</text>" of
       [(m, _, gs)] -> all (\(Glyph ga _ _) -> approxPt (applyLinearA (composeA m ga) (Pt 1 0)) (Pt 0 0.01)) gs
       _ -> False)
  check "svg text: glyph cache keys are per face and glyph"
    (case glyphRuns "<text>aa</text>" of
       [(_, _, [Glyph _ k1 _, Glyph _ k2 _])] -> k1 == k2 && k1 >= 0
       _ -> False)
  check "svg text: tspan fill makes its own run"
    (map (\(_, p, gs) -> (solidOf p, length gs)) (glyphRuns "<text fill='red'>ab<tspan fill='blue'>c</tspan></text>")
      == [(Just red, 2), (Just blue, 1)])
  check "svg text: stroke outlines the glyphs"
    (case parseWith fontEnv (wrap "<text stroke='red' fill='none'>ab</text>") of
       Right s -> case strokes s of [(_, _, _, p)] -> length p == 10; _ -> False
       Left _ -> False)
  check "svg text: dominant-baseline central"
    (origins "<text y='100' font-size='10' dominant-baseline='central'>a</text>" == [Pt 0 103])
  check "svg text: no face means no glyphs and a note"
    (case parse (wrap "<text>hello</text>") of
       Right s -> null (leaves (scCmds s)) && any ("font" `isInfixOf`) (scNotes s)
       Left _ -> False)
  check "svg text: per-character fallback through the family list"
    (case parseWith defaultSvgEnv { seFonts = \q -> if fqFamilies q == ["B"] then Just (squareFace 2) else
                                                      if take 1 (fqFamilies q) == ["A"] then Just narrow else Nothing }
                    (wrap "<text font-family='A, B'>ab</text>") of
       Right s -> length [ () | (_, CGlyphs _ _ gs) <- leaves (scCmds s), _ <- gs ] == 2
       Left _ -> False)
  check "svg text: font query carries family list, weight and style"
    (let seen q = fqFamilies q == ["Foo Bar", "serif"] && fqWeight q == 700 && fqStyle q == FontItalic
     in case parseWith defaultSvgEnv { seFonts = \q -> if seen q then Just (squareFace 1) else Nothing }
                       (wrap "<text style=\"font: italic bold 12px 'Foo Bar', serif\">a</text>") of
          Right s -> not (null [ () | (_, CGlyphs {}) <- leaves (scCmds s) ])
          Left _ -> False)
  check "svg text: textPath places glyph midpoints on the path"
    -- A vertical path: glyphs 5 wide (font-size 10) centred on it, rotated 90°.
    (case glyphRuns "<defs><path id='p' d='M50 0 L50 100'/></defs><text font-size='10'><textPath href='#p' startOffset='10'>ab</textPath></text>" of
       [(m, _, [Glyph a _ _, Glyph b _ _])] ->
         approxPt (applyA (composeA m a) (Pt 250 0)) (Pt 50 12.5) && approxPt (applyA (composeA m b) (Pt 250 0)) (Pt 50 17.5)
         && approxPt (applyLinearA (composeA m a) (Pt 100 0)) (Pt 0 1)
       _ -> False)
  check "svg text: textPath hides glyphs past the end, anchors along the path"
    (length (origins "<defs><path id='p' d='M0 0 L12 0'/></defs><text font-size='10'><textPath href='#p'>abcd</textPath></text>") == 2
      && origins "<defs><path id='p' d='M0 0 L100 0'/></defs><text font-size='10' text-anchor='middle'><textPath href='#p' startOffset='50%'>ab</textPath></text>" == [Pt 45 0, Pt 50 0])
  check "svg text: text in a clip path"
    (case parseWith fontEnv (wrap "<clipPath id='c'><text font-size='10'>ab</text></clipPath><rect width='10' height='10' clip-path='url(#c)'/>") of
       Right s -> map length (clips (body s)) == [2]
       Left _ -> False)
  where
    -- Only 'a'.
    narrow = (squareFace 3) { faceGlyph = \c -> if c == 'a' then Just (97, rectPath 0 0 500 700, 500) else Nothing }

------------------------------------------------------------------------------
-- Filters

filterTests :: (String -> Bool -> IO ()) -> IO ()
filterTests check = do
  let filt s = case parse (wrap s) of
        Right sc -> [ f | g <- groups (scCmds sc), Just f <- [goFilter g] ]
        Left _ -> []
  check "svg filter: blur with the default region"
    (case filt "<filter id='f'><feGaussianBlur stdDeviation='2 3'/></filter><rect x='0' y='0' width='10' height='20' filter='url(#f)'/>" of
       [Filter r m [FilterPrim (FeBlur 2 3) [Previous] Nothing True 0]] -> r == Rect (-1) (-2) 11 22 && m == identityA
       _ -> False)
  check "svg filter: named results, in/in2, merge and composite"
    (case filt ("<filter id='f'><feOffset in='SourceAlpha' dx='2' dy='3' result='o'/><feFlood flood-color='red' flood-opacity='0.5'/>"
                ++ "<feComposite in2='o' operator='in' result='c'/><feMerge><feMergeNode in='c'/><feMergeNode in='SourceGraphic'/></feMerge></filter>"
                ++ "<rect width='10' height='10' filter='url(#f)'/>") of
       [Filter _ _ [p0, p1, p2, p3]] ->
         case (fpOp p0, fpIns p0, fpOp p1, fpOp p2, fpIns p2, fpOp p3, fpIns p3) of
           (FeOffset 2 3, [SourceAlpha], FeFlood c, FeComposite CompIn, [Previous, Result 0], FeMerge, [Result 2, SourceGraphic]) ->
             approxC c (Color 1 0 0 0.5)
           _ -> False
       _ -> False)
  check "svg filter: colour matrix types expand to 20 values"
    (case filt "<filter id='f'><feColorMatrix type='saturate' values='0'/><feColorMatrix type='hueRotate' values='90'/><feColorMatrix type='luminanceToAlpha'/><feColorMatrix values='1 2'/></filter><rect width='1' height='1' filter='url(#f)'/>" of
       [Filter _ _ ps] -> map (\p -> case fpOp p of FeColorMatrix m -> length m; _ -> 0) ps == [20, 20, 20, 20]
                          && (case fpOp (head ps) of FeColorMatrix m -> approx (head m) 0.213; _ -> False)
       _ -> False)
  check "svg filter: primitiveUnits objectBoundingBox scales values and subregions"
    (case filt "<filter id='f' primitiveUnits='objectBoundingBox'><feGaussianBlur stdDeviation='0.1' x='0' width='0.5'/></filter><rect width='20' height='40' filter='url(#f)'/>" of
       [Filter _ _ [FilterPrim (FeBlur sx sy) _ (Just (Rect 0 _ 10 _)) _ _]] -> approx sx 2 && approx sy 4
       _ -> False)
  check "svg filter: color-interpolation-filters sRGB"
    (case filt "<filter id='f' color-interpolation-filters='sRGB'><feGaussianBlur stdDeviation='1'/></filter><rect width='1' height='1' filter='url(#f)'/>" of
       [Filter _ _ [p]] -> not (fpLinearRGB p)
       _ -> False)
  check "svg filter: drop shadow and blend"
    (case filt "<filter id='f'><feDropShadow dx='1' dy='1' stdDeviation='1' flood-color='blue'/><feBlend in2='SourceGraphic' mode='multiply'/></filter><rect width='1' height='1' filter='url(#f)'/>" of
       [Filter _ _ [p0, p1]] -> case (fpOp p0, fpOp p1) of
         (FeDropShadow 1 1 1 1 c, FeBlend BlendMultiply) -> approxC c blue
         _ -> False
       _ -> False)
  check "svg filter: linear component transfer becomes a matrix"
    (case filt "<filter id='f'><feComponentTransfer><feFuncR type='linear' slope='2' intercept='0.1'/><feFuncA type='table' tableValues='1 0'/></feComponentTransfer></filter><rect width='1' height='1' filter='url(#f)'/>" of
       [Filter _ _ [FilterPrim (FeColorMatrix m) _ _ _ _]] -> m == [2,0,0,0,0.1, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,-1,1]
       _ -> False)
  check "svg filter: unsupported primitive draws unfiltered with a note"
    (case parse (wrap "<filter id='f'><feTurbulence baseFrequency='0.1'/></filter><rect width='1' height='1' filter='url(#f)'/>") of
       Right s -> null (groups (scCmds s)) && length (fills s) == 1 && any ("feTurbulence" `isInfixOf`) (scNotes s)
       Left _ -> False)
  check "svg filter: BackgroundImage input is unsupported"
    (case parse (wrap "<filter id='f'><feBlend in2='BackgroundImage'/></filter><rect width='1' height='1' filter='url(#f)'/>") of
       Right s -> length (fills s) == 1 && any ("BackgroundImage" `isInfixOf`) (scNotes s)
       Left _ -> False)
  check "svg filter: a missing filter hides the element"
    (either (const False) (null . fills) (parse (wrap "<rect width='1' height='1' filter='url(#nope)'/>")))
  check "svg filter: an empty filter hides the element"
    (either (const False) (null . fills) (parse (wrap "<filter id='f'/><rect width='1' height='1' filter='url(#f)'/>")))
  check "svg filter: CSS shorthand functions chain"
    (length (filt "<rect width='10' height='10' style='filter: blur(2px) grayscale(1)'/>") == 2)
  check "svg filter: filter applies inside the clip"
    (case parse (wrap "<clipPath id='c'><rect width='5' height='5'/></clipPath><filter id='f'><feGaussianBlur stdDeviation='1'/></filter><rect width='10' height='10' filter='url(#f)' clip-path='url(#c)'/>") of
       Right s -> case body s of
         [CClip _ [CGroup o _]] -> isJust (goFilter o)
         _ -> False
       Left _ -> False)

------------------------------------------------------------------------------
-- Robustness: fuzzing and bounds

fixtures :: [BS.ByteString]
fixtures = map svgDoc
  [ wrap ("<style>.a{fill:red} #b>c{stroke:blue}</style><defs><linearGradient id='g' x2='1'><stop offset='0' stop-color='red'/><stop offset='1' stop-color='#00f'/></linearGradient>"
          ++ "<pattern id='p' width='4' height='4' patternUnits='userSpaceOnUse'><circle r='2' fill='url(#g)'/></pattern></defs>"
          ++ "<path class='a' d='M10 10 h20 v20 a5 5 0 1 1-10 0 q5 5 10 0 t10 0 c1 2 3 4 5 6 s1 1 2 2 z' fill='url(#p)'/>")
  , wrap ("<clipPath id='c'><text>x</text><rect width='5' height='5'/></clipPath><mask id='m'><rect width='9' height='9' fill='white'/></mask>"
          ++ "<g clip-path='url(#c)' mask='url(#m)' opacity='.5'><use href='#r'/><rect id='r' width='3' height='3' rx='1'/></g>"
          ++ "<marker id='k' viewBox='0 0 2 2' orient='auto'><path d='M0 0 L2 1 L0 2z'/></marker><polyline points='0,0 5,5 10,0' marker-mid='url(#k)' stroke='red'/>")
  , wrap ("<filter id='f'><feGaussianBlur stdDeviation='2' result='b'/><feOffset dx='1' in='b'/><feMerge><feMergeNode/><feMergeNode in='SourceGraphic'/></feMerge></filter>"
          ++ "<symbol id='s' viewBox='0 0 4 4'><ellipse cx='2' cy='2' rx='2' ry='1'/></symbol><use href='#s' width='20' height='20' filter='url(#f)'/>"
          ++ "<text x='1 2 3' dy='1' font-size='2em'>ab<tspan fill='blue' rotate='30'>cd</tspan></text>"
          ++ "<image width='2' height='1' href='data:image/png;base64," ++ png2x1 ++ "'/>")
  ]

robustnessTests :: (String -> Bool -> IO ()) -> IO ()
robustnessTests check = do
  let force env bs = case parseSvg env bs of
        Left e -> length e
        Right sc -> length (show sc)
      variants bs =
        [ BS.take n bs | n <- [0, 7 .. BS.length bs] ]
        ++ [ flipAt k b bs | (k, b) <- zip [0, 13 .. BS.length bs - 1] (cycle [0, 1, 3, 5, 7]) ]
      flipAt k b bs = let (a, c) = BS.splitAt k bs
                      in case BS.uncons c of
                           Just (x, rest) -> BS.concat [a, BS.singleton (x `xor` (1 `shiftL` b)), rest]
                           Nothing -> bs
  results <- mapM (\bs -> try (evaluate (force fontEnv bs)) :: IO (Either SomeException Int))
                  (concatMap variants fixtures)
  check "svg fuzz: truncations and bit flips never throw" (all (either (const False) (const True)) results)
  check "svg fuzz: every fixture parses" (all (\bs -> either (const False) (const True) (parseSvg fontEnv bs)) fixtures)
  -- Garbage and pathological inputs.
  let junk = [ BS.empty, BC.pack "<svg", BC.pack "<svg xmlns='http://www.w3.org/2000/svg' width='1e400' height='5'/>"
             , BC.pack "<svg width='NaN' height='5'/>", BS.pack [0x1f, 0x8b], BS.replicate 1000 0xff
             , svgDoc (wrap (replicate 300 '<' ++ "g>"))
             , svgDoc (wrap ("<path d='M0 0" ++ concat (replicate 20000 " l1 1") ++ "'/>"))
             , svgDoc (wrap (concat (replicate 2000 "<g>") ++ "<rect width='1' height='1'/>"))
             , svgDoc (wrap ("<style>" ++ concat (replicate 20000 ".a{fill:red}") ++ "</style><rect class='a' width='1' height='1'/>"))
             , svgDoc (wrap "<linearGradient id='a' href='#b'/><linearGradient id='b' href='#a'/><rect width='1' height='1' fill='url(#a)'/>")
             , svgDoc (wrap "<mask id='m' mask='url(#m)'><rect width='1' height='1'/></mask><rect width='1' height='1' mask='url(#m)'/>")
             , svgDoc (wrap "<clipPath id='c' clip-path='url(#c)'><rect width='1' height='1'/></clipPath><rect width='1' height='1' clip-path='url(#c)'/>")
             , svgDoc (wrap "<marker id='m'><path d='M0 0 L1 1' marker-end='url(#m)'/></marker><path d='M0 0 L1 1' marker-end='url(#m)'/>")
             , svgDoc (wrap "<filter id='f' href='#f'><feOffset/></filter><rect width='1' height='1' filter='url(#f)'/>")
             , svgDoc (wrap "<image width='1' height='1' href='data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxJyBoZWlnaHQ9JzEnLz4='/>") ]
  t0 <- getMonotonicTime
  jr <- mapM (\bs -> try (evaluate (force fontEnv bs)) :: IO (Either SomeException Int)) junk
  t1 <- getMonotonicTime
  check "svg fuzz: pathological inputs never throw" (all (either (const False) (const True)) jr)
  check "svg fuzz: pathological inputs are quick" (t1 - t0 < 20)
  -- A self-nesting SVG image is bounded by depth.
  let selfImg = wrap ("<image width='10' height='10' href='data:image/svg+xml;base64," ++ selfB64 ++ "'/>")
      selfB64 = "PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHdpZHRoPScxJyBoZWlnaHQ9JzEnPjxyZWN0IHdpZHRoPScxJyBoZWlnaHQ9JzEnLz48L3N2Zz4="
  check "svg image: nested data SVG renders"
    (either (const False) ((== 1) . length . fills) (parse selfImg))
