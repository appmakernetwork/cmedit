{-# LANGUAGE BangPatterns #-}
-- | The CSS that SVG files actually contain: @\<style\>@ sheets, @style=\"\"@
-- declaration lists, selectors and their specificity, and CSS colours.
-- Internal to "Cmedit.Svg".
--
-- __A subset, chosen by what exporters write.__ Illustrator, Inkscape,
-- Figma, svgo and icon fonts emit type, class and id selectors, descendant
-- and child combinators and the odd attribute selector; that is the subset,
-- plus the sibling combinators and @:first-child@ because they cost a line
-- each. Anything else is handled the way CSS itself says to: an at-rule is
-- skipped whole (including a nested @\@media@ block), a dynamic
-- pseudo-class such as @:hover@ simply never matches in a static picture,
-- and a selector this module cannot parse drops its rule, which is CSS's own
-- error recovery.
--
-- __Matching is against an interface, not the SVG tree.__ 'ElemInfo' is
-- the few questions a selector asks (name, attributes, parent, previous
-- sibling), so this module needs nothing from "Cmedit.Svg" and the tree
-- needs no CSS knowledge. Rules are bucketed by the rightmost compound's id,
-- class or type ('indexRules'), so matching a sheet of a thousand rules
-- against a large drawing costs the rules that could match, not all of them.
module Cmedit.SvgCss
  ( -- * Declarations
    Decl(..)
  , parseDecls
    -- * Style sheets
  , Rule(..)
  , Specificity
  , parseSheet
  , maxCssRules
    -- * Selectors
  , ElemInfo(..)
  , Selector
  , parseSelector
  , selectorSpecificity
  , matches
  , RuleIndex
  , indexRules
  , candidateRules
    -- * Colours
  , ColorSpec(..)
  , parseColor
  , namedColor
  , hslToRgb
  ) where

import Data.Char (isAlphaNum, isHexDigit, isSpace, digitToInt)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Scene (Color(..))
import Cmedit.SvgPath (parseNumber)

------------------------------------------------------------------------------
-- Declarations

-- | One @property: value@ pair. The property name is lower-cased; the value
-- is trimmed and has had any @!important@ removed into 'dImportant'.
data Decl = Decl
  { dProp      :: !Text
  , dValue     :: !Text
  , dImportant :: !Bool
  } deriving (Eq, Show)

-- | A declaration block's contents (@a: b; c: d !important@). Semicolons
-- inside parentheses or quotes do not split (a @url(data:...;base64,...)@
-- is one value).
parseDecls :: Text -> [Decl]
parseDecls = mapMaybe decl . splitTop ';' . stripComments
  where
    decl d =
      let (k, v0) = T.break (== ':') d
          k' = T.toLower (T.strip k)
          v1 = T.strip (T.drop 1 v0)
          (v, imp) = important v1
      in if T.null k' || T.null v0 || T.null v || not (T.all propChar k') then Nothing
         else Just (Decl k' v imp)
    propChar c = isAlphaNum c || c == '-' || c == '_'
    important v =
      let (pre, post) = T.breakOnEnd "!" v
      in if not (T.null pre) && T.toLower (T.strip post) == "important"
           then (T.strip (T.dropEnd 1 pre), True)
           else (v, False)

-- | Split on a character at nesting depth zero (outside parentheses,
-- brackets and quotes).
splitTop :: Char -> Text -> [Text]
splitTop sep = go (0 :: Int) Nothing [] [] . T.unpack
  where
    go _ _ cur acc [] = reverse (fin cur : acc)
    go d q cur acc (c : cs) = case q of
      Just qc | c == qc   -> go d Nothing (c : cur) acc cs
              | c == '\\', (x : xs) <- cs -> go d q (x : c : cur) acc xs
              | otherwise -> go d q (c : cur) acc cs
      Nothing
        | c == '"' || c == '\'' -> go d (Just c) (c : cur) acc cs
        | c == '(' || c == '['  -> go (d + 1) q (c : cur) acc cs
        | c == ')' || c == ']'  -> go (max 0 (d - 1)) q (c : cur) acc cs
        | c == sep && d == 0    -> go d q [] (fin cur : acc) cs
        | otherwise             -> go d q (c : cur) acc cs
    fin = T.pack . reverse

-- | Remove @/* ... */@ comments (an unterminated one runs to the end).
stripComments :: Text -> Text
stripComments t =
  let (pre, rest) = T.breakOn "/*" t
  in if T.null rest then t
     else let (_, post) = T.breakOn "*/" (T.drop 2 rest)
          in T.append pre (if T.null post then T.empty else stripComments (T.drop 2 post))

------------------------------------------------------------------------------
-- Style sheets

-- | (ids, classes + attributes + pseudo-classes, types).
type Specificity = (Int, Int, Int)

-- | One selector of a rule with the rule's declarations. A rule with a
-- selector list becomes one 'Rule' per selector, because each has its own
-- specificity. 'ruleOrder' is the position in the document's sheets.
data Rule = Rule
  { ruleSel   :: Selector
  , ruleSpec  :: !Specificity
  , ruleOrder :: !Int
  , ruleDecls :: [Decl]
  } deriving (Show)

-- | Most rules (selectors, after splitting lists) a document's sheets may
-- contribute; the rest are dropped.
maxCssRules :: Int
maxCssRules = 5000

-- | Parse a style sheet. @order0@ numbers the rules so that several
-- @\<style\>@ elements keep document order.
parseSheet :: Int -> Text -> [Rule]
parseSheet order0 src = take maxCssRules (number order0 (go (stripComments src)))
  where
    number !_ [] = []
    number !n ((s, ds) : r) = Rule s (selectorSpecificity s) n ds : number (n + 1) r
    go t0 =
      let t = T.dropWhile (\c -> isSpace c) (dropCdo t0)
      in case T.uncons t of
           Nothing -> []
           Just ('@', r) ->
             -- An at-rule: up to ';' or the end of its (nested) block.
             let (pre, post) = T.break (\c -> c == ';' || c == '{') r
             in case T.uncons post of
                  Just (';', r') -> go r'
                  Just ('{', r') -> go (skipBlock 1 r')
                  _ -> const [] pre
           Just ('}', r) -> go r       -- stray closer
           _ ->
             let (sels, post) = T.break (== '{') t
             in case T.uncons post of
                  Nothing -> []
                  Just (_, r) ->
                    let (body, r') = T.break (== '}') r
                        decls = parseDecls body
                        parsed = map parseSelector (splitTop ',' sels)
                        -- One bad selector drops the whole rule (CSS error
                        -- recovery); dynamic pseudo-classes parse but never
                        -- match.
                        rules = if not (all isJust parsed) || null decls
                                  then []
                                  else [ (s, decls) | Just s <- parsed ]
                    in rules ++ go (T.drop 1 r')
    dropCdo t = let t' = T.dropWhile isSpace t
                in case () of
                     _ | "<!--" `T.isPrefixOf` t' -> dropCdo (T.drop 4 t')
                       | "-->" `T.isPrefixOf` t'  -> dropCdo (T.drop 3 t')
                       | otherwise                -> t'
    skipBlock :: Int -> Text -> Text
    skipBlock 0 t = t
    skipBlock n t =
      let (_, post) = T.break (\c -> c == '{' || c == '}') t
      in case T.uncons post of
           Nothing        -> T.empty
           Just ('{', r)  -> skipBlock (n + 1) r
           Just (_, r)    -> skipBlock (n - 1) r

------------------------------------------------------------------------------
-- Selectors

-- | What a selector may ask about an element. Built lazily by the caller.
data ElemInfo = ElemInfo
  { eiName    :: Text
  , eiAttr    :: Text -> Maybe Text
  , eiParent  :: Maybe ElemInfo
  , eiPrev    :: Maybe ElemInfo    -- ^ Previous element sibling.
  , eiIsLast  :: Bool              -- ^ No element sibling after it.
  }

data AttrOp = AExists | AEq Text | AWord Text | APrefix Text | ASuffix Text | ASub Text | ADash Text
  deriving (Eq, Show)

data Simple
  = SType Text
  | SId Text
  | SClass Text
  | SAttr Text AttrOp
  | SFirstChild
  | SLastChild
  | SRoot
  | SUniversal
  | SNever            -- ^ A pseudo-class that never holds in a static picture (or one outside the subset).
  deriving (Eq, Show)

data Comb = Descendant | Child | Adjacent | Sibling
  deriving (Eq, Show)

-- | A complex selector, stored right to left: the subject compound first,
-- each further compound with the combinator that links it to the one before.
data Selector = Selector [Simple] [(Comb, [Simple])]
  deriving (Eq, Show)

-- | Parse one complex selector, or 'Nothing' if it uses syntax outside the
-- subset (see the module header).
parseSelector :: Text -> Maybe Selector
parseSelector t0 = do
  parts <- go (T.strip t0) []
  case parts of
    [] -> Nothing
    ((_, subj) : more) -> Just (Selector subj (zip (map fst parts) (map snd more)))
  where
    -- Accumulates (combinator linking it to the compound on its left,
    -- compound), rightmost first; the subject is the head.
    go t acc
      | T.null t = Just acc
      | otherwise = do
          let (comb, t1) = combinator t
          (cmp, t2) <- compound t1 []
          if null cmp then Nothing else go t2 ((comb, cmp) : acc)
    combinator t =
      let t1 = T.dropWhile isSpace t
      in case T.uncons t1 of
           Just ('>', r) -> (Child, T.dropWhile isSpace r)
           Just ('+', r) -> (Adjacent, T.dropWhile isSpace r)
           Just ('~', r) -> (Sibling, T.dropWhile isSpace r)
           _             -> (Descendant, t1)
    -- A compound; "*" alone is a compound with no conditions, recorded as
    -- an always-true attribute-free entry so it is not mistaken for empty.
    compound t acc = case T.uncons t of
      Just ('*', r) -> compound r (if null acc then [SUniversal] else acc)
      Just ('#', r) -> let (n, r') = ident r in if T.null n then Nothing else compound r' (SId n : acc)
      Just ('.', r) -> let (n, r') = ident r in if T.null n then Nothing else compound r' (SClass n : acc)
      Just ('[', r) -> do (a, r') <- attrSel r; compound r' (a : acc)
      Just (':', r) -> do (ps, r') <- pseudo r; compound r' (ps ++ acc)
      Just (c, _) | isIdentStart c && null acc ->
        let (n, r') = ident t in compound r' [SType n]
      Just (c, _) | isSpace c || c == '>' || c == '+' || c == '~' -> Just (reverse acc, t)
      Nothing -> Just (reverse acc, t)
      _ -> Nothing
    attrSel r0 = do
      let r = T.dropWhile isSpace r0
          (n, r1) = ident r
      if T.null n then Nothing else do
        let r2 = T.dropWhile isSpace r1
        case T.uncons r2 of
          Just (']', r3) -> Just (SAttr n AExists, r3)
          _ -> do
            let (opT, r3) = T.span (`elem` ("~^$*|=" :: String)) r2
            mk <- case opT of
              "="  -> Just AEq; "~=" -> Just AWord; "^=" -> Just APrefix
              "$=" -> Just ASuffix; "*=" -> Just ASub; "|=" -> Just ADash
              _    -> Nothing
            let r4 = T.dropWhile isSpace r3
            (v, r5) <- case T.uncons r4 of
              Just (q, rq) | q == '"' || q == '\'' ->
                let (v, rr) = T.break (== q) rq in Just (v, T.drop 1 rr)
              _ -> let (v, rr) = ident r4 in if T.null v then Nothing else Just (v, rr)
            -- A case-sensitivity flag ([a=b i]) is accepted and ignored.
            let r5' = T.dropWhile isSpace r5
                r5'' = case T.uncons r5' of
                         Just (f, rr) | f == 'i' || f == 's' || f == 'I' || f == 'S' -> rr
                         _ -> r5'
            r6 <- T.stripPrefix "]" (T.dropWhile isSpace r5'')
            Just (SAttr n (mk v), r6)
    pseudo r = case T.uncons r of
      Just (':', _) -> Just ([SNever], snd (ident (T.drop 1 r)))   -- pseudo-element
      _ -> let (n, r') = ident r
               n' = T.toLower n
           in if T.null n' then Nothing else case T.uncons r' of
                -- A functional pseudo-class (:not(...), :nth-child(...)) is
                -- outside the subset: the selector never matches.
                Just ('(', r2) -> Just ([SNever], T.drop 1 (T.dropWhile (/= ')') r2))
                _ -> Just (case n' of
                             "first-child" -> [SFirstChild]
                             "last-child"  -> [SLastChild]
                             "only-child"  -> [SFirstChild, SLastChild]
                             "root"        -> [SRoot]
                             _             -> [SNever], r')
    isIdentStart c = isAlphaNum c || c == '_' || c == '-' || c > '\x7f'
    ident = T.span (\c -> isAlphaNum c || c == '_' || c == '-' || c > '\x7f')

-- | Specificity of a selector.
selectorSpecificity :: Selector -> Specificity
selectorSpecificity (Selector subj rest) =
  foldr add (0, 0, 0) (subj ++ concatMap snd rest)
  where
    add s (a, b, c) = case s of
      SId _     -> (a + 1, b, c)
      SClass _  -> (a, b + 1, c)
      SAttr _ _ -> (a, b + 1, c)
      SType _   -> (a, b, c + 1)
      SUniversal -> (a, b, c)
      _         -> (a, b + 1, c)

-- | Does the selector match the element?
matches :: Selector -> ElemInfo -> Bool
matches (Selector subj rest) e = compoundOk subj e && chain rest e
  where
    chain [] _ = True
    chain ((comb, cmp) : more) el = case comb of
      Child      -> case eiParent el of
                      Just p  -> compoundOk cmp p && chain more p
                      Nothing -> False
      Descendant -> anyUp (eiParent el)
        where anyUp Nothing  = False
              anyUp (Just p) = (compoundOk cmp p && chain more p) || anyUp (eiParent p)
      Adjacent   -> case eiPrev el of
                      Just p  -> compoundOk cmp p && chain more p
                      Nothing -> False
      Sibling    -> anyPrev (eiPrev el)
        where anyPrev Nothing  = False
              anyPrev (Just p) = (compoundOk cmp p && chain more p) || anyPrev (eiPrev p)

compoundOk :: [Simple] -> ElemInfo -> Bool
compoundOk ss e = all ok ss
  where
    ok s = case s of
      SType n     -> n == eiName e
      SId n       -> eiAttr e "id" == Just n
      SClass n    -> maybe False (elem n . T.words) (eiAttr e "class")
      SAttr n op  -> case eiAttr e n of
        Nothing -> False
        Just v  -> case op of
          AExists   -> True
          AEq x     -> v == x
          AWord x   -> x `elem` T.words v
          APrefix x -> not (T.null x) && x `T.isPrefixOf` v
          ASuffix x -> not (T.null x) && x `T.isSuffixOf` v
          ASub x    -> not (T.null x) && x `T.isInfixOf` v
          ADash x   -> v == x || T.append x "-" `T.isPrefixOf` v
      SFirstChild -> case eiPrev e of Nothing -> True; Just _ -> False
      SLastChild  -> eiIsLast e
      SRoot       -> case eiParent e of Nothing -> True; Just _ -> False
      SUniversal  -> True
      SNever      -> False

-- | Rules bucketed by the subject compound's most selective simple selector.
data RuleIndex = RuleIndex
  { riId    :: M.Map Text [Rule]
  , riClass :: M.Map Text [Rule]
  , riType  :: M.Map Text [Rule]
  , riAny   :: [Rule]
  }

indexRules :: [Rule] -> RuleIndex
indexRules = foldr ins (RuleIndex M.empty M.empty M.empty [])
  where
    ins r ix = case key (subjOf (ruleSel r)) of
      Just (0, k) -> ix { riId = M.insertWith (++) k [r] (riId ix) }
      Just (1, k) -> ix { riClass = M.insertWith (++) k [r] (riClass ix) }
      Just (_, k) -> ix { riType = M.insertWith (++) k [r] (riType ix) }
      Nothing     -> ix { riAny = r : riAny ix }
    subjOf (Selector s _) = s
    key :: [Simple] -> Maybe (Int, Text)
    key ss = case ([ i | SId i <- ss ], [ c | SClass c <- ss ], [ t | SType t <- ss ]) of
      (i : _, _, _) -> Just (0, i)
      (_, c : _, _) -> Just (1, c)
      (_, _, t : _) -> Just (2, t)
      _             -> Nothing

-- | The rules that could match an element with this name, id and class
-- list, in document order. The caller still runs 'matches' on each.
candidateRules :: RuleIndex -> Text -> Maybe Text -> [Text] -> [Rule]
candidateRules ix name mid classes =
  sortOn ruleOrder $
       maybe [] (\i -> M.findWithDefault [] i (riId ix)) mid
    ++ concatMap (\c -> M.findWithDefault [] c (riClass ix)) (dedup classes)
    ++ M.findWithDefault [] name (riType ix)
    ++ riAny ix
  where dedup = M.keys . M.fromList . map (\c -> (c, ()))

------------------------------------------------------------------------------
-- Colours

-- | A parsed colour: a concrete one or @currentColor@ (resolved by the
-- caller against the inherited @color@).
data ColorSpec = CSColor !Color | CSCurrent
  deriving (Eq, Show)

-- | Parse a CSS colour: a name (the 147 SVG names, @rebeccapurple@,
-- @transparent@), @#rgb@, @#rgba@, @#rrggbb@, @#rrggbbaa@, @rgb[a]()@ and
-- @hsl[a]()@ in the comma and the CSS4 space-separated forms with optional
-- @/ alpha@, percentages anywhere, and @currentColor@. A trailing ICC colour
-- specification (@#f00 icc-color(...)@) is ignored, as SVG 1.1 allows.
parseColor :: Text -> Maybe ColorSpec
parseColor t0 =
  let t = T.strip t0
      lt = T.toLower t
  in case T.uncons t of
       Nothing -> Nothing
       Just ('#', r) -> CSColor <$> hexColor (T.takeWhile isHexDigit r)
                          >>= \c -> if validTail (T.dropWhile isHexDigit r) then Just c else Nothing
       _ | lt == "currentcolor" -> Just CSCurrent
         | Just inner <- fn "rgba(" lt <|> fn "rgb(" lt -> CSColor <$> rgbFn inner
         | Just inner <- fn "hsla(" lt <|> fn "hsl(" lt -> CSColor <$> hslFn inner
         | otherwise ->
             let (w, rest) = T.break isSpace lt
             in if validTail rest then CSColor <$> namedColor w else Nothing
  where
    Nothing <|> b = b
    a <|> _       = a
    validTail r = let r' = T.strip r in T.null r' || "icc-color(" `T.isPrefixOf` r'
    fn pre s = do
      r <- T.stripPrefix pre s
      let (inner, post) = T.breakOn ")" r
      if T.null post || not (validTail (T.drop 1 post)) then Nothing else Just inner
    hexColor h = case map digitToInt (T.unpack h) of
      [r, g, b]          -> Just (c4 (r * 17) (g * 17) (b * 17) 255)
      [r, g, b, a]       -> Just (c4 (r * 17) (g * 17) (b * 17) (a * 17))
      [r1, r2, g1, g2, b1, b2] -> Just (c4 (r1 * 16 + r2) (g1 * 16 + g2) (b1 * 16 + b2) 255)
      [r1, r2, g1, g2, b1, b2, a1, a2] -> Just (c4 (r1 * 16 + r2) (g1 * 16 + g2) (b1 * 16 + b2) (a1 * 16 + a2))
      _ -> Nothing
    c4 r g b a = Color (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) (fromIntegral a / 255)
    -- Arguments: comma-separated or space-separated with "/ alpha".
    args inner =
      let (main, alpha) = T.breakOn "/" inner
          comps = filter (not . T.null) (T.split (\c -> c == ',' || isSpace c) (T.strip main))
          alphaT = if T.null alpha then [] else filter (not . T.null) [T.strip (T.drop 1 alpha)]
      in (comps, alphaT)
    comp :: Double -> Text -> Maybe Double   -- scale for plain numbers
    comp scale s = case parseNumber s of
      Just (v, r) | r == "%"   -> Just (clamp01 (v / 100))
                  | T.null r   -> Just (clamp01 (v / scale))
      _ -> Nothing
    alphaOf s = case parseNumber s of
      Just (v, r) | r == "%" -> Just (clamp01 (v / 100))
                  | T.null r -> Just (clamp01 v)
      _ -> Nothing
    rgbFn inner = do
      let (cs, as) = args inner
      (rgb, a) <- case (cs, as) of
        ([r, g, b], [])    -> Just ([r, g, b], Nothing)
        ([r, g, b], [a])   -> Just ([r, g, b], Just a)
        ([r, g, b, a], []) -> Just ([r, g, b], Just a)
        _                  -> Nothing
      [r, g, b] <- mapM (comp 255) rgb
      al <- maybe (Just 1) alphaOf a
      Just (Color r g b al)
    hslFn inner = do
      let (cs, as) = args inner
      (hslT, a) <- case (cs, as) of
        ([h, s, l], [])    -> Just ((h, s, l), Nothing)
        ([h, s, l], [a])   -> Just ((h, s, l), Just a)
        ([h, s, l, a], []) -> Just ((h, s, l), Just a)
        _                  -> Nothing
      let (hT, sT, lT) = hslT
      h <- hue hT
      s <- pct sT
      l <- pct lT
      al <- maybe (Just 1) alphaOf a
      let (r, g, b) = hslToRgb h s l
      Just (Color r g b al)
    hue s = case parseNumber s of
      Just (v, r) | T.null r || r == "deg" -> Just v
                  | r == "rad"  -> Just (v * 180 / pi)
                  | r == "grad" -> Just (v * 0.9)
                  | r == "turn" -> Just (v * 360)
      _ -> Nothing
    pct s = case parseNumber s of
      Just (v, r) | r == "%" || T.null r -> Just (clamp01 (v / 100))
      _ -> Nothing

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

-- | HSL (hue in degrees, saturation and lightness in [0,1]) to sRGB.
hslToRgb :: Double -> Double -> Double -> (Double, Double, Double)
hslToRgb h0 s l =
  let h = (h0 - 360 * fromIntegral (floor (h0 / 360) :: Int)) / 360
      q = if l < 0.5 then l * (1 + s) else l + s - l * s
      p = 2 * l - q
      f t0 = let t = if t0 < 0 then t0 + 1 else if t0 > 1 then t0 - 1 else t0
             in if t < 1 / 6 then p + (q - p) * 6 * t
                else if t < 1 / 2 then q
                else if t < 2 / 3 then p + (q - p) * (2 / 3 - t) * 6
                else p
  in (f (h + 1 / 3), f h, f (h - 1 / 3))

-- | A named colour (case-insensitive), including @transparent@.
namedColor :: Text -> Maybe Color
namedColor n = M.lookup (T.toLower n) namedTable

namedTable :: M.Map Text Color
namedTable = M.fromList $
  ("transparent", Color 0 0 0 0) :
  [ (T.pack k, Color (ch 0) (ch 2) (ch 4) 1)
  | (k, hex) <- names
  , let ch i = fromIntegral (digitToInt (hex !! i) * 16 + digitToInt (hex !! (i + 1))) / 255 ]
  where
    names :: [(String, String)]
    names =
      [ ("aliceblue","f0f8ff"),("antiquewhite","faebd7"),("aqua","00ffff"),("aquamarine","7fffd4")
      , ("azure","f0ffff"),("beige","f5f5dc"),("bisque","ffe4c4"),("black","000000")
      , ("blanchedalmond","ffebcd"),("blue","0000ff"),("blueviolet","8a2be2"),("brown","a52a2a")
      , ("burlywood","deb887"),("cadetblue","5f9ea0"),("chartreuse","7fff00"),("chocolate","d2691e")
      , ("coral","ff7f50"),("cornflowerblue","6495ed"),("cornsilk","fff8dc"),("crimson","dc143c")
      , ("cyan","00ffff"),("darkblue","00008b"),("darkcyan","008b8b"),("darkgoldenrod","b8860b")
      , ("darkgray","a9a9a9"),("darkgreen","006400"),("darkgrey","a9a9a9"),("darkkhaki","bdb76b")
      , ("darkmagenta","8b008b"),("darkolivegreen","556b2f"),("darkorange","ff8c00"),("darkorchid","9932cc")
      , ("darkred","8b0000"),("darksalmon","e9967a"),("darkseagreen","8fbc8f"),("darkslateblue","483d8b")
      , ("darkslategray","2f4f4f"),("darkslategrey","2f4f4f"),("darkturquoise","00ced1"),("darkviolet","9400d3")
      , ("deeppink","ff1493"),("deepskyblue","00bfff"),("dimgray","696969"),("dimgrey","696969")
      , ("dodgerblue","1e90ff"),("firebrick","b22222"),("floralwhite","fffaf0"),("forestgreen","228b22")
      , ("fuchsia","ff00ff"),("gainsboro","dcdcdc"),("ghostwhite","f8f8ff"),("gold","ffd700")
      , ("goldenrod","daa520"),("gray","808080"),("grey","808080"),("green","008000")
      , ("greenyellow","adff2f"),("honeydew","f0fff0"),("hotpink","ff69b4"),("indianred","cd5c5c")
      , ("indigo","4b0082"),("ivory","fffff0"),("khaki","f0e68c"),("lavender","e6e6fa")
      , ("lavenderblush","fff0f5"),("lawngreen","7cfc00"),("lemonchiffon","fffacd"),("lightblue","add8e6")
      , ("lightcoral","f08080"),("lightcyan","e0ffff"),("lightgoldenrodyellow","fafad2"),("lightgray","d3d3d3")
      , ("lightgreen","90ee90"),("lightgrey","d3d3d3"),("lightpink","ffb6c1"),("lightsalmon","ffa07a")
      , ("lightseagreen","20b2aa"),("lightskyblue","87cefa"),("lightslategray","778899"),("lightslategrey","778899")
      , ("lightsteelblue","b0c4de"),("lightyellow","ffffe0"),("lime","00ff00"),("limegreen","32cd32")
      , ("linen","faf0e6"),("magenta","ff00ff"),("maroon","800000"),("mediumaquamarine","66cdaa")
      , ("mediumblue","0000cd"),("mediumorchid","ba55d3"),("mediumpurple","9370db"),("mediumseagreen","3cb371")
      , ("mediumslateblue","7b68ee"),("mediumspringgreen","00fa9a"),("mediumturquoise","48d1cc"),("mediumvioletred","c71585")
      , ("midnightblue","191970"),("mintcream","f5fffa"),("mistyrose","ffe4e1"),("moccasin","ffe4b5")
      , ("navajowhite","ffdead"),("navy","000080"),("oldlace","fdf5e6"),("olive","808000")
      , ("olivedrab","6b8e23"),("orange","ffa500"),("orangered","ff4500"),("orchid","da70d6")
      , ("palegoldenrod","eee8aa"),("palegreen","98fb98"),("paleturquoise","afeeee"),("palevioletred","db7093")
      , ("papayawhip","ffefd5"),("peachpuff","ffdab9"),("peru","cd853f"),("pink","ffc0cb")
      , ("plum","dda0dd"),("powderblue","b0e0e6"),("purple","800080"),("rebeccapurple","663399")
      , ("red","ff0000"),("rosybrown","bc8f8f"),("royalblue","4169e1"),("saddlebrown","8b4513")
      , ("salmon","fa8072"),("sandybrown","f4a460"),("seagreen","2e8b57"),("seashell","fff5ee")
      , ("sienna","a0522d"),("silver","c0c0c0"),("skyblue","87ceeb"),("slateblue","6a5acd")
      , ("slategray","708090"),("slategrey","708090"),("snow","fffafa"),("springgreen","00ff7f")
      , ("steelblue","4682b4"),("tan","d2b48c"),("teal","008080"),("thistle","d8bfd8")
      , ("tomato","ff6347"),("turquoise","40e0d0"),("violet","ee82ee"),("wheat","f5deb3")
      , ("white","ffffff"),("whitesmoke","f5f5f5"),("yellow","ffff00"),("yellowgreen","9acd32")
      ]
