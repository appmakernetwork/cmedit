-- | A small, non-validating XML pull parser.
--
-- Three of the editor's reading views — DOCX, XLSX and EPUB — are ZIP
-- containers full of XML, so they all need the same thing: a stream of
-- @(element, attributes, text)@ events. This module is that stream and
-- nothing more.
--
-- Two decisions keep it small, and both are the bargain "Cmedit.Rtf" and
-- "Cmedit.Pdf" already strike with their own formats:
--
--   * __Model what we consume, skip what we do not.__ Comments, processing
--     instructions (including the XML declaration) and @\<!DOCTYPE ...\>@ are
--     skipped wholesale; CDATA sections become text; anything malformed
--     degrades to the text it looks like rather than failing the parse. A
--     reader that cannot fail cannot refuse to open someone's book because
--     one chapter has a stray @&@ in it.
--
--   * __Namespaces by local name__ (for the formats that want it). OOXML and EPUB files disagree about
--     prefixes constantly — @w:p@ in one producer's output, a default
--     namespace and a bare @p@ in another's — so 'localName' throws the
--     prefix away and every consumer matches on the local part. This is what
--     every pragmatic OOXML reader does, and it costs a namespace-resolution
--     table that would buy nothing here: the three formats have no local-name
--     collisions we care about.
--
-- "Cmedit.Svg" is the one consumer that cannot live with that: an Inkscape
-- file puts @sodipodi:cx@ on the very elements whose @cx@ matters, and an
-- Illustrator file wraps its fallback artwork in a namespace switch. It uses
-- 'parseXmlQualified', the same parser with names left exactly as written,
-- and resolves namespaces itself.
--
-- __Internal entities.__ A @\<!DOCTYPE\>@'s internal subset is skipped as
-- markup but its @\<!ENTITY name \"value\"\>@ declarations are kept:
-- Illustrator exports declare @ns_svg@ and friends there and then write
-- @xmlns=\"&ns_svg;\"@. Expansion is one level deep (an entity value is
-- decoded with the built-in table only), which makes the billion-laughs
-- construction inert, but one level is still amplification: a 64 KiB entity
-- referenced a dozen times per attribute, on every element, turns a 20 KB
-- file into gigabytes. So expansion is metered twice: per value (an
-- attribute may grow by at most 'maxXmlAttrExpansion', a text node is held to
-- 'maxXmlTextChars') and per document ('maxEntityExpansion', a budget threaded
-- through the whole parse; past it, references are left as written).
--
-- The event list is __lazy__, deliberately. Consumers bound their own output
-- (a cell count, a character budget) and simply stop demanding events; the
-- parser then never runs over the rest of the member. What is bounded /here/
-- is what a malformed file could make unbounded regardless of the consumer:
-- nesting depth ('maxXmlDepth') and the length of a single text node
-- ('maxXmlTextChars').
--
-- Leaf module: it imports nothing from @Cmedit@, so "Cmedit.Zip", the three
-- format mappers and the test suite can all use it cycle-free.
module Cmedit.Xml
  ( -- * Events
    XmlEvent(..)
    -- * Parsing
  , parseXml
  , parseXmlBytes
  , parseXmlQualified
  , decodeXmlBytes
    -- * Reading events
  , localName
  , xAttr
  , xAttrInt
  , elemText
    -- * Bounds
  , maxXmlDepth
  , maxXmlTextChars
  , maxXmlAttrExpansion
  , maxEntityExpansion
  ) where

import Data.Char (chr, isAlpha, isDigit, isHexDigit, isSpace, digitToInt)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

------------------------------------------------------------------------------
-- Events

-- | One parse event. Element names and attribute names are always /local/
-- names — the prefix, if any, has been dropped (see 'localName').
--
-- A self-closing element @\<br\/\>@ produces 'XStart' immediately followed by
-- 'XEnd', so a consumer tracking nesting never has to special-case it.
-- Likewise a closing tag is reported even when it matches nothing: this
-- parser does not validate, and a consumer that keeps a stack is in a better
-- position to decide what a stray @\<\/b\>@ means than the tokeniser is.
data XmlEvent
  = XStart !Text ![(Text, Text)]  -- ^ Element name and its attributes, in document order.
  | XEnd   !Text                  -- ^ Closing tag (or the close half of a self-closing one).
  | XText  !Text                  -- ^ Character data, with entity references already resolved.
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Bounds

-- | Deepest element nesting the parser will follow. Real OOXML runs to a
-- dozen or so levels and XHTML rarely past thirty; this is far beyond either,
-- and exists so that a file built to nest a million elements makes the parser
-- stop rather than make a consumer's stack grow without limit.
maxXmlDepth :: Int
maxXmlDepth = 256

-- | Longest single run of character data, in characters. A text node longer
-- than this is truncated. (The whole member is already capped by
-- 'Cmedit.Zip.maxMemberBytes'; this bounds what one /node/ can hand a
-- consumer that concatenates.)
maxXmlTextChars :: Int
maxXmlTextChars = 1024 * 1024

------------------------------------------------------------------------------
-- Decoding

-- | Decode a member's bytes to text before parsing.
--
-- The byte-order mark decides, when there is one: OOXML is UTF-8 in practice
-- but EPUB producers do occasionally ship UTF-16, and getting that wrong
-- yields a document of NUL-separated letters rather than an error. Without a
-- BOM the answer is UTF-8 — which is what the XML specification says to
-- assume, and what an @encoding=@ pseudo-attribute almost always confirms.
-- Invalid bytes are replaced rather than fatal, per the module's no-failure
-- rule.
decodeXmlBytes :: BS.ByteString -> Text
decodeXmlBytes bs
  | BS.take 3 bs == BS.pack [0xEF, 0xBB, 0xBF] = utf8 (BS.drop 3 bs)
  | BS.take 2 bs == BS.pack [0xFF, 0xFE]       = TE.decodeUtf16LEWith TEE.lenientDecode (BS.drop 2 bs)
  | BS.take 2 bs == BS.pack [0xFE, 0xFF]       = TE.decodeUtf16BEWith TEE.lenientDecode (BS.drop 2 bs)
  | otherwise                                  = utf8 bs
  where utf8 = TE.decodeUtf8With TEE.lenientDecode

-- | 'decodeXmlBytes' then 'parseXml'.
parseXmlBytes :: BS.ByteString -> [XmlEvent]
parseXmlBytes = parseXml . decodeXmlBytes

------------------------------------------------------------------------------
-- The parser

-- | Parse a document into its event stream. Never fails; see the module
-- header for what that costs and buys. Element and attribute names are local
-- names (see 'localName').
parseXml :: Text -> [XmlEvent]
parseXml = parseXmlWith localName

-- | 'parseXml' with element and attribute names exactly as written,
-- prefixes included (@xlink:href@, @sodipodi:cx@, @xmlns:svg@), for a
-- consumer that resolves namespaces itself.
parseXmlQualified :: Text -> [XmlEvent]
parseXmlQualified = parseXmlWith id

-- The parser's running state for entities: the document's declared
-- entities and what is left of its expansion budget.
data Ents = Ents [(Text, Text)] !Int

parseXmlWith :: (Text -> Text) -> Text -> [XmlEvent]
parseXmlWith qname = content (Ents [] maxEntityExpansion) 0
  where
    content :: Ents -> Int -> Text -> [XmlEvent]
    content ents !d t
      | T.null t  = []
      | otherwise =
          let (txt, rest) = T.break (== '<') t
          in withText ents txt (\e -> markup e d rest)

    withText ents txt k
      | T.null txt = k ents
      | otherwise  = let (v, ents') = expand ents maxXmlTextChars (T.take maxXmlTextChars txt)
                     in XText v : k ents'

    -- At a '<' (or at end of input).
    markup ents@(Ents es bud) !d t
      | T.null t                       = []
      | "<!--"      `T.isPrefixOf` t   = content ents d (after "-->" (T.drop 4 t))
      | "<![CDATA[" `T.isPrefixOf` t   = cdata ents d (T.drop 9 t)
      | "<!"        `T.isPrefixOf` t   = let (es', r) = skipDecl (T.drop 2 t)
                                         in content (Ents (take maxEntities (es ++ es')) bud) d r
      | "<?"        `T.isPrefixOf` t   = content ents d (after "?>" (T.drop 2 t))
      | "</"        `T.isPrefixOf` t   = closeTag ents d (T.drop 2 t)
      | otherwise                      = openTag ents d (T.drop 1 t)

    cdata ents !d t =
      let (cd, rest) = T.breakOn "]]>" t
      in withRaw cd (content ents d (T.drop 3 rest))

    withRaw txt k
      | T.null txt = k
      | otherwise  = XText (T.take maxXmlTextChars txt) : k

    closeTag ents !d t =
      let (nm, rest) = T.span isNameChar t
          rest'      = T.drop 1 (T.dropWhile (/= '>') rest)
      in if T.null nm
           then content ents d rest'
           else XEnd (qname nm) : content ents (max 0 (d - 1)) rest'

    openTag ents !d t =
      let (nm, rest) = T.span isNameChar t
      in if T.null nm
           -- A bare '<' that starts no tag: XML forbids it, real documents
           -- contain it, and the honest rendering is the character itself.
           then XText "<" : content ents d t
           else if d >= maxXmlDepth
                  then []
                  else let (as, selfClose, rest', ents') = attrs ents [] rest
                           nm' = qname nm
                       in XStart nm' as
                            : if selfClose
                                then XEnd nm' : content ents' d rest'
                                else content ents' (d + 1) rest'

    -- Attributes up to '>' or '/>'. Returns them in document order.
    attrs ents acc s =
      let s1 = T.dropWhile isSpace s
      in case T.uncons s1 of
           Nothing        -> (reverse acc, True, T.empty, ents)   -- truncated file
           Just ('>', r)  -> (reverse acc, False, r, ents)
           Just ('/', r)  -> (reverse acc, True, T.drop 1 (T.dropWhile (/= '>') r), ents)
           Just _         ->
             let (nm, s2) = T.span isNameChar s1
             in if T.null nm
                  then attrs ents acc (T.drop 1 s1)        -- junk byte: step over it
                  else let s3 = T.dropWhile isSpace s2
                       in case T.uncons s3 of
                            Just ('=', s4) ->
                              let (v, s5, ents') = attrValue ents (T.dropWhile isSpace s4)
                              in attrs ents' ((qname nm, v) : acc) s5
                            -- A valueless attribute is an HTML habit, not XML;
                            -- accept it as the empty string rather than losing
                            -- the rest of the tag to a parse error.
                            _ -> attrs ents ((qname nm, T.empty) : acc) s2

    attrValue ents s = case T.uncons s of
      Just (q, r) | q == '"' || q == '\'' ->
        let (v, r') = T.break (== q) r
            (v', ents') = attrExpand ents v
        in (v', T.drop 1 r', ents')
      _ -> let (v, r) = T.break (\c -> isSpace c || c == '>' || c == '/') s
               (v', ents') = attrExpand ents v
           in (v', r, ents')
    -- An attribute may be as long as the input makes it (a map's path data
    -- runs to megabytes, and costs nothing it did not already cost to read),
    -- but entities may grow it by at most 'maxXmlAttrExpansion'.
    attrExpand ents v = expand ents (T.length v + maxXmlAttrExpansion) v

    -- Resolve references in one value, capped at @cap@ characters, charging
    -- the document's own entities to the budget.
    expand (Ents es bud) cap v =
      let (out, used) = decodeEntitiesBudget es bud cap v
      in (out, Ents es (bud - used))

-- | How many characters entity expansion may add to one attribute value. Far
-- below a text node's cap: every element can carry several attributes, so
-- this is the figure a hostile document multiplies.
maxXmlAttrExpansion :: Int
maxXmlAttrExpansion = 64 * 1024

-- | Total characters a document's /own/ entities may expand to, across the
-- whole document. Built-in references (@&amp;@, @&#38;@) are not charged:
-- they never produce more than they consume. Past the budget a reference is
-- left as written, which is what an unknown one already does.
maxEntityExpansion :: Int
maxEntityExpansion = 8 * 1024 * 1024

-- | Most internal entities one document may declare.
maxEntities :: Int
maxEntities = 256

-- Everything after the first occurrence of @pat@, or nothing if it is absent
-- (an unterminated comment ends the document, which is what it means).
after :: Text -> Text -> Text
after pat t = let (_, rest) = T.breakOn pat t
              in if T.null rest then T.empty else T.drop (T.length pat) rest

-- @\<!DOCTYPE ...\>@, possibly with an internal subset in brackets that may
-- itself contain '>'. Everything here is declaration, never content; the
-- general entities the subset declares come back alongside the rest of the
-- input (see the module header).
skipDecl :: Text -> ([(Text, Text)], Text)
skipDecl = go []
  where
    go es t = case T.uncons t of
      Nothing         -> (reverse es, T.empty)
      Just ('>', r)   -> (reverse es, r)
      Just ('[', r)   -> let (es', r') = subset es r in go es' r'
      Just ('"', r)   -> go es (T.drop 1 (T.dropWhile (/= '"') r))
      Just ('\'', r)  -> go es (T.drop 1 (T.dropWhile (/= '\'') r))
      Just (_, r)     -> go es r
    -- The internal subset, up to its closing ']'. Quoted literals and
    -- comments may contain ']' and are stepped over whole.
    subset es t = case T.uncons t of
      Nothing        -> (es, T.empty)
      Just (']', r)  -> (es, r)
      Just ('"', r)  -> subset es (T.drop 1 (T.dropWhile (/= '"') r))
      Just ('\'', r) -> subset es (T.drop 1 (T.dropWhile (/= '\'') r))
      _ | "<!--" `T.isPrefixOf` t -> subset es (after "-->" (T.drop 4 t))
        | "<!ENTITY" `T.isPrefixOf` t ->
            let (e, r) = entityDecl (T.drop 8 t) in subset (maybe es (: es) e) r
        | otherwise -> subset es (T.drop 1 t)
    -- @name "value"@ or @name 'value'@; parameter (%) and external (SYSTEM,
    -- PUBLIC) entities are skipped, since nothing here can fetch them.
    entityDecl t0 =
      let t1 = T.dropWhile isSpace t0
          (nm, t2) = T.span isNameChar t1
          t3 = T.dropWhile isSpace t2
          skipRest r = T.drop 1 (T.dropWhile (/= '>') r)
      in case T.uncons t3 of
           Just (q, r) | not (T.null nm), q == '"' || q == '\'' ->
             let (v, r') = T.break (== q) r
             in (Just (nm, T.take maxEntityChars (decodeEntities v)), skipRest (T.drop 1 r'))
           _ -> (Nothing, skipRest t3)

-- | Longest replacement text an internal entity may have.
maxEntityChars :: Int
maxEntityChars = 64 * 1024

isNameChar :: Char -> Bool
isNameChar c = isAlpha c || isDigit c || c `elem` (":_-." :: String) || c > '\x7f'

-- | Strip a namespace prefix: @w:p@ becomes @p@. A name with no colon comes
-- back unchanged, and so does one that is /all/ prefix (@w:@), which no
-- well-formed document contains but a truncated one might.
localName :: Text -> Text
localName t = case T.breakOnEnd ":" t of
  (pre, post) | T.null pre || T.null post -> t
              | otherwise                 -> post

------------------------------------------------------------------------------
-- Reading events

-- | Look up an attribute by local name.
xAttr :: Text -> [(Text, Text)] -> Maybe Text
xAttr k as = lookup k as

-- | An attribute read as a decimal integer, with optional sign. Anything that
-- is not a number — including OOXML's habit of writing measurements as
-- @"1440"@ in one place and @"1440.5"@ in another — yields 'Nothing' or the
-- integer part, never an exception.
xAttrInt :: Text -> [(Text, Text)] -> Maybe Int
xAttrInt k as = xAttr k as >>= readIntT

readIntT :: Text -> Maybe Int
readIntT t0 = case T.uncons (T.strip t0) of
  Just ('-', r) -> negate <$> digits r
  Just ('+', r) -> digits r
  _             -> digits (T.strip t0)
  where
    digits t = let ds = T.takeWhile isDigit t
               in if T.null ds
                    then Nothing
                    -- Clamped: these become indents and font sizes, and a
                    -- corrupt file must not be able to ask for a billion
                    -- columns of padding.
                    else Just (min 100000000 (T.foldl' (\ !a c -> a * 10 + digitToInt c) 0 (T.take 9 ds)))

-- | The concatenated character data of the element the stream is positioned
-- inside, and the events after its closing tag.
--
-- The caller has just consumed the element's 'XStart'; this consumes up to
-- and including its matching 'XEnd', ignoring any child elements' tags but
-- keeping their text. That is exactly what @\<si\>\<t\>a\<\/t\>\<\/si\>@ and
-- @\<title\>...\<\/title\>@ want, and it degrades sanely on an unclosed
-- element (it stops at end of stream).
elemText :: [XmlEvent] -> (Text, [XmlEvent])
elemText = go 0 []
  where
    go :: Int -> [Text] -> [XmlEvent] -> (Text, [XmlEvent])
    go _ acc []               = (T.concat (reverse acc), [])
    go !d acc (XText t : es)  = go d (t : acc) es
    go !d acc (XStart _ _ : es) = go (d + 1) acc es
    go !d acc (XEnd _ : es)
      | d <= 0                = (T.concat (reverse acc), es)
      | otherwise             = go (d - 1) acc es

------------------------------------------------------------------------------
-- Entities

-- | Resolve entity references. Fast path: text with no @&@ in it — which is
-- nearly all of it — comes back untouched and unallocated.
--
-- The named set is the five XML built-ins plus the handful of HTML ones that
-- XHTML chapters use even when they declare no DTD to define them. An
-- unrecognised reference is left alone: showing @&foo;@ is honest, and
-- swallowing it would silently delete text.
decodeEntities :: Text -> Text
decodeEntities t = fst (decodeEntitiesBudget [] 0 maxBound t)

-- | Expand references with a document's own entities (consulted after the
-- built-in names): at most @cap@ characters of output, and at most @budget@
-- of them produced by the document's entities. Returns the text and the
-- budget used.
decodeEntitiesBudget :: [(Text, Text)] -> Int -> Int -> Text -> (Text, Int)
decodeEntitiesBudget ents budget cap t
  | not (T.any (== '&') t) = (T.take cap t, 0)
  | otherwise              = let (parts, used) = go 0 0 t in (T.take cap (T.concat parts), used)
  where
    -- @n@ counts the output so far, @u@ the budget spent.
    go :: Int -> Int -> Text -> ([Text], Int)
    go !n !u s
      | n >= cap = ([], u)
      | otherwise =
      let (pre, rest) = T.break (== '&') s
      in if T.null rest
           then ([pre], u)
           else let (ent, after') = T.break (\c -> c == ';' || c == '&' || isSpace c) (T.drop 1 rest)
                    n' = n + T.length pre
                in case T.uncons after' of
                     Just (';', r)
                       | Just c <- resolve ent -> cons2 pre c (go (n' + T.length c) u r)
                       | Just c <- lookup ent ents
                       , let len = min (T.length c) (cap - n')
                       , u + len <= budget -> cons2 pre (T.take len c) (go (n' + len) (u + len) r)
                     _ -> cons2 pre "&" (go (n' + 1) u (T.drop 1 rest))
    cons2 a b (xs, u) = (a : b : xs, u)

resolve :: Text -> Maybe Text
resolve e = case T.uncons e of
  Nothing -> Nothing
  Just ('#', r)
    | Just ('x', h) <- T.uncons r -> codePoint (hex h)
    | otherwise                   -> codePoint (dec r)
  _ -> lookup e namedEntities
  where
    codePoint (Just n) | n > 0 && n <= 0x10FFFF && not (n >= 0xD800 && n <= 0xDFFF) = Just (T.singleton (chr n))
    codePoint _ = Nothing
    dec s | T.null s || not (T.all isDigit s) = Nothing
          | otherwise = Just (T.foldl' (\ !a c -> min 0x110000 (a * 10 + digitToInt c)) 0 (T.take 8 s))
    hex s | T.null s || not (T.all isHexDigit s) = Nothing
          | otherwise = Just (T.foldl' (\ !a c -> min 0x110000 (a * 16 + digitToInt c)) 0 (T.take 7 s))

namedEntities :: [(Text, Text)]
namedEntities =
  [ ("amp", "&"), ("lt", "<"), ("gt", ">"), ("quot", "\""), ("apos", "'")
  , ("nbsp", "\xa0"), ("shy", "\x00ad")
  , ("ndash", "\x2013"), ("mdash", "\x2014")
  , ("lsquo", "\x2018"), ("rsquo", "\x2019"), ("sbquo", "\x201a")
  , ("ldquo", "\x201c"), ("rdquo", "\x201d"), ("bdquo", "\x201e")
  , ("hellip", "\x2026"), ("bull", "\x2022"), ("middot", "\xb7")
  , ("dagger", "\x2020"), ("Dagger", "\x2021"), ("prime", "\x2032")
  , ("copy", "\xa9"), ("reg", "\xae"), ("trade", "\x2122"), ("deg", "\xb0")
  , ("laquo", "\xab"), ("raquo", "\xbb"), ("times", "\xd7"), ("divide", "\xf7")
  , ("plusmn", "\xb1"), ("frac12", "\xbd"), ("frac14", "\xbc"), ("frac34", "\xbe")
  , ("pound", "\xa3"), ("euro", "\x20ac"), ("yen", "\xa5"), ("cent", "\xa2")
  , ("sect", "\xa7"), ("para", "\xb6"), ("micro", "\xb5"), ("eacute", "\xe9")
  , ("egrave", "\xe8"), ("agrave", "\xe0"), ("ccedil", "\xe7"), ("uuml", "\xfc")
  , ("ouml", "\xf6"), ("auml", "\xe4"), ("szlig", "\xdf"), ("ntilde", "\xf1")
  , ("emsp", "\x2003"), ("ensp", "\x2002"), ("thinsp", "\x2009")
  , ("larr", "\x2190"), ("rarr", "\x2192"), ("harr", "\x2194")
  , ("hearts", "\x2665"), ("star", "\x2606"), ("dash", "\x2010")
  ]
