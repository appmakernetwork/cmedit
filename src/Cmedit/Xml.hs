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
--   * __Namespaces by local name.__ OOXML and EPUB files disagree about
--     prefixes constantly — @w:p@ in one producer's output, a default
--     namespace and a bare @p@ in another's — so 'localName' throws the
--     prefix away and every consumer matches on the local part. This is what
--     every pragmatic OOXML reader does, and it costs a namespace-resolution
--     table that would buy nothing here: the three formats have no local-name
--     collisions we care about.
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
  , decodeXmlBytes
    -- * Reading events
  , localName
  , xAttr
  , xAttrInt
  , elemText
    -- * Bounds
  , maxXmlDepth
  , maxXmlTextChars
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
-- header for what that costs and buys.
parseXml :: Text -> [XmlEvent]
parseXml = content 0
  where
    content :: Int -> Text -> [XmlEvent]
    content !d t
      | T.null t  = []
      | otherwise =
          let (txt, rest) = T.break (== '<') t
          in withText txt (markup d rest)

    withText txt k
      | T.null txt = k
      | otherwise  = XText (decodeEntities (T.take maxXmlTextChars txt)) : k

    -- At a '<' (or at end of input).
    markup !d t
      | T.null t                       = []
      | "<!--"      `T.isPrefixOf` t   = content d (after "-->" (T.drop 4 t))
      | "<![CDATA[" `T.isPrefixOf` t   = cdata d (T.drop 9 t)
      | "<!"        `T.isPrefixOf` t   = content d (skipDecl (T.drop 2 t))
      | "<?"        `T.isPrefixOf` t   = content d (after "?>" (T.drop 2 t))
      | "</"        `T.isPrefixOf` t   = closeTag d (T.drop 2 t)
      | otherwise                      = openTag d (T.drop 1 t)

    cdata !d t =
      let (cd, rest) = T.breakOn "]]>" t
      in withRaw cd (content d (T.drop 3 rest))

    withRaw txt k
      | T.null txt = k
      | otherwise  = XText (T.take maxXmlTextChars txt) : k

    closeTag !d t =
      let (nm, rest) = T.span isNameChar t
          rest'      = T.drop 1 (T.dropWhile (/= '>') rest)
      in if T.null nm
           then content d rest'
           else XEnd (localName nm) : content (max 0 (d - 1)) rest'

    openTag !d t =
      let (nm, rest) = T.span isNameChar t
      in if T.null nm
           -- A bare '<' that starts no tag: XML forbids it, real documents
           -- contain it, and the honest rendering is the character itself.
           then XText "<" : content d t
           else if d >= maxXmlDepth
                  then []
                  else let (as, selfClose, rest') = attrs [] rest
                           nm' = localName nm
                       in XStart nm' as
                            : if selfClose
                                then XEnd nm' : content d rest'
                                else content (d + 1) rest'

    -- Attributes up to '>' or '/>'. Returns them in document order.
    attrs acc s =
      let s1 = T.dropWhile isSpace s
      in case T.uncons s1 of
           Nothing        -> (reverse acc, True, T.empty)   -- truncated file
           Just ('>', r)  -> (reverse acc, False, r)
           Just ('/', r)  -> (reverse acc, True, T.drop 1 (T.dropWhile (/= '>') r))
           Just _         ->
             let (nm, s2) = T.span isNameChar s1
             in if T.null nm
                  then attrs acc (T.drop 1 s1)             -- junk byte: step over it
                  else let s3 = T.dropWhile isSpace s2
                       in case T.uncons s3 of
                            Just ('=', s4) ->
                              let (v, s5) = attrValue (T.dropWhile isSpace s4)
                              in attrs ((localName nm, v) : acc) s5
                            -- A valueless attribute is an HTML habit, not XML;
                            -- accept it as the empty string rather than losing
                            -- the rest of the tag to a parse error.
                            _ -> attrs ((localName nm, T.empty) : acc) s2

    attrValue s = case T.uncons s of
      Just (q, r) | q == '"' || q == '\'' ->
        let (v, r') = T.break (== q) r in (decodeEntities v, T.drop 1 r')
      _ -> let (v, r) = T.break (\c -> isSpace c || c == '>' || c == '/') s
           in (decodeEntities v, r)

-- Everything after the first occurrence of @pat@, or nothing if it is absent
-- (an unterminated comment ends the document, which is what it means).
after :: Text -> Text -> Text
after pat t = let (_, rest) = T.breakOn pat t
              in if T.null rest then T.empty else T.drop (T.length pat) rest

-- @\<!DOCTYPE ...\>@, possibly with an internal subset in brackets that may
-- itself contain '>'. Everything here is declaration, never content.
skipDecl :: Text -> Text
skipDecl = go
  where
    go t = case T.uncons t of
      Nothing         -> T.empty
      Just ('>', r)   -> r
      Just ('[', r)   -> go (T.drop 1 (T.dropWhile (/= ']') r))
      Just ('"', r)   -> go (T.drop 1 (T.dropWhile (/= '"') r))
      Just ('\'', r)  -> go (T.drop 1 (T.dropWhile (/= '\'') r))
      Just (_, r)     -> go r

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
decodeEntities t
  | not (T.any (== '&') t) = t
  | otherwise              = T.concat (go t)
  where
    go s =
      let (pre, rest) = T.break (== '&') s
      in if T.null rest
           then [pre]
           else let (ent, after') = T.break (\c -> c == ';' || c == '&' || isSpace c) (T.drop 1 rest)
                in case T.uncons after' of
                     Just (';', r) | Just c <- resolve ent -> pre : c : go r
                     _ -> pre : "&" : go (T.drop 1 rest)

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
