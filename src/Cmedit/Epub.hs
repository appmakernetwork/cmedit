-- | Reading an e-book: an EPUB's chapters mapped onto the formatted view's
-- paragraph model.
--
-- An EPUB is a ZIP of XHTML. Once the markup is off, a chapter is what a
-- @.docx@ body and an @.rtf@ file are — paragraphs of styled runs with
-- alignment and indentation — so this module, like "Cmedit.Docx", is a
-- /mapping/ and not a view: "Cmedit.Rtf" lays the paragraphs out, the
-- renderer draws them, and the reading view is the RTF one with
-- 'Cmedit.Rtf.RtfFromContainer' as its origin.
--
-- What EPUB adds that neither of the others has is __structure__. A book is
-- an ordered list of chapters, so they become the formatted view's /sections/
-- ('Cmedit.Rtf.rdSects'): @[@ and @]@ turn them, the status bar says which one
-- you are in, and Go To reads a chapter number — the same treatment
-- "Cmedit.Pdf" gives pages, and for the same reason. A laid-out line number
-- moves with the window width and would mean nothing to anyone.
--
-- Three small pieces of container plumbing get from the archive to the text,
-- and any of them failing degrades to the archive listing rather than to an
-- error:
--
--   1. @META-INF\/container.xml@ names the package document ('epubOpfPath').
--   2. The package document's manifest and spine give the chapters, in
--      reading order ('epubSpine') — the spine, not the manifest, because the
--      manifest is a bag and its order means nothing.
--   3. Each chapter is XHTML, mapped by 'htmlPars'.
--
-- Path resolution is the fiddly part and is done in one place ('resolveHref'):
-- hrefs are relative to the package document's own directory, may be
-- percent-encoded, and may carry a fragment. Getting any of those wrong loses
-- a chapter silently, which is why it is one function with tests rather than
-- three call sites.
module Cmedit.Epub
  ( -- * Detection
    isEpub
  , containerPath
    -- * The container
  , epubOpfPath
  , epubSpine
  , epubTitle
  , resolveHref
    -- * Chapters
  , htmlPars
  , htmlTitle
  , maxEpubChapters
  , maxEpubChars
  ) where

import qualified Data.ByteString as BS
import Data.Char (chr, isHexDigit, isSpace, digitToInt)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Rtf
  (RtfAlign(..), RtfFmt(..), RtfPar(..), RtfRun(..), defaultFmt, defaultPar)
import Cmedit.Xml (XmlEvent(..), elemText, parseXmlBytes, xAttr)

------------------------------------------------------------------------------
-- Detection

-- | The member every EPUB must contain, and the entry point to the rest of it.
containerPath :: Text
containerPath = "META-INF/container.xml"

-- | Does this archive's table of contents look like an e-book?
--
-- The @mimetype@ member is EPUB's own signature (a stored, uncompressed
-- member holding exactly @application\/epub+zip@), but plenty of real books
-- are repackaged by tools that drop or recompress it, so the container
-- document — which nothing can read the book without — is the test that
-- actually holds.
isEpub :: [Text] -> Bool
isEpub names = containerPath `elem` names

------------------------------------------------------------------------------
-- Bounds

-- | Most chapters a book will be read from. Well past any real book's spine,
-- and a bound on what a generated file can make the loader do.
maxEpubChapters :: Int
maxEpubChapters = 5000

-- | Most characters of body text a book will yield across all its chapters.
-- The loader stops here and says so; a partial book still reads.
maxEpubChars :: Int
maxEpubChars = 16 * 1024 * 1024

------------------------------------------------------------------------------
-- The container

-- | The package document's path, from @META-INF\/container.xml@.
--
-- A container may list several root files (an EPUB may ship the same book in
-- more than one rendition); the first is the one to read, which is what the
-- specification says and what every reader does.
epubOpfPath :: BS.ByteString -> Maybe Text
epubOpfPath bs = case [ p | XStart "rootfile" as <- parseXmlBytes bs
                          , Just p <- [xAttr "full-path" as], not (T.null p) ] of
  (p : _) -> Just (normalisePath (percentDecode p))
  []      -> Nothing

-- | The book's chapters, in reading order, as archive member paths.
--
-- @base@ is the package document's directory (@\"\"@ when it sits at the
-- archive root). Only spine items that resolve to a manifest entry come back,
-- and only ones that are documents: an EPUB's manifest also holds its images,
-- fonts and style sheets, none of which are chapters.
epubSpine :: Text -> BS.ByteString -> [Text]
epubSpine base bs =
  [ resolveHref base href
  | idref <- spine
  , Just (href, mt) <- [lookup idref manifest]
  , isDocType mt || isDocHref href ]
  where
    evs = parseXmlBytes bs
    manifest = [ (i, (h, maybe "" id (xAttr "media-type" as)))
               | XStart "item" as <- evs
               , Just i <- [xAttr "id" as], Just h <- [xAttr "href" as] ]
    spine = [ i | XStart "itemref" as <- evs, Just i <- [xAttr "idref" as] ]
    isDocType mt = mt `elem` [ "application/xhtml+xml", "text/html"
                             , "application/x-dtbook+xml", "text/x-oeb1-document" ]
    -- Producers do get the media type wrong; the extension is the tiebreak,
    -- and a spine item is a document by definition anyway.
    isDocHref h = any (`T.isSuffixOf` T.toLower (T.takeWhile (/= '#') h))
                      [".xhtml", ".html", ".htm", ".xml"]

-- | The book's title, from the package document's Dublin Core metadata.
epubTitle :: BS.ByteString -> Text
epubTitle = firstTitle . parseXmlBytes

-- | Resolve a manifest href against the package document's directory.
--
-- Percent-decoding first (a chapter really can be called @ch%201.xhtml@),
-- then the fragment goes (it addresses a place /within/ a chapter, not a
-- different member), then @.@ and @..@ segments are folded away. An absolute
-- href — one starting with @\/@ — is relative to the archive root, not to the
-- package document.
resolveHref :: Text -> Text -> Text
resolveHref base href0
  | T.null href             = T.empty
  | "/" `T.isPrefixOf` href = normalisePath (T.drop 1 href)
  | T.null base             = normalisePath href
  | otherwise               = normalisePath (base <> "/" <> href)
  where href = T.takeWhile (/= '#') (percentDecode href0)

-- Fold @.@ and @..@ segments away. A @..@ that would escape the archive root
-- is dropped rather than kept: there is nothing above the root to reach.
normalisePath :: Text -> Text
normalisePath = T.intercalate "/" . reverse . foldl' step [] . T.split (== '/')
  where
    step acc seg = case seg of
      ""   -> acc
      "."  -> acc
      ".." -> drop 1 acc
      _    -> seg : acc

percentDecode :: Text -> Text
percentDecode t
  | not (T.any (== '%') t) = t
  | otherwise              = T.pack (go (T.unpack t))
  where
    go ('%' : a : b : rest) | isHexDigit a && isHexDigit b =
      chr (16 * digitToInt a + digitToInt b) : go rest
    go (c : rest) = c : go rest
    go []         = []

------------------------------------------------------------------------------
-- XHTML to paragraphs

-- | A chapter's @\<title\>@, for the section list. Empty when it has none.
htmlTitle :: BS.ByteString -> Text
htmlTitle = firstTitle . parseXmlBytes

firstTitle :: [XmlEvent] -> Text
firstTitle = go
  where
    go (XStart "title" _ : es) = T.strip (collapse (fst (elemText es)))
    go (XStart "body" _ : _)   = T.empty   -- past the head: there is none
    go (_ : es)                = go es
    go []                      = T.empty

-- | Map an XHTML chapter to paragraphs.
--
-- The subset is deliberately small — the block elements that end a paragraph,
-- the inline elements a terminal can express, lists, block quotes,
-- preformatted text and tables. Everything else is /unwrapped/: an unknown
-- element's text still appears, in its parent's paragraph, which is the right
-- default for the endless @\<span\>@s and @\<a\>@s real books are made of.
-- Only the handful of subtrees that are not prose at all ('htmlSkip') are
-- dropped.
htmlPars :: BS.ByteString -> Seq RtfPar
htmlPars bs = hsOut (endHPar (foldl' hstep hs0 (parseXmlBytes bs)))

-- Subtrees whose character data is not text to read. @rt@ and @rp@ are ruby
-- annotations — pronunciation glosses printed above the text, which inline
-- would read as noise between every pair of characters.
htmlSkip :: [Text]
htmlSkip = ["head", "script", "style", "svg", "template", "noscript", "rt", "rp"]

-- Elements that end the paragraph in progress when they open and when they
-- close.
htmlBlocks :: [Text]
htmlBlocks =
  [ "p", "div", "h1", "h2", "h3", "h4", "h5", "h6", "li", "blockquote", "pre"
  , "section", "article", "aside", "header", "footer", "nav", "figure"
  , "figcaption", "dd", "dt", "dl", "ul", "ol", "table", "tr", "caption"
  , "address", "main", "body", "hgroup", "details", "summary", "center"
  ]

headings :: [Text]
headings = ["h1", "h2", "h3", "h4", "h5", "h6"]

-- Inline elements whose formatting nests: @\<b\>a\<i\>b\<\/i\>c\<\/b\>@ must
-- leave @c@ bold, which a stack gives and a flat \"current format\" does not.
inlineFmts :: [(Text, RtfFmt -> RtfFmt)]
inlineFmts =
  [ ("b",      \f -> f { rfBold = True })
  , ("strong", \f -> f { rfBold = True })
  , ("i",      \f -> f { rfItalic = True })
  , ("em",     \f -> f { rfItalic = True })
  , ("cite",   \f -> f { rfItalic = True })
  , ("dfn",    \f -> f { rfItalic = True })
  , ("var",    \f -> f { rfItalic = True })
  , ("u",      \f -> f { rfUnder = True })
  , ("ins",    \f -> f { rfUnder = True })
  , ("s",      \f -> f { rfStrike = True })
  , ("del",    \f -> f { rfStrike = True })
  , ("strike", \f -> f { rfStrike = True })
  ]

data HS = HS
  { hsStack :: ![Text]
  , hsSkip  :: !(Maybe Int)    -- ^ Stack depth at which a skipped subtree began.
  , hsFmts  :: ![RtfFmt]       -- ^ Formatting stack; the head is in force.
  , hsPar   :: !RtfPar         -- ^ Block properties of the paragraph in progress.
  , hsRuns  :: ![RtfRun]       -- ^ Completed runs, reversed.
  , hsChars :: ![Text]         -- ^ Text of the run in progress, reversed.
  , hsRunF  :: !RtfFmt         -- ^ Formatting that text was written with.
  , hsOut   :: !(Seq RtfPar)
  , hsPre   :: !Int            -- ^ @pre@ nesting: whitespace is significant inside.
  , hsLists :: ![(Bool, Int)]  -- ^ Open lists, innermost first: @(ordered, items so far)@.
  , hsCell  :: !Int            -- ^ Cells closed in the current table row.
  , hsQuote :: !Int            -- ^ Open @blockquote@ nesting; a block indent that outlives the paragraph inside it.
  , hsTight :: !Bool           -- ^ This paragraph is a list item or table row: set close, not spaced.
  , hsWs    :: !Bool           -- ^ The paragraph so far ends in whitespace, so another space would be redundant.
  , hsAny   :: !Bool           -- ^ Anything at all has been emitted into it.
  , hsMark  :: !Text           -- ^ A list marker held back until the item's first text arrives.
  , hsChars2:: !Int            -- ^ Characters emitted (against 'maxEpubChars', per chapter).
  }

hs0 :: HS
hs0 = HS [] Nothing [defaultFmt] defaultPar [] [] defaultFmt Seq.empty 0 [] 0 0
         False True False T.empty 0

curFmt :: HS -> RtfFmt
curFmt st = case hsFmts st of (f : _) -> f; [] -> defaultFmt

popFmt :: HS -> HS
popFmt st = st { hsFmts = case hsFmts st of
                            (_ : rest@(_ : _)) -> rest
                            other              -> other }

hstep :: HS -> XmlEvent -> HS
hstep st ev = case ev of
  XText t
    | hsSkip st /= Nothing -> st
    | otherwise            -> hemit t st

  XStart nm as ->
    let st' = st { hsStack = nm : hsStack st }
    in if hsSkip st' /= Nothing then st' else hstart nm as st'

  XEnd nm ->
    let d = length (hsStack st)
    in case hsSkip st of
         Just k | d <= k -> pop st { hsSkip = Nothing }
         Just _          -> pop st
         Nothing         -> pop (hend nm st)
  where pop s = s { hsStack = drop 1 (hsStack s) }

hstart :: Text -> [(Text, Text)] -> HS -> HS
hstart nm as st
  | nm `elem` htmlSkip = st { hsSkip = Just (length (hsStack st)) }
  | Just f <- lookup nm inlineFmts = st { hsFmts = f (curFmt st) : hsFmts st }
  | otherwise = case nm of
      "br"  -> hemitRaw "\n" st
      "hr"  -> let st' = endHPar st
               in st' { hsOut = hsOut st' |> defaultPar { rpBreak = True } }
      "img" -> altText as st
      "wbr" -> st

      "ul"  -> (endHPar st) { hsLists = (False, 0) : hsLists st }
      "ol"  -> (endHPar st) { hsLists = (True, 0) : hsLists st }
      "li"  -> listItem (endHPar st)
      "pre" -> (endHPar st) { hsPre = hsPre st + 1 }
      -- The indent belongs to the quote, not to a paragraph: a blockquote
      -- holds paragraphs, and each of those resets 'hsPar'. So it is depth
      -- that is tracked and 'endHPar' that applies it.
      "blockquote" -> (endHPar st) { hsQuote = hsQuote st + 1 }

      "td"  -> if hsCell st > 0 then hemitRaw "\t" st else st
      "th"  -> if hsCell st > 0 then hemitRaw "\t" st else st
      "tr"  -> (endHPar st) { hsCell = 0, hsTight = True }

      "center" -> let st' = endHPar st
                  in st' { hsPar = (hsPar st') { rpAlign = AlignCenter } }

      _ | nm `elem` headings ->
            let st' = endHPar st
            in st' { hsFmts = (curFmt st') { rfSize = headingSizeOf nm } : hsFmts st'
                   , hsPar  = (hsPar st') { rpAlign = alignAttr as (rpAlign (hsPar st')) } }
        | nm `elem` htmlBlocks ->
            let st' = endHPar st
            in st' { hsPar = (hsPar st') { rpAlign = alignAttr as (rpAlign (hsPar st')) } }
        | otherwise -> st

hend :: Text -> HS -> HS
hend nm st
  | nm `elem` map fst inlineFmts = popFmt st
  | otherwise = case nm of
      "ul"  -> (endHPar st) { hsLists = drop 1 (hsLists st) }
      "ol"  -> (endHPar st) { hsLists = drop 1 (hsLists st) }
      "pre" -> (endHPar st) { hsPre = max 0 (hsPre st - 1) }
      "blockquote" -> (endHPar st) { hsQuote = max 0 (hsQuote st - 1) }
      "td"  -> st { hsCell = hsCell st + 1 }
      "th"  -> st { hsCell = hsCell st + 1 }
      _ | nm `elem` headings   -> endHPar (popFmt st)
        | nm `elem` htmlBlocks -> endHPar st
        | otherwise            -> st

headingSizeOf :: Text -> Int
headingSizeOf nm = case nm of
  "h1" -> 36
  "h2" -> 32
  "h3" -> 30
  _    -> 28

-- @align="center"@ is long-deprecated HTML and still all over converted books.
alignAttr :: [(Text, Text)] -> RtfAlign -> RtfAlign
alignAttr as dflt = case fmap T.toLower (xAttr "align" as) of
  Just "center"  -> AlignCenter
  Just "right"   -> AlignRight
  Just "justify" -> AlignJustify
  Just "left"    -> AlignLeft
  _              -> dflt

-- An image's alternative text is the only thing about it a terminal can show,
-- and in an illustrated book it is often the caption.
altText :: [(Text, Text)] -> HS -> HS
altText as st = case xAttr "alt" as of
  Just a | not (T.null (T.strip a)) -> hemit ("[" <> T.strip a <> "]") st
  _                                 -> st

-- A list item's marker: the number for an ordered list, a bullet otherwise.
-- Held back until the item's first text arrives ('hsMark'), so an empty
-- @\<li\>@ leaves no stray bullet behind. The hanging first-line indent is
-- what lines a wrapped item's continuation up under its own text.
listItem :: HS -> HS
listItem st = case hsLists st of
  ((ord, n) : rest) ->
    st { hsLists = (ord, n + 1) : rest
       , hsMark  = if ord then T.pack (show (n + 1) ++ ". ") else "\x2022 "
       , hsTight = True
       , hsPar   = (hsPar st) { rpLeft = 360 * length (hsLists st), rpFirst = -360 } }
  [] -> st { hsMark = "\x2022 ", hsTight = True
           , hsPar = (hsPar st) { rpLeft = 360, rpFirst = -360 } }

-- Character data. Outside @\<pre\>@, HTML collapses every run of whitespace to
-- one space and drops it at the start of a block — which is what turns a
-- pretty-printed chapter's indentation into nothing rather than into a ragged
-- left margin.
hemit :: Text -> HS -> HS
hemit t st
  | hsPre st > 0 = hemitRaw t st
  | T.null t     = st
  | otherwise =
      let c  = collapse t
          c' = if hsWs st then T.dropWhile (== ' ') c else c
      in if T.null c' then st else hemitRaw c' st

hemitRaw :: Text -> HS -> HS
hemitRaw t st
  | T.null t                    = st
  | hsChars2 st >= maxEpubChars = st
  | otherwise =
      let st1 | T.null (hsMark st) = st
              | otherwise          = (pushRun (hsMark st) defaultFmt st) { hsMark = T.empty }
          st2 = pushRun t (curFmt st1) st1
      in st2 { hsWs    = T.null (T.dropWhileEnd (\c -> c == ' ' || c == '\n') t)
                         || " " `T.isSuffixOf` t || "\n" `T.isSuffixOf` t
             , hsAny   = True
             , hsChars2 = hsChars2 st + T.length t }

pushRun :: Text -> RtfFmt -> HS -> HS
pushRun t f st
  | not (null (hsChars st)), f == hsRunF st = st { hsChars = t : hsChars st }
  | otherwise = let st' = flushHRun st in st' { hsChars = [t], hsRunF = f }

flushHRun :: HS -> HS
flushHRun st = case hsChars st of
  [] -> st
  ps -> st { hsRuns = RtfRun (joinPieces ps) (hsRunF st) : hsRuns st, hsChars = [] }
  where
    -- Detached: one retained slice pins the whole decoded chapter.
    joinPieces [x] = T.copy x
    joinPieces xs  = T.concat (reverse xs)

-- Close the paragraph in progress.
--
-- An empty block — a wrapper @\<div\>@, the @\<ul\>@ around its items, the
-- whitespace between two @\<p\>@s — produces no paragraph at all. That is what
-- makes 'rpSpace' safe to set on every real paragraph: exactly one blank line
-- between them, never a run of them, whatever the chapter's markup looks like.
endHPar :: HS -> HS
endHPar st0' =
  let st = flushHRun st0'
      keep = hsAny st || rpBreak (hsPar st)
      par  = (hsPar st) { rpRuns = reverse (hsRuns st)
                        , rpSpace = not (hsTight st)
                        , rpLeft = rpLeft (hsPar st) + 360 * hsQuote st }
  in st { hsOut   = if keep then hsOut st |> par else hsOut st
        , hsRuns  = [], hsChars = [], hsPar = defaultPar
        , hsTight = False, hsWs = True, hsAny = False, hsMark = T.empty }

-- Collapse every run of whitespace (including the newlines a pretty-printed
-- chapter is full of) to a single space, keeping one at each end where there
-- was any: the space between @\<em\>word\<\/em\>@ and the next one lives in a
-- text node that starts with it.
collapse :: Text -> Text
collapse t
  | T.all (== ' ') spaces, not ("  " `T.isInfixOf` t) = t
  | otherwise = T.pack (go (T.unpack t))
  where
    spaces = T.filter isSpace t
    go (c : cs) | isSpace c = ' ' : go (dropWhile isSpace cs)
                | otherwise = c : go cs
    go []                   = []
