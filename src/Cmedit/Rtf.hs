-- | The RTF \"formatted\" view: reading a word-processor document in a terminal.
--
-- Rich Text Format is plain ASCII with markup, so an @.rtf@ file already opens
-- as text — but what you see is @{\\rtf1\\ansi...@, not the document. This
-- module is the pure half of showing the document instead: a parser from RTF
-- bytes to a paragraph model, and a layout pass from paragraphs to laid-out
-- lines carrying per-character formatting for the renderer.
--
-- Two properties keep it tractable:
--
--   * __Unknown markup is skippable by construction.__ RTF defines
--     @{\\*\\anything ...}@ as \"ignore this whole group if you do not
--     recognise the control word\", so a reader can decline to understand most
--     of a document and still render its text correctly. Everything this
--     module does not model — style sheets, font tables, embedded pictures,
--     revision marks, OLE objects — is skipped by that rule or by the
--     'skipDestinations' list, not mis-rendered.
--
--   * __The view is read-only and derived.__ The line buffer remains the
--     document, and the only thing that is ever saved. This model is a
--     projection built from the buffer when the view is entered and thrown
--     away when it is left, so there is no serialisation path back to RTF and
--     therefore no way for the formatting we do not model to be dropped on
--     save. That is deliberate: a lossy round-trip through a partial model
--     would quietly destroy the parts of someone's document this module cannot
--     see. Editing happens in the raw text view (Alt+T toggles).
--
-- Compare "Cmedit.Csv", whose table /is/ the text and so is edited and written
-- back, and "Cmedit.Pager", the other read-only view.
module Cmedit.Rtf
  ( -- * The document model
    RtfDoc(..)
  , RtfPar(..)
  , RtfRun(..)
  , RtfFmt(..)
  , RtfAlign(..)
  , defaultFmt
  , defaultPar
    -- * Parsing
  , parseRtf
  , looksLikeRtf
  , maxRtfChars
    -- * The view
  , mkRtfDoc
  , rtfStale
  , rtfRelayout
  , rtfLines
  , rtfLineCount
  , rtfParCount
    -- * Laid-out lines (what the renderer draws)
  , RtfLine(..)
  , layoutRtf
  , twipsToCols
    -- * Movement (all clamped; the view is read-only)
  , rtfScroll
  , rtfGoTop
  , rtfGoBottom
  , rtfClamp
    -- * Presentation
  , rtfStatus
  ) where

import Data.Char (chr, digitToInt, isAlpha, isDigit, isHexDigit)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

import Cmedit.Types (Color(..), ptrEq)
import Cmedit.Width (lineDisplayWidth, wrapLine)

------------------------------------------------------------------------------
-- The model

-- | Character formatting. Group-scoped while parsing (an RTF group inherits
-- its parent's formatting and its changes end with the group), then frozen
-- onto each run.
data RtfFmt = RtfFmt
  { rfBold   :: !Bool
  , rfItalic :: !Bool
  , rfUnder  :: !Bool
  , rfStrike :: !Bool
  , rfColor  :: !(Maybe Color)  -- ^ From the colour table via @\\cf@; 'Nothing' is \"auto\" (the theme's text colour).
  , rfSize   :: !Int            -- ^ Font size in half-points (@\\fs@); 0 when unspecified. 24 (12pt) is the usual body size.
  } deriving (Eq, Show)

defaultFmt :: RtfFmt
defaultFmt = RtfFmt False False False False Nothing 0

-- | Paragraph alignment (@\\ql \\qc \\qr \\qj@). Justified lays out as
-- left-aligned: padding between words to a flush right edge reads badly at
-- terminal column widths.
data RtfAlign = AlignLeft | AlignCenter | AlignRight | AlignJustify
  deriving (Eq, Show)

-- | One paragraph: its block properties plus the styled runs of its text.
-- A run's text may contain @\\n@ (from @\\line@, a hard break inside a
-- paragraph); layout splits on those before wrapping.
data RtfPar = RtfPar
  { rpAlign :: !RtfAlign
  , rpLeft  :: !Int        -- ^ Left indent in twips (@\\li@).
  , rpFirst :: !Int        -- ^ First-line indent in twips (@\\fi@); negative is a hanging indent.
  , rpRuns  :: ![RtfRun]
  , rpBreak :: !Bool       -- ^ A page or section break (@\\page@, @\\sect@) follows this paragraph; drawn as a rule beneath it.
  } deriving (Eq, Show)

defaultPar :: RtfPar
defaultPar = RtfPar AlignLeft 0 0 [] False

data RtfRun = RtfRun { rrText :: !Text, rrFmt :: !RtfFmt }
  deriving (Eq, Show)

-- | One laid-out line: what the renderer draws on one terminal row. @rlSpans@
-- are character ranges into @rlText@ (half-open, in order, covering it), which
-- is exactly the shape 'Cmedit.Render.expandLineCellsFrom' consumes as its
-- @baseAt@ lookup.
data RtfLine = RtfLine
  { rlPad   :: !Int                    -- ^ Leading blank columns (indent + alignment).
  , rlText  :: !Text
  , rlSpans :: ![(Int, Int, RtfFmt)]
  , rlRule  :: !Bool                   -- ^ Draw as a horizontal rule (a page break), ignoring the text.
  } deriving (Eq, Show)

-- | The RTF view of a document: the parsed paragraphs, the scroll position,
-- and the laid-out lines cached against the width they were laid out for.
--
-- Scrolling never re-lays out (the cache key is width and tab size only); a
-- resize does, which is the same bargain 'Cmedit.EditorState.refreshImage'
-- makes for the image view.
data RtfDoc = RtfDoc
  { rdPars   :: !(Seq RtfPar)
  , rdTop    :: !Int    -- ^ First visible laid-out line.
  , rdCache  :: !(Maybe (Int, Int, Seq RtfLine))  -- ^ (width, tab width, lines).
  , rdSeq    :: !Int
    -- ^ The editor's edit counter when this was parsed, so the view can tell
    -- when the buffer has moved under it ('rtfStale').
    --
    -- This deliberately does /not/ compare the buffer. The obvious
    -- implementation — keep the line 'Seq' and check pointer identity, the
    -- way "Cmedit.Syntax"'s cache does — silently fails here: under @-O2@
    -- 'Cmedit.Types.ptrEq' on a lifted value reports a mismatch even for a
    -- buffer nothing has touched, and unlike @sameText@ there is no cheap
    -- @(==)@ to fall back to on a whole document. The result was a full
    -- re-parse on every keystroke. An @Int@ comparison cannot lie.
  } deriving (Show)

-- | Longest document this view will parse, in characters.
--
-- The parser walks the text as a list of characters and builds a paragraph
-- model beside the buffer, so its cost and its memory are both linear in the
-- file. Ordinary RTF documents are a few hundred KB; the cap exists so that
-- pointing this view at something pathological declines rather than stalls.
maxRtfChars :: Int
maxRtfChars = 8 * 1024 * 1024

-- | Does this text actually start like an RTF document? Used to warn rather
-- than to refuse: a file that fails this still parses (its text simply has no
-- markup in it), but saying so beats silently showing a blank document.
looksLikeRtf :: Text -> Bool
looksLikeRtf = T.isPrefixOf "{\\rtf" . T.dropWhile (`elem` (" \t\r\n\xfeff" :: String))

------------------------------------------------------------------------------
-- Parsing
--
-- A single left-to-right pass with a stack of group states. Text accumulates
-- into the current run until the formatting changes or the paragraph ends.

-- | The special destinations whose content is not document text. Everything
-- else unrecognised is ignored as a control word while its text still renders,
-- which is what the RTF specification asks a reader to do; @{\\*\\...}@ groups
-- are skipped without needing to be listed at all.
--
-- Notably absent: @\\pntext@ and @\\listtext@, the legacy and modern
-- destinations holding a list item's literal bullet or number. Those /are/
-- document text — skipping them loses every bullet in the document.
skipDestinations :: [String]
skipDestinations =
  [ "fonttbl", "filetbl", "stylesheet", "listtable", "listoverridetable"
  , "revtbl", "rsidtbl", "generator", "info", "pict", "object", "objdata"
  , "objclass", "objname", "objalias", "objsect", "themedata"
  , "colorschememapping", "datastore", "latentstyles", "xmlnstbl", "xmlopen"
  , "header", "headerl", "headerr", "headerf"
  , "footer", "footerl", "footerr", "footerf"
  , "footnote", "ftnsep", "ftnsepc", "ftncn", "aftnsep", "aftnsepc", "aftncn"
  , "annotation", "atnid", "atnauthor", "atndate", "atnref", "atnparent"
  , "bkmkstart", "bkmkend", "fldinst", "template", "mmathPr", "wgrffmtfilter"
  ]

-- | Per-group parser state. RTF scopes formatting to groups: @{@ pushes a
-- copy, @}@ pops back to what the enclosing group had.
data GState = GState
  { gsFmt    :: !RtfFmt
  , gsPar    :: !RtfPar    -- ^ Block properties in force ('rpRuns' unused here).
  , gsUc     :: !Int       -- ^ @\\ucN@: characters of fallback text to skip after each @\\uN@.
  , gsSkip   :: !Bool      -- ^ Inside a destination whose text is not rendered.
  , gsHidden :: !Bool      -- ^ Inside @\\v@ hidden text (kept apart from 'gsSkip' so @\\v0@ can never un-skip a destination).
  , gsColTbl :: !Bool      -- ^ Inside @{\\colortbl ...}@, which we do read.
  }

gsInit :: GState
gsInit = GState defaultFmt defaultPar 1 False False False

data PState = PState
  { psCur    :: !GState
  , psStack  :: ![GState]
  , psPars   :: !(Seq RtfPar)   -- ^ Completed paragraphs.
  , psRuns   :: ![RtfRun]       -- ^ Runs of the paragraph in progress, reversed.
  , psChars  :: !String         -- ^ Text of the run in progress, reversed.
  , psFmt    :: !RtfFmt         -- ^ Formatting the accumulating text was written with.
  , psColors :: !(Seq (Maybe Color))  -- ^ The colour table; 'Nothing' is an \"auto\" entry.
  , psRGB    :: !(Int, Int, Int)      -- ^ Colour under construction while in the colour table.
  , psRGBSet :: !Bool                 -- ^ Whether that entry named any component; an entry with none is \"auto\".
  , psBreak  :: !Bool                 -- ^ A page break ends the paragraph in progress.
  }

psInit :: PState
psInit = PState gsInit [] Seq.empty [] [] defaultFmt Seq.empty (0, 0, 0) False False

-- | Parse RTF source into paragraphs. Never fails: malformed markup degrades
-- to its literal text, and an unterminated group simply ends at end of input.
parseRtf :: Text -> Seq RtfPar
parseRtf txt = finish (go psInit (T.unpack (T.take maxRtfChars txt)))
  where
    -- Close the paragraph in progress, if it has anything in it.
    finish st =
      let st' = flushRun st
      in if null (psRuns st') then psPars st' else psPars st' |> mkPar st'

    go !st [] = st
    go !st (c : cs) = case c of
      '{'  -> go st { psStack = psCur st : psStack st } cs
      '}'  -> go (popGroup st) cs
      '\\' -> control st cs
      '\r' -> go st cs          -- raw line breaks in the source are not text
      '\n' -> go st cs
      _    -> go (emit c st) cs

    popGroup st = case psStack st of
      (g : gs) ->
        -- Leaving a group can change the formatting, so the accumulated text
        -- must be closed off before the old state comes back.
        let st' = flushRun st
        in st' { psCur = g, psStack = gs }
      []       -> st

    -- After a backslash: a control word (letters, optional signed parameter,
    -- optional single trailing space) or a control symbol (one character).
    control st cs = case cs of
      (c : rest)
        | isAlpha c ->
            let (word, rest1) = span isAlpha (c : rest)
                (mparam, rest2) = param rest1
                rest3 = case rest2 of (' ' : r) -> r; r -> r  -- the delimiting space belongs to the word
            in applyWord word mparam st rest3 go
        | otherwise -> applySymbol c st rest go
      [] -> st

    param s = case s of
      ('-' : d : r) | isDigit d ->
        let (ds, r') = span isDigit (d : r) in (Just (negate (readInt ds)), r')
      (d : r) | isDigit d ->
        let (ds, r') = span isDigit (d : r) in (Just (readInt ds), r')
      _ -> (Nothing, s)

-- Bounded decimal read: RTF parameters are 16-bit, and clamping keeps a
-- corrupt file from producing a huge indent or allocation.
readInt :: String -> Int
readInt = min 1000000 . foldl' (\ !a d -> a * 10 + digitToInt d) 0 . take 7

-- | A control symbol: @\\\\@, @\\{@, @\\}@, @\\'hh@, and the typographic
-- shorthands.
applySymbol
  :: Char -> PState -> String
  -> (PState -> String -> PState) -> PState
applySymbol c st rest k = case c of
  '\\' -> k (emit '\\' st) rest
  '{'  -> k (emit '{' st) rest
  '}'  -> k (emit '}' st) rest
  '\n' -> k (endPar st) rest        -- an escaped newline is a paragraph break
  '\r' -> k (endPar st) rest
  '~'  -> k (emit '\xa0' st) rest   -- non-breaking space
  '_'  -> k (emit '\x2011' st) rest -- non-breaking hyphen
  '-'  -> k st rest                 -- optional hyphen: only shown when it breaks
  ':'  -> k st rest                 -- subentry in an index range
  '*'  -> k (markSkippable st) rest -- "ignore this group if unrecognised"
  '\'' -> case rest of
    (a : b : rest') | isHexDigit a && isHexDigit b ->
      k (emit (ansiChar (16 * digitToInt a + digitToInt b)) st) rest'
    _ -> k st rest
  _ -> k st rest

-- | @{\\*\\foo ...}@: an unrecognised destination. We treat every such group
-- as skippable, which is exactly the contract — a reader that understood
-- @\\foo@ would have handled it before reaching here, and the ones we do
-- understand ('pntext', 'listtext') are re-enabled by 'applyWord'.
markSkippable :: PState -> PState
markSkippable st = st { psCur = (psCur st) { gsSkip = True } }

-- | Decode one byte of the document's code page. Windows-1252 is what
-- @\\ansi@ means in practice; its only divergence from Latin-1 is the
-- 0x80..0x9F range, which carries the curly quotes and dashes that appear in
-- real documents constantly.
ansiChar :: Int -> Char
ansiChar b
  | b >= 0x80 && b <= 0x9f = cp1252High !! (b - 0x80)
  | otherwise              = chr b

cp1252High :: String
cp1252High =
  "\x20ac\xfffd\x201a\x0192\x201e\x2026\x2020\x2021\
  \\x02c6\x2030\x0160\x2039\x0152\xfffd\x017d\xfffd\
  \\xfffd\x2018\x2019\x201c\x201d\x2022\x2013\x2014\
  \\x02dc\x2122\x0161\x203a\x0153\xfffd\x017e\x0178"

-- | Apply a control word. The continuation @k@ takes the remaining input,
-- which @\\uN@ needs so it can drop the fallback characters that follow it.
applyWord
  :: String -> Maybe Int -> PState -> String
  -> (PState -> String -> PState) -> PState
applyWord word mparam st rest k = case word of
  -- Document-level declarations we note or ignore.
  "rtf"     -> k st rest
  "uc"      -> k (modG (\g -> g { gsUc = max 0 (min 32 (fromMaybe 1 mparam)) }) st) rest

  -- Unicode: the character, then the fallback representation to discard.
  "u"       -> case mparam of
    Just n  -> let cp = if n < 0 then n + 65536 else n
                   ch = if cp >= 0 && cp <= 0x10ffff && not (cp >= 0xd800 && cp <= 0xdfff)
                          then chr cp else '\xfffd'
               in k (emit ch st) (skipAlt (gsUc (psCur st)) rest)
    Nothing -> k st rest

  -- Structure.
  "par"     -> k (endPar st) rest
  "line"    -> k (emit '\n' st) rest        -- hard break inside the paragraph
  "tab"     -> k (emit '\t' st) rest
  "cell"    -> k (emit '\t' st) rest        -- table cells lay out on tab stops
  "nestcell"-> k (emit '\t' st) rest
  "row"     -> k (endPar st) rest
  "nestrow" -> k (endPar st) rest
  "page"    -> k (endPar st { psBreak = True }) rest
  "sect"    -> k (endPar st { psBreak = True }) rest
  "pard"    -> k (modG (\g -> g { gsPar = defaultPar }) st) rest
  "plain"   -> k (setFmt (const defaultFmt) st) rest

  -- Character formatting.
  "b"       -> k (setFmt (\f -> f { rfBold   = onOff }) st) rest
  "i"       -> k (setFmt (\f -> f { rfItalic = onOff }) st) rest
  "strike"  -> k (setFmt (\f -> f { rfStrike = onOff }) st) rest
  "striked" -> k (setFmt (\f -> f { rfStrike = onOff }) st) rest
  "ulnone"  -> k (setFmt (\f -> f { rfUnder  = False  }) st) rest
  "caps"    -> k st rest
  "scaps"   -> k st rest
  -- Hidden text (field codes, index entries) is not shown, as in a word
  -- processor. Its own flag, so \v0 can only ever re-show text \v hid.
  "v"       -> k (modG (\g -> g { gsHidden = onOff }) st) rest
  "fs"      -> k (setFmt (\f -> f { rfSize = fromMaybe 0 mparam }) st) rest
  "cf"      -> k (setFmt (\f -> f { rfColor = colorAt (fromMaybe 0 mparam) st }) st) rest

  -- Paragraph properties.
  "ql"      -> k (setPar (\p -> p { rpAlign = AlignLeft }) st) rest
  "qc"      -> k (setPar (\p -> p { rpAlign = AlignCenter }) st) rest
  "qr"      -> k (setPar (\p -> p { rpAlign = AlignRight }) st) rest
  "qj"      -> k (setPar (\p -> p { rpAlign = AlignJustify }) st) rest
  "li"      -> k (setPar (\p -> p { rpLeft  = fromMaybe 0 mparam }) st) rest
  "fi"      -> k (setPar (\p -> p { rpFirst = fromMaybe 0 mparam }) st) rest

  -- The colour table, the one destination whose contents we keep.
  "colortbl" -> k ((modG (\g -> g { gsColTbl = True, gsSkip = True }) st)
                     { psRGB = (0, 0, 0), psRGBSet = False }) rest
  "red"     -> k (withRGB (\(_, g, b) -> (fromMaybe 0 mparam, g, b)) st) rest
  "green"   -> k (withRGB (\(r, _, b) -> (r, fromMaybe 0 mparam, b)) st) rest
  "blue"    -> k (withRGB (\(r, g, _) -> (r, g, fromMaybe 0 mparam)) st) rest

  -- Typographic shorthands.
  "bullet"    -> k (emit '\x2022' st) rest
  "endash"    -> k (emit '\x2013' st) rest
  "emdash"    -> k (emit '\x2014' st) rest
  "lquote"    -> k (emit '\x2018' st) rest
  "rquote"    -> k (emit '\x2019' st) rest
  "ldblquote" -> k (emit '\x201c' st) rest
  "rdblquote" -> k (emit '\x201d' st) rest
  "enspace"   -> k (emit ' ' st) rest
  "emspace"   -> k (emit ' ' st) rest
  "qmspace"   -> k (emit ' ' st) rest

  -- A list item's literal bullet or number: document text, despite being a
  -- destination, so a {\*\pntext ...} group is un-skipped here.
  "pntext"   -> k (modG (\g -> g { gsSkip = False }) st) rest
  "listtext" -> k (modG (\g -> g { gsSkip = False }) st) rest

  _ | word `elem` skipDestinations -> k (modG (\g -> g { gsSkip = True }) st) rest
    -- Every other \ul... variant (uldb, ulwave, ulth, ...) is some flavour of
    -- underline, and a terminal has exactly one.
    | take 2 word == "ul" -> k (setFmt (\f -> f { rfUnder = onOff }) st) rest
    | otherwise -> k st rest
  where
    onOff = mparam /= Just 0
    modG f s = s { psCur = f (psCur s) }
    setPar f s = modG (\g -> g { gsPar = f (gsPar g) }) s
    -- A formatting change closes the run being accumulated.
    setFmt f s = let s' = flushRun s
                 in s' { psCur = (psCur s') { gsFmt = f (gsFmt (psCur s')) } }
    withRGB f s
      | gsColTbl (psCur s) = s { psRGB = f (psRGB s), psRGBSet = True }
      | otherwise          = s

-- | The colour table entry @n@ refers to; out of range or the \"auto\" entry
-- gives 'Nothing', which the renderer paints in the theme's text colour.
colorAt :: Int -> PState -> Maybe Color
colorAt n st = case Seq.lookup n (psColors st) of
  Just c  -> c
  Nothing -> Nothing

-- | Skip the fallback characters following a @\\uN@ (@\\ucN@ of them). A
-- control word or a @\\'hh@ escape each count as one character; group
-- delimiters are never consumed, since they belong to the structure.
skipAlt :: Int -> String -> String
skipAlt 0 s = s
skipAlt n s = case s of
  ('\\' : '\'' : a : b : rest) | isHexDigit a && isHexDigit b -> skipAlt (n - 1) rest
  ('\\' : c : rest)
    | isAlpha c -> let (_, r1) = span isAlpha (c : rest)
                       r2 = dropWhile isDigit (dropWhile (== '-') r1)
                       r3 = case r2 of (' ' : r) -> r; r -> r
                   in skipAlt (n - 1) r3
    | otherwise -> skipAlt (n - 1) rest
  ('{' : _) -> s
  ('}' : _) -> s
  (c : rest)
    | c == '\r' || c == '\n' -> skipAlt n rest
    | otherwise              -> skipAlt (n - 1) rest
  [] -> []

-- | Add one character of document text, unless we are inside a destination
-- that is not text.
emit :: Char -> PState -> PState
emit c st
  | gsSkip (psCur st) =
      -- The colour table is a destination we read rather than render: its
      -- ';' terminators separate entries. An entry that named no component
      -- (the leading ';' every table starts with) is the "auto" colour.
      if gsColTbl (psCur st) && c == ';'
        then let (r, g, b) = psRGB st
                 col | not (psRGBSet st) = Nothing
                     | otherwise = Just (ColorRGB (clamp8 r) (clamp8 g) (clamp8 b))
             in st { psColors = psColors st |> col, psRGB = (0, 0, 0), psRGBSet = False }
        else st
  | gsHidden (psCur st) = st
  | otherwise =
      -- Text written under different formatting starts a new run.
      let st' = if psFmt st == gsFmt (psCur st) || null (psChars st)
                  then st
                  else flushRun st
      in st' { psChars = c : psChars st', psFmt = gsFmt (psCur st') }

clamp8 :: Int -> Word8
clamp8 = fromIntegral . max 0 . min 255

-- | Close the run being accumulated, if any.
flushRun :: PState -> PState
flushRun st
  | null (psChars st) = st
  | otherwise =
      st { psRuns = RtfRun (T.pack (reverse (psChars st))) (psFmt st) : psRuns st
         , psChars = [] }

-- | Close the paragraph in progress. An empty paragraph is kept: blank lines
-- are how a document is spaced.
endPar :: PState -> PState
endPar st0 =
  let st = flushRun st0
  in st { psPars = psPars st |> mkPar st, psRuns = [], psBreak = False }

mkPar :: PState -> RtfPar
mkPar st = (gsPar (psCur st)) { rpRuns = reverse (psRuns st), rpBreak = psBreak st }

------------------------------------------------------------------------------
-- Layout

-- | Twips (1\/1440 inch) to terminal columns, at roughly twelve characters to
-- the inch — so the half-inch indent every word processor defaults to comes
-- out as six columns, and a hanging bullet indent lines up the way it looks on
-- paper.
twipsToCols :: Int -> Int
twipsToCols t = t `div` 120

-- | Lay paragraphs out for a given text width: wrap, indent and align.
--
-- Cost is linear in the document, and it runs once per width — a resize — not
-- per frame or per scroll ('rtfRelayout' keys the cache on the width).
layoutRtf :: Int -> Int -> Seq RtfPar -> Seq RtfLine
layoutRtf tabw width pars
  | width <= 0 = Seq.empty
  | otherwise  = Seq.fromList (concatMap parLines (toList pars))
  where
    parLines p =
      let (txt, spans) = flattenRuns (rpRuns p)
          -- \line breaks split the paragraph into hard lines before wrapping;
          -- only the first of them takes the first-line indent.
          hard = splitHard txt spans
          pad0 = clampPad (twipsToCols (rpLeft p))
          padF = clampPad (twipsToCols (rpLeft p) + twipsToCols (rpFirst p))
          rule = [ RtfLine 0 T.empty [] True | rpBreak p ]
      in concat [ hardLines p pad0 (if k == (0 :: Int) then padF else pad0) t sp
                | (k, (t, sp)) <- zip [0 ..] hard ]
         ++ rule

    clampPad = max 0 . min (max 0 (width - 8))

    hardLines p pad padFirst txt spans =
      let avail = max 1 (width - pad)
          segs  = wrapLine tabw avail txt
      in [ mkLine p (if k == (0 :: Int) then padFirst else pad) avail txt spans s e
         | (k, (s, e)) <- zip [0 ..] segs ]

    mkLine p pad avail txt spans s e =
      let seg  = T.take (e - s) (T.drop s txt)
          sp   = sliceSpans s e spans
          w    = lineDisplayWidth tabw seg
          extra = case rpAlign p of
                    AlignCenter -> max 0 (avail - w) `div` 2
                    AlignRight  -> max 0 (avail - w)
                    _           -> 0
      in RtfLine (pad + extra) seg sp False

-- | Concatenate a paragraph's runs into one text plus the character ranges
-- each run occupies.
flattenRuns :: [RtfRun] -> (Text, [(Int, Int, RtfFmt)])
flattenRuns runs = (T.concat (map rrText runs), go 0 runs)
  where
    go _ [] = []
    go !i (r : rs) =
      let n = T.length (rrText r)
      in if n == 0 then go i rs else (i, i + n, rrFmt r) : go (i + n) rs

-- | Split on the @\\n@s that @\\line@ produced, rebasing the spans onto each
-- piece. Always at least one piece, so an empty paragraph still draws a line.
splitHard :: Text -> [(Int, Int, RtfFmt)] -> [(Text, [(Int, Int, RtfFmt)])]
splitHard txt spans = go 0 (T.split (== '\n') txt)
  where
    go _ []       = []
    go !off (p : ps) =
      let n = T.length p
      in (p, sliceSpans off (off + n) spans) : go (off + n + 1) ps

-- | Restrict spans to the half-open character range @[s, e)@ and rebase them
-- to start at zero.
sliceSpans :: Int -> Int -> [(Int, Int, RtfFmt)] -> [(Int, Int, RtfFmt)]
sliceSpans s e spans =
  [ (max 0 (a - s), min (e - s) (b - s), f)
  | (a, b, f) <- spans, b > s, a < e ]

------------------------------------------------------------------------------
-- The view

-- | Parse a buffer's lines into a formatted view, stamped with the editor's
-- current edit counter. Raw line breaks carry no meaning in RTF (only @\\par@
-- and @\\line@ do), so joining the lines back with newlines is exactly right.
mkRtfDoc :: Int -> Seq Text -> RtfDoc
mkRtfDoc editSeq lns = RtfDoc
  { rdPars  = parseRtf (T.intercalate "\n" (toList lns))
  , rdTop   = 0
  , rdCache = Nothing
  , rdSeq   = editSeq
  }

-- | Could the buffer have changed since this view was parsed from it?
--
-- The counter is bumped at every buffer edit, undo, redo and load anywhere in
-- the editor, so this never misses a change. It is global rather than
-- per-document, so it can over-report: editing a different file and switching
-- back re-parses once, needlessly. That is the whole cost, it is bounded, and
-- it buys an O(1) check that is right — see 'rdSeq' for what the clever
-- version did instead.
rtfStale :: Int -> RtfDoc -> Bool
rtfStale editSeq rd = editSeq /= rdSeq rd

-- | Lay the document out for this width and tab size if that is not what the
-- cache already holds, and re-clamp the scroll position to the result.
rtfRelayout :: Int -> Int -> Int -> RtfDoc -> RtfDoc
rtfRelayout tabw width height rd
  | Just (w, tw, _) <- rdCache rd, w == width, tw == tabw = rd
  | width <= 0 = rd
  | otherwise =
      rtfClamp height rd { rdCache = Just (width, tabw, layoutRtf tabw width (rdPars rd)) }

rtfLines :: RtfDoc -> Seq RtfLine
rtfLines rd = case rdCache rd of
  Just (_, _, ls) -> ls
  Nothing         -> Seq.empty

rtfLineCount :: RtfDoc -> Int
rtfLineCount = Seq.length . rtfLines

rtfParCount :: RtfDoc -> Int
rtfParCount = Seq.length . rdPars

-- | Scroll by @d@ lines, keeping at least one line on screen.
rtfScroll :: Int -> Int -> RtfDoc -> RtfDoc
rtfScroll height d rd = rtfClamp height rd { rdTop = rdTop rd + d }

rtfGoTop :: RtfDoc -> RtfDoc
rtfGoTop rd = rd { rdTop = 0 }

rtfGoBottom :: Int -> RtfDoc -> RtfDoc
rtfGoBottom height rd = rtfClamp height rd { rdTop = rtfLineCount rd }

rtfClamp :: Int -> RtfDoc -> RtfDoc
rtfClamp height rd =
  rd { rdTop = max 0 (min (max 0 (rtfLineCount rd - max 1 height)) (rdTop rd)) }

-- | The status-bar text for the formatted view: where you are, how big the
-- document is, and that this is not the editable view of the file.
rtfStatus :: RtfDoc -> String
rtfStatus rd =
  "Ln " ++ show (rdTop rd + 1) ++ " of " ++ show (rtfLineCount rd)
    ++ "   " ++ show (rtfParCount rd) ++ " paras"
    ++ "   FORMATTED "
