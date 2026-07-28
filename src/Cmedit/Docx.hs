-- | Reading a Word document: @word\/document.xml@ mapped onto the formatted
-- view's paragraph model.
--
-- __This module is a mapping, not a view.__ Everything that makes a document
-- readable in a terminal — wrapping, indenting, aligning, styled runs, the
-- scroll bar, the key handler — already exists in "Cmedit.Rtf" and
-- "Cmedit.Render", because RTF needed all of it first. A @.docx@ turns out to
-- carry the same information in different spelling: paragraphs of runs, with
-- alignment and indentation measured in the very same twips. So the whole of
-- DOCX support is @'docxPars' :: bytes -> 'Seq' 'RtfPar'@, and the reading
-- view is the RTF one with 'Cmedit.Rtf.RtfFromContainer' as its origin.
--
-- __The bargain about coverage is OOXML's own.__ The format is an ocean; this
-- reader models the ~20 elements that carry text and position and /ignores/
-- everything else rather than mis-rendering it — the rule "Cmedit.Rtf" applies
-- to @{\\*\\...}@ groups and "Cmedit.Pdf" applies to content-stream operators.
-- Concretely: no style resolution (a run's own @w:rPr@ is honoured, the style
-- sheet it inherits from is not, beyond recognising the built-in heading
-- names), no numbering definitions (a numbered list gets a bullet, because the
-- alternative is a list with no marks at all), no fields, no pictures, no
-- footnotes. Tables /are/ read, laid out on tab stops the way @\\cell@ is in
-- RTF, because dropping them would silently lose real text.
--
-- __And it never writes.__ There is no serialiser here and could not sensibly
-- be one, which is the same reason "Cmedit.Rtf" has none: a round-trip through
-- a partial model would quietly destroy the parts of someone's document this
-- reader cannot see. A @.docx@ opens read-only or not at all.
module Cmedit.Docx
  ( -- * Detection
    docxBodyMember
    -- * Mapping
  , docxPars
  , maxDocxPars
  , maxDocxChars
  ) where

import qualified Data.ByteString as BS
import Data.Char (isHexDigit, digitToInt)
import Data.List (isSuffixOf)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Rtf
  (RtfAlign(..), RtfFmt(..), RtfPar(..), RtfRun(..), defaultFmt, defaultPar)
import Cmedit.Types (Color(..))
import Cmedit.Xml (XmlEvent(..), parseXmlBytes, xAttr, xAttrInt)

------------------------------------------------------------------------------
-- Detection

-- | The archive member holding a Word document's body, if this looks like a
-- @.docx@ at all.
--
-- Peeking at member names costs nothing (they are already parsed, in the
-- central directory the listing needed anyway) and is more honest than the
-- file extension: a @.docx@ renamed to @.zip@ still reads, and a @.docx@ that
-- is really a template with no body still falls through to the listing.
--
-- The suffix fallback catches producers that write a leading slash or an
-- unusual part name; the strict path is checked first so an archive that has
-- both is not surprised by the wrong one.
docxBodyMember :: [Text] -> Maybe Text
docxBodyMember names
  | "word/document.xml" `elem` names = Just "word/document.xml"
  | otherwise = case [ n | n <- names, "/document.xml" `isSuffixOf` T.unpack n
                         , "word" `T.isInfixOf` n ] of
      (n : _) -> Just n
      []      -> Nothing

------------------------------------------------------------------------------
-- Bounds
--
-- Both exist for the same reason 'Cmedit.Rtf.maxRtfChars' does: this builds a
-- model beside the file, so its memory is linear in the document, and a file
-- built to be pathological should decline rather than stall. Hitting either
-- truncates and says so — a partial document still reads.

-- | Most paragraphs a single document will yield.
maxDocxPars :: Int
maxDocxPars = 400000

-- | Most characters of body text a single document will yield.
maxDocxChars :: Int
maxDocxChars = 8 * 1024 * 1024

------------------------------------------------------------------------------
-- Mapping

-- | Map @word\/document.xml@ to paragraphs, and say whether the bounds cut it
-- short.
docxPars :: BS.ByteString -> (Seq RtfPar, Bool)
docxPars bs = finish (run st0 (parseXmlBytes bs))
  where
    finish st
      | dsInP st  = (dsOut (endPar st), dsCut st)
      | otherwise = (dsOut st, dsCut st)

-- Per-document state. The element stack is what makes @w:jc@ inside @w:pPr@
-- (a paragraph's alignment) distinguishable from @w:jc@ inside @w:tblPr@ (a
-- table's), which matters: matching on the element name alone would let a
-- table centre every paragraph in the document.
data DS = DS
  { dsStack :: ![Text]         -- ^ Open elements, innermost first.
  , dsSkip  :: !(Maybe Int)    -- ^ Stack depth at which a skipped subtree began.
  , dsFmt   :: !RtfFmt         -- ^ Character formatting of the run in progress.
  , dsPar   :: !RtfPar         -- ^ Block properties of the paragraph in progress ('rpRuns' unused).
  , dsRuns  :: ![RtfRun]       -- ^ Completed runs of that paragraph, reversed.
  , dsChars :: ![Text]         -- ^ Text of the run in progress, reversed.
  , dsInT   :: !Bool           -- ^ Inside a @w:t@ (the only element whose character data is document text).
  , dsHide  :: !Bool           -- ^ The run in progress is @w:vanish@ (hidden) text.
  , dsInP   :: !Bool           -- ^ A paragraph is open.
  , dsHead  :: !Int            -- ^ Half-point size the paragraph's style implies (0 = body).
  , dsBullet:: !Bool           -- ^ The paragraph is a list item.
  , dsLevel :: !Int            -- ^ Its list level.
  , dsTbl   :: !Int            -- ^ Table nesting depth.
  , dsCell  :: !Int            -- ^ Cells closed so far in the current row.
  , dsOut   :: !(Seq RtfPar)
  , dsChar  :: !Int            -- ^ Characters emitted so far (against 'maxDocxChars').
  , dsCut   :: !Bool           -- ^ A bound was hit.
  }

st0 :: DS
st0 = DS [] Nothing defaultFmt defaultPar [] [] False False False 0 False 0 0 0 Seq.empty 0 False

run :: DS -> [XmlEvent] -> DS
run !st [] = st
run !st (e : es)
  | dsCut st, Seq.length (dsOut st) >= maxDocxPars = st
  | otherwise = run (step st e) es

step :: DS -> XmlEvent -> DS
step st ev = case ev of
  XText t
    | skipping st || not (dsInT st) -> st
    | otherwise                     -> emit t st

  XStart nm as ->
    let st' = st { dsStack = nm : dsStack st }
    in if skipping st' then st' else start nm as st'

  XEnd nm ->
    let d   = length (dsStack st)
        st' = end nm st
    in case dsSkip st of
         -- The subtree we were skipping has closed.
         Just k | d <= k -> (popStack st) { dsSkip = Nothing }
         Just _          -> popStack st
         Nothing         -> popStack st'
  where
    popStack s = s { dsStack = drop 1 (dsStack s) }

skipping :: DS -> Bool
skipping = (/= Nothing) . dsSkip

-- The element enclosing the one just opened (the stack already has the new
-- element on top).
parentOf :: DS -> Text
parentOf st = case dsStack st of
  (_ : p : _) -> p
  _           -> T.empty

start :: Text -> [(Text, Text)] -> DS -> DS
start nm as st = case nm of
  -- Content we deliberately do not read. Skipping the whole subtree is what
  -- keeps an image's alt text, a field's instruction code and a deleted
  -- revision out of the rendered document.
  _ | nm `elem` skipSubtrees -> st { dsSkip = Just (length (dsStack st)) }

  -- Inside a table, a paragraph is one line of one cell and must *not* start
  -- a new one: the row is the paragraph (see 'end'), and starting one here
  -- would flush every cell separately and lose the table's shape.
  "p"  | dsTbl st > 0        -> st
       | not (inTblProps st) -> startPar st
  "r"  -> (flushRun st) { dsFmt = defaultFmt, dsHide = False }
  "t"  -> st { dsInT = True }

  -- Character formatting: only from a run's own w:rPr.
  _ | parentOf st == "rPr" -> case nm of
        "b"       -> setFmt (\f -> f { rfBold   = onOff }) st
        "bCs"     -> setFmt (\f -> f { rfBold   = onOff }) st
        "i"       -> setFmt (\f -> f { rfItalic = onOff }) st
        "iCs"     -> setFmt (\f -> f { rfItalic = onOff }) st
        "u"       -> setFmt (\f -> f { rfUnder  = uOn }) st
        "strike"  -> setFmt (\f -> f { rfStrike = onOff }) st
        "dstrike" -> setFmt (\f -> f { rfStrike = onOff }) st
        "sz"      -> setFmt (\f -> f { rfSize   = maybe 0 (min 400) (xAttrInt "val" as) }) st
        "color"   -> setFmt (\f -> f { rfColor  = hexColor (xAttr "val" as) }) st
        -- Hidden text (a field's result, an index entry) is not shown, as in
        -- a word processor. A flag rather than a skipped subtree, because
        -- what it hides is the rest of the *run* — which encloses the w:rPr
        -- this appears in, not the other way round.
        "vanish"  -> st { dsHide = onOff }
        _         -> st

  -- Paragraph properties: only from the paragraph's own w:pPr.
  _ | parentOf st == "pPr" -> case nm of
        "jc"    -> setPar (\p -> p { rpAlign = alignOf (xAttr "val" as) }) st
        "ind"   -> setPar (indent as) st
        "numPr" -> st { dsBullet = True }
        "pStyle"-> st { dsHead = headingSize (xAttr "val" as) }
        "pageBreakBefore" -> breakHere st
        _       -> st

  "ilvl" | parentOf st == "numPr" ->
      st { dsLevel = max 0 (min 8 (maybe 0 id (xAttrInt "val" as))) }

  -- Inline breaks and whitespace.
  "br"  | xAttr "type" as == Just "page" -> breakHere st
        | otherwise                      -> emit "\n" st
  "cr"  -> emit "\n" st
  "tab" -> emit "\t" st
  "noBreakHyphen" -> emit "\x2011" st
  "sym" -> emit (symChar (xAttr "char" as)) st

  -- Tables lay out on tab stops, exactly as RTF's \cell does.
  "tbl" -> st { dsTbl = dsTbl st + 1 }
  "tr"  -> st { dsCell = 0 }
  "tc"  -> if dsCell st > 0 then emit "\t" st else st
  _     -> st
  where
    -- OOXML's on/off attribute: absent means on, and only an explicit false
    -- turns it off. Getting this backwards makes every <w:b/> a no-op.
    onOff = case xAttr "val" as of
              Just v  -> v `notElem` ["0", "false", "off"]
              Nothing -> True
    uOn = case xAttr "val" as of
            Just v  -> v /= "none"
            Nothing -> True

-- Subtrees whose character data is not document text. @mc:Fallback@ is in the
-- list for a different reason from the rest: it is a *duplicate* of the
-- @mc:Choice@ beside it, so reading both prints everything in a text box twice.
skipSubtrees :: [Text]
skipSubtrees =
  [ "instrText", "delText", "del", "drawing", "pict", "object", "txbxContent"
  , "Fallback", "footnoteReference", "endnoteReference", "commentReference"
  , "proofErr", "bookmarkStart", "bookmarkEnd", "sectPr", "rPrChange"
  , "pPrChange", "moveFrom", "framePr", "background", "sdtPr", "sdtEndPr"
  ]

-- Are we inside a table's or a cell's property block? Those contain a @w:p@
-- in no producer's output, but they do contain @w:jc@ and @w:ind@, which is
-- what 'parentOf' guards against; this guards the one remaining case.
inTblProps :: DS -> Bool
inTblProps st = any (`elem` ["tblPr", "tcPr", "trPr"]) (take 3 (drop 1 (dsStack st)))

end :: Text -> DS -> DS
end nm st = case nm of
  "t"  -> st { dsInT = False }
  "r"  -> flushRun st
  "p"  | dsInP st, dsTbl st == 0 -> endPar st
       -- Inside a table a paragraph is not a paragraph: the *row* is, and its
       -- cells are its tab stops (the tab is emitted when the next cell
       -- opens). Ending one here would put every cell on its own line and
       -- lose the table's shape entirely.
       | dsInP st                -> flushRun st
  "tc" -> st { dsCell = dsCell st + 1 }
  "tr" -> if dsInP st then endPar st else st
  "tbl"-> st { dsTbl = max 0 (dsTbl st - 1) }
  _    -> st

------------------------------------------------------------------------------
-- Building paragraphs

startPar :: DS -> DS
startPar st0' =
  let st = if dsInP st0' then endPar st0' else st0'
  in st { dsInP = True, dsPar = defaultPar, dsFmt = defaultFmt
        , dsRuns = [], dsChars = [], dsHead = 0, dsBullet = False, dsLevel = 0 }

-- Close the paragraph in progress.
--
-- An empty paragraph produces nothing. Word documents space themselves partly
-- with empty paragraphs and partly with a style's @spacing after@, which this
-- reader does not resolve — so honouring the first and missing the second
-- gives a document with gaps in some places and none in others. Dropping the
-- empties and setting 'rpSpace' on every real paragraph instead gives exactly
-- one blank line between paragraphs, wherever the document put its spacing.
endPar :: DS -> DS
endPar st0' =
  let st    = flushRun st0'
      runs0 = reverse (dsRuns st)
      -- A style's font size applies to any run that did not set its own,
      -- which is how a heading gets to be a heading: Word puts the size in
      -- the style, not on the runs.
      runs1 = [ if rfSize (rrFmt r) == 0 && dsHead st > 0
                  then r { rrFmt = (rrFmt r) { rfSize = dsHead st } }
                  else r
              | r <- runs0 ]
      -- A list item's marker comes from numbering.xml, which this reader does
      -- not resolve. A bullet for every level is a small lie about ordered
      -- lists and a large improvement on a list with no marks at all.
      runs2 | dsBullet st = RtfRun "\x2022 " defaultFmt : runs1
            | otherwise   = runs1
      tight = dsBullet st || dsTbl st > 0
      par = (dsPar st)
              { rpRuns = runs2
              , rpSpace = not tight
              , rpLeft = rpLeft (dsPar st)
                           + (if dsBullet st && rpLeft (dsPar st) == 0
                                then 360 * (dsLevel st + 1) else 0)
              , rpFirst = if dsBullet st && rpFirst (dsPar st) == 0
                            then -360 else rpFirst (dsPar st)
              }
      keep = any (not . T.null . rrText) runs1 || rpBreak par
  in st { dsOut = if keep then dsOut st |> par else dsOut st
        , dsRuns = [], dsChars = [], dsInP = False
        , dsPar = defaultPar, dsBullet = False, dsHead = 0
        , dsCut = dsCut st || Seq.length (dsOut st) + 1 >= maxDocxPars }

-- A page break belongs *between* two paragraphs, and 'rpBreak' draws its rule
-- after the paragraph it is set on — so the break closes the paragraph it
-- appears in and marks the one before the rule, rather than the one after it.
breakHere :: DS -> DS
breakHere st0' =
  let st = if dsInP st0' then endPar st0' else st0'
  in case Seq.viewr (dsOut st) of
       rest Seq.:> p -> st { dsOut = rest |> p { rpBreak = True } }
       Seq.EmptyR    -> st

setPar :: (RtfPar -> RtfPar) -> DS -> DS
setPar f st = st { dsPar = f (dsPar st) }

-- A formatting change closes the run being accumulated, so the text before it
-- keeps the formatting it was written with.
setFmt :: (RtfFmt -> RtfFmt) -> DS -> DS
setFmt f st = let st' = flushRun st in st' { dsFmt = f (dsFmt st') }

emit :: Text -> DS -> DS
emit t st
  | T.null t || dsHide st     = st
  | dsChar st >= maxDocxChars = st { dsCut = True }
  | not (dsInP st)            = emit t (startPar st)
  | otherwise = st { dsChars = t : dsChars st, dsChar = dsChar st + T.length t }

flushRun :: DS -> DS
flushRun st = case dsChars st of
  [] -> st
  -- Detached: a single piece is a slice of the whole decoded member, and one
  -- retained slice pins all of it for as long as the document is open (the
  -- rule 'Cmedit.EditorState.detach' exists for).
  ps -> st { dsRuns = RtfRun (joinPieces ps) (dsFmt st) : dsRuns st, dsChars = [] }
  where
    joinPieces [x] = T.copy x
    joinPieces xs  = T.concat (reverse xs)

------------------------------------------------------------------------------
-- Small mappings

alignOf :: Maybe Text -> RtfAlign
alignOf v = case v of
  Just "center" -> AlignCenter
  Just "right"  -> AlignRight
  Just "end"    -> AlignRight
  Just "both"   -> AlignJustify
  Just "distribute" -> AlignJustify
  _             -> AlignLeft

-- Indents are already in twips, which is why 'Cmedit.Rtf.twipsToCols'
-- transfers to this format without a conversion: OOXML inherited the unit.
-- A hanging indent is a negative first line, exactly as RTF's \fi expresses it.
indent :: [(Text, Text)] -> RtfPar -> RtfPar
indent as p = p
  { rpLeft  = clamp (firstOf ["left", "start"])
  , rpFirst = case xAttrInt "hanging" as of
                Just h  -> negate (clamp (Just h))
                Nothing -> clamp (firstOf ["firstLine"])
  }
  where
    firstOf ks = case [ v | k <- ks, Just v <- [xAttrInt k as] ] of
                   (v : _) -> Just v
                   []      -> Nothing
    clamp = max 0 . min 20000 . maybe 0 id

-- Built-in style names that mean "heading", mapped to a half-point size. The
-- renderer draws anything at 14pt or over in bold ('Cmedit.Render.rtfStyle'),
-- since a terminal has one font size — so this is how a heading reads as one.
headingSize :: Maybe Text -> Int
headingSize v = case fmap (T.toLower . T.filter (/= ' ')) v of
  Just "title"    -> 36
  Just "subtitle" -> 30
  Just s | Just n <- T.stripPrefix "heading" s ->
    case n of
      "1" -> 32
      "2" -> 30
      _   -> 28
  _ -> 0

-- @w:color w:val="RRGGBB"@, or "auto" for the theme's own text colour.
hexColor :: Maybe Text -> Maybe Color
hexColor (Just v)
  | T.length v == 6, T.all isHexDigit v = Just (ColorRGB (byteAt 0) (byteAt 2) (byteAt 4))
  where byteAt i = fromIntegral (16 * digitToInt (T.index v i) + digitToInt (T.index v (i + 1)))
hexColor _ = Nothing

-- @w:sym w:char="F0B7"@ names a code point in a symbol font's private-use
-- area. The fonts are not available and the mapping is per-font, so the one
-- honest rendering of every symbol is a bullet — which is what the overwhelming
-- majority of them are (Wingdings' F0B7 is the list bullet).
symChar :: Maybe Text -> Text
symChar _ = "\x2022"
