-- | Reading OpenDocument: @.odt@ text documents and @.ods@ spreadsheets.
--
-- The third container format, and by far the cheapest, because everything it
-- needs was built for the first two: "Cmedit.Zip" opens it, "Cmedit.Xml"
-- reads it, and it maps onto the two targets "Cmedit.Docx" and "Cmedit.Xlsx"
-- already map onto — @'Cmedit.Rtf.RtfPar'@ for a document, a grid of 'Text'
-- for a spreadsheet. Both live here rather than in two modules because they
-- share the whole container: one @content.xml@, one style table, one set of
-- length and colour conversions.
--
-- __The one structural difference from OOXML, and it is the whole of the
-- work: formatting lives in named styles, not on the run.__ Where a @.docx@
-- writes @\<w:b\/\>@ inside the run it applies to, OpenDocument writes
-- @\<text:span text:style-name=\"T1\"\>@ and defines @T1@ in an
-- @\<office:automatic-styles\>@ block near the top of the file. So a reader
-- that matches on elements alone sees no formatting at all. 'odfStyles' builds
-- that table in the same pass, which works because the automatic styles are
-- required to precede @\<office:body\>@; 'resolveFmt' then walks each style's
-- @style:parent-style-name@ chain. Named styles from @styles.xml@ are /not/
-- resolved — the same bargain "Cmedit.Docx" strikes with a style sheet — but
-- headings do not need them, because @\<text:h\>@ carries its outline level as
-- an attribute.
--
-- __And one place OpenDocument is better than OOXML.__ An @.ods@ cell carries
-- its /displayed/ text in a @\<text:p\>@ child as well as its raw value, so a
-- date reads as @2024-01-15@ and a currency as @$1,234.56@ — the number
-- formats "Cmedit.Xlsx" has to decline to apply. That display text is
-- preferred wherever it exists, which also means a formula is normally already
-- answered; 'odfFormula' translates the few that are not into the syntax
-- "Cmedit.Formula" reads.
--
-- __Two bounds are not optional here.__ An @.ods@ row is padded out to the
-- sheet's full width with a single cell carrying
-- @table:number-columns-repeated=\"1024\"@, and trailing empty rows with
-- @table:number-rows-repeated=\"1048576\"@. Expanding those literally would
-- turn every one-cell spreadsheet into a million-row grid, so a repeated run
-- of /empty/ cells or rows at the end of its line is dropped rather than
-- materialised, and what remains is capped.
module Cmedit.Odf
  ( -- * Detection
    odfContentPath
  , isOdf
  , OdfKind(..)
  , odfKind
    -- * Text documents
  , odfPars
  , maxOdfPars
  , maxOdfChars
    -- * Spreadsheets
  , odfSheets
  , odfFormula
    -- * Shared pieces (exposed for testing)
  , odfStyles
  , OdfStyle(..)
  , lengthToTwips
  ) where

import qualified Data.ByteString as BS
import Data.Char (isDigit, isHexDigit, isSpace, digitToInt)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
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

-- | The member every OpenDocument file has, and the only one either view
-- reads.
odfContentPath :: Text
odfContentPath = "content.xml"

-- | Does this archive's table of contents look like an OpenDocument file?
--
-- The @mimetype@ member is ODF's own signature and would name the kind
-- outright, but repackaging tools drop and recompress it routinely, so the
-- manifest — which nothing can read the file without — is the test that
-- actually holds. Which /kind/ it is comes from 'odfKind' instead, out of the
-- content it has to read anyway.
isOdf :: [Text] -> Bool
isOdf names = odfContentPath `elem` names && "META-INF/manifest.xml" `elem` names

-- | The kinds of OpenDocument file that have a reading view.
data OdfKind = OdfText | OdfSheet deriving (Eq, Show)

-- | Which kind this is, from the element inside @\<office:body\>@.
--
-- A presentation or a drawing yields 'Nothing' and falls back to the archive
-- listing: their content is positioned shapes, which is a different problem
-- from a document and not one this module pretends to solve.
odfKind :: BS.ByteString -> Maybe OdfKind
odfKind bs = go (parseXmlBytes bs)
  where
    go (XStart "body" _ : rest) = case dropWhile notStart rest of
      (XStart "text" _ : _)        -> Just OdfText
      (XStart "spreadsheet" _ : _) -> Just OdfSheet
      _                            -> Nothing
    go (_ : rest) = go rest
    go []         = Nothing
    notStart e = case e of XStart _ _ -> False; _ -> True

------------------------------------------------------------------------------
-- Bounds

-- | Most paragraphs one text document will yield.
maxOdfPars :: Int
maxOdfPars = 400000

-- | Most characters of body text one text document will yield.
maxOdfChars :: Int
maxOdfChars = 8 * 1024 * 1024

-- | Most cells one sheet will yield, and most columns a row will be expanded
-- to. Both exist because of the repeat counts described in the module header:
-- without them a file that says @table:number-rows-repeated=\"1048576\"@ asks
-- for a million rows and gets them.
maxOdfCells, maxOdfCols, maxOdfRows :: Int
maxOdfCells = 2000000
maxOdfCols  = 16384
maxOdfRows  = 1048576

------------------------------------------------------------------------------
-- The style table

-- | One automatic style, as the attributes it sets. Every field is a \"was it
-- set\" option rather than a value, because a style that says nothing about
-- boldness must inherit it rather than turn it off.
data OdfStyle = OdfStyle
  { osParent :: !(Maybe Text)
  , osBold   :: !(Maybe Bool)
  , osItalic :: !(Maybe Bool)
  , osUnder  :: !(Maybe Bool)
  , osStrike :: !(Maybe Bool)
  , osColor  :: !(Maybe (Maybe Color))   -- ^ Outer: was set. Inner: 'Nothing' is \"automatic\".
  , osSize   :: !(Maybe Int)             -- ^ Half-points, matching 'rfSize'.
  , osAlign  :: !(Maybe RtfAlign)
  , osLeft   :: !(Maybe Int)             -- ^ Twips, matching 'rpLeft'.
  , osFirst  :: !(Maybe Int)
  } deriving (Eq, Show)

emptyStyle :: OdfStyle
emptyStyle = OdfStyle Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                      Nothing Nothing Nothing

-- | Build the automatic-style table from a @content.xml@ event stream.
--
-- One pass, stopping at @\<office:body\>@ — the styles are required to come
-- first, and reading past the body would walk the whole document for nothing.
odfStyles :: [XmlEvent] -> Map Text OdfStyle
odfStyles = go M.empty T.empty
  where
    go acc _   (XStart "body" _ : _) = acc
    go acc _   (XStart "style" as : rest)
      | Just nm <- xAttr "name" as =
          go (M.insert nm (emptyStyle { osParent = xAttr "parent-style-name" as }) acc) nm rest
    go acc cur (XStart "text-properties" as : rest) =
        go (M.adjust (textProps as) cur acc) cur rest
    go acc cur (XStart "paragraph-properties" as : rest) =
        go (M.adjust (parProps as) cur acc) cur rest
    go acc cur (_ : rest) = go acc cur rest
    go acc _   []         = acc

    textProps as st = st
      { osBold   = fmap (`notElem` ["normal", "inherit"]) (xAttr "font-weight" as)
                     `orKeep` osBold st
      , osItalic = fmap (`notElem` ["normal", "inherit"]) (xAttr "font-style" as)
                     `orKeep` osItalic st
      -- ODF spells these as a *line style*, so anything but "none" is on.
      , osUnder  = fmap (/= "none") (xAttr "text-underline-style" as) `orKeep` osUnder st
      , osStrike = fmap (/= "none") (xAttr "text-line-through-style" as) `orKeep` osStrike st
      , osColor  = fmap hexColor (xAttr "color" as) `orKeep` osColor st
      , osSize   = (xAttr "font-size" as >>= fontSize) `orKeep` osSize st
      }
    parProps as st = st
      { osAlign = fmap alignOf (xAttr "text-align" as) `orKeep` osAlign st
      , osLeft  = (xAttr "margin-left" as >>= lengthToTwips) `orKeep` osLeft st
      , osFirst = (xAttr "text-indent" as >>= lengthToTwips) `orKeep` osFirst st
      }
    orKeep new old = case new of Just _ -> new; Nothing -> old

-- | Apply a style's character formatting, parents first.
resolveFmt :: Map Text OdfStyle -> Text -> RtfFmt -> RtfFmt
resolveFmt tbl = walk (0 :: Int)
  where
    walk d nm f
      | d > 16 = f          -- a parent chain that loops is a corrupt file
      | otherwise = case M.lookup nm tbl of
          Nothing -> f
          Just st -> apply st (maybe f (\p -> walk (d + 1) p f) (osParent st))
    apply st f = f
      { rfBold   = fromMaybe (rfBold f) (osBold st)
      , rfItalic = fromMaybe (rfItalic f) (osItalic st)
      , rfUnder  = fromMaybe (rfUnder f) (osUnder st)
      , rfStrike = fromMaybe (rfStrike f) (osStrike st)
      , rfColor  = fromMaybe (rfColor f) (osColor st)
      , rfSize   = fromMaybe (rfSize f) (osSize st)
      }

-- | Apply a style's block properties, parents first.
resolvePar :: Map Text OdfStyle -> Text -> RtfPar -> RtfPar
resolvePar tbl = walk (0 :: Int)
  where
    walk d nm p
      | d > 16 = p
      | otherwise = case M.lookup nm tbl of
          Nothing -> p
          Just st -> apply st (maybe p (\q -> walk (d + 1) q p) (osParent st))
    apply st p = p
      { rpAlign = fromMaybe (rpAlign p) (osAlign st)
      , rpLeft  = fromMaybe (rpLeft p) (osLeft st)
      , rpFirst = fromMaybe (rpFirst p) (osFirst st)
      }

------------------------------------------------------------------------------
-- Units

-- | An ODF length to twips. OpenDocument writes CSS lengths — @0.5in@,
-- @1.27cm@, @12pt@ — where OOXML writes a bare twip count, so this is the
-- conversion "Cmedit.Docx" did not need.
lengthToTwips :: Text -> Maybe Int
lengthToTwips t0
  | Just v <- num = Just (clamp (round (v * unit)))
  | otherwise     = Nothing
  where
    t    = T.strip t0
    digs = T.takeWhile (\c -> isDigit c || c `elem` ("-+." :: String)) t
    suf  = T.toLower (T.strip (T.drop (T.length digs) t))
    num  = readDouble digs
    unit = case suf of
      "in" -> 1440
      "\"" -> 1440
      "cm" -> 566.929
      "mm" -> 56.6929
      "pt" -> 20
      "pc" -> 240
      "px" -> 15          -- at the 96 dpi every producer assumes
      _    -> 20          -- a bare number: points is the least surprising guess
    clamp = max (-20000) . min 20000

-- | @fo:font-size@ to half-points. Percentages are relative to a style we did
-- not resolve, so they are declined rather than guessed at.
fontSize :: Text -> Maybe Int
fontSize t
  | "%" `T.isSuffixOf` T.strip t = Nothing
  | otherwise = case lengthToTwips t of
      Just tw -> Just (max 0 (min 400 (tw `div` 10)))   -- twips → half-points
      Nothing -> Nothing

readDouble :: Text -> Maybe Double
readDouble t
  | T.null t = Nothing
  | otherwise = case reads (fixup (T.unpack t)) :: [(Double, String)] of
      [(d, rest)] | all isSpace rest -> Just d
      _ -> Nothing
  where
    fixup s = case s of
      ('+' : r)       -> fixup r
      ('.' : r)       -> '0' : '.' : r
      ('-' : '.' : r) -> '-' : '0' : '.' : r
      _ -> case break (== '.') s of
             (a, ".") -> a ++ ".0"
             _        -> s

alignOf :: Text -> RtfAlign
alignOf v = case T.toLower v of
  "center"  -> AlignCenter
  "end"     -> AlignRight
  "right"   -> AlignRight
  "justify" -> AlignJustify
  _         -> AlignLeft

hexColor :: Text -> Maybe Color
hexColor v0
  | T.length v == 6, T.all isHexDigit v = Just (ColorRGB (byteAt 0) (byteAt 2) (byteAt 4))
  | otherwise = Nothing
  where
    v = T.dropWhile (== '#') (T.strip v0)
    byteAt i = fromIntegral (16 * digitToInt (T.index v i) + digitToInt (T.index v (i + 1)))

-- ODF collapses runs of whitespace the way HTML does; a run of real spaces is
-- written as @\<text:s text:c=\"n\"\/\>@ instead.
collapse :: Text -> Text
collapse t
  | T.all (== ' ') (T.filter isSpace t), not ("  " `T.isInfixOf` t) = t
  | otherwise = T.pack (go (T.unpack t))
  where
    go (c : cs) | isSpace c = ' ' : go (dropWhile isSpace cs)
                | otherwise = c : go cs
    go []                   = []

------------------------------------------------------------------------------
-- Text documents

-- | Map an @.odt@'s @content.xml@ to paragraphs, and say whether a bound cut
-- it short.
odfPars :: BS.ByteString -> (Seq RtfPar, Bool)
odfPars bs = tbl `seq` finish (foldl' (tstep tbl) ts0 evs)
  where
    evs = parseXmlBytes bs
    -- Forced before the body fold starts. Both walk the same lazy event list,
    -- and an unforced style table holds its head — so the prefix the fold has
    -- already passed would be retained until the first styled span asked for
    -- a lookup.
    tbl = odfStyles evs
    finish st = let st' = endPar st in (tsOut st', tsCut st')

-- Subtrees that are not body text. Footnotes and annotations are a word
-- processor's marginalia; the tracked-changes block is a record of text that
-- is no longer there.
odfSkip :: [Text]
odfSkip =
  [ "automatic-styles", "font-face-decls", "scripts", "forms"
  , "note", "annotation", "tracked-changes", "sequence-decls"
  , "user-field-decls", "variable-decls", "binary-data", "object", "image"
  ]

data TS = TS
  { tsStack :: ![Text]
  , tsSkip  :: !(Maybe Int)
  , tsFmts  :: ![RtfFmt]      -- ^ Formatting stack; spans nest.
  , tsPar   :: !RtfPar
  , tsRuns  :: ![RtfRun]
  , tsChars :: ![Text]
  , tsRunF  :: !RtfFmt
  , tsOut   :: !(Seq RtfPar)
  , tsOpen  :: !Bool          -- ^ A paragraph is being built.
  , tsTbl   :: !Int           -- ^ Table nesting: a row is the paragraph, cells are tab stops.
  , tsCell  :: !Int
  , tsList  :: !Int           -- ^ @text:list@ nesting.
  , tsMark  :: !Text          -- ^ A list bullet held back until the item's first text.
  , tsItem  :: !(Maybe RtfPar)
    -- ^ The block properties a list item imposes on its paragraphs. Held apart
    -- from 'tsPar' because in ODF the item's content /is/ a @text:p@, and that
    -- paragraph starting would otherwise reset the indent and the bullet the
    -- item had just set.
  , tsWs    :: !Bool
  , tsAny   :: !Bool
  , tsChar  :: !Int
  , tsCut   :: !Bool
  }

ts0 :: TS
ts0 = TS [] Nothing [defaultFmt] defaultPar [] [] defaultFmt Seq.empty False 0 0 0
         T.empty Nothing True False 0 False

curF :: TS -> RtfFmt
curF st = case tsFmts st of (f : _) -> f; [] -> defaultFmt

tstep :: Map Text OdfStyle -> TS -> XmlEvent -> TS
tstep tbl st ev = case ev of
  XText t
    | tsSkip st /= Nothing -> st
    | otherwise            -> emit (collapse t) st

  XStart nm as ->
    let st' = st { tsStack = nm : tsStack st }
    in if tsSkip st' /= Nothing then st' else tstart tbl nm as st'

  XEnd nm ->
    let d = length (tsStack st)
    in case tsSkip st of
         Just k | d <= k -> pop st { tsSkip = Nothing }
         Just _          -> pop st
         Nothing         -> pop (tend nm st)
  where pop s = s { tsStack = drop 1 (tsStack s) }

tstart :: Map Text OdfStyle -> Text -> [(Text, Text)] -> TS -> TS
tstart tbl nm as st
  | nm `elem` odfSkip = st { tsSkip = Just (length (tsStack st)) }
  | otherwise = case nm of
      -- A paragraph, unless we are inside a table, where the *row* is the
      -- paragraph and its cells are tab stops (as in "Cmedit.Docx").
      "p" | tsTbl st > 0 -> styled st
          | otherwise    -> styled (startPar st)
      "h" | tsTbl st > 0 -> heading (styled st)
          | otherwise    -> heading (styled (startPar st))

      -- Character formatting, which in ODF is only ever a style name.
      "span" -> st { tsFmts = applyName (curF st) : tsFmts st }
      "a"    -> st { tsFmts = curF st : tsFmts st }   -- unwrapped, but its end pops

      "line-break" -> emitRaw "\n" st
      "tab"        -> emitRaw "\t" st
      "s"          -> emitRaw (T.replicate (max 1 (min 200 (fromMaybe 1 (xAttrInt "c" as)))) " ") st
      "soft-page-break" -> st

      "list"      -> (endPar st) { tsList = tsList st + 1 }
      -- The marker is a bullet whatever the list's numbering style says: the
      -- style that would say otherwise lives in styles.xml, and a list with no
      -- marks at all is worse than a bulleted numbered one.
      "list-item" -> (endPar st)
                       { tsMark = "\x2022 "
                       , tsItem = Just defaultPar { rpLeft = 360 * max 1 (tsList st)
                                                  , rpFirst = -360 } }

      "table"          -> st { tsTbl = tsTbl st + 1 }
      "table-row"      -> (endPar st) { tsCell = 0 }
      "table-cell"     -> if tsCell st > 0 then emitRaw "\t" st else st
      "covered-table-cell" -> st
      _ -> st
  where
    styled s = case xAttr "style-name" as of
      Just sn -> s { tsPar  = resolvePar tbl sn (tsPar s)
                     -- A paragraph style carries character properties too, and
                     -- they are the baseline any span inside it layers onto.
                   , tsFmts = [resolveFmt tbl sn defaultFmt] }
      Nothing -> s { tsFmts = [defaultFmt] }
    applyName f = case xAttr "style-name" as of
      Just sn -> resolveFmt tbl sn f
      Nothing -> f
    -- A heading names its level outright, which is why the named styles in
    -- styles.xml never have to be resolved to recognise one.
    heading s = case xAttrInt "outline-level" as of
      Just n  -> s { tsFmts = map (\f -> f { rfSize = headingSize n }) (tsFmts s) }
      Nothing -> s { tsFmts = map (\f -> f { rfSize = headingSize 1 }) (tsFmts s) }

headingSize :: Int -> Int
headingSize n = case n of
  1 -> 36
  2 -> 32
  3 -> 30
  _ -> 28

tend :: Text -> TS -> TS
tend nm st = case nm of
  "span" -> popF st
  "a"    -> popF st
  "p"    | tsTbl st > 0 -> flushRun st
         | otherwise    -> endPar st
  "h"    | tsTbl st > 0 -> flushRun st
         | otherwise    -> endPar st
  "list"       -> (endPar st) { tsList = max 0 (tsList st - 1) }
  "list-item"  -> (endPar st) { tsItem = Nothing, tsMark = T.empty }
  "table-cell" -> st { tsCell = tsCell st + 1 }
  "table-row"  -> endPar st
  "table"      -> st { tsTbl = max 0 (tsTbl st - 1) }
  _            -> st
  where popF s = s { tsFmts = case tsFmts s of
                                (_ : rest@(_ : _)) -> rest
                                other              -> other }

startPar :: TS -> TS
startPar st =
  let st' = endPar st
  in st' { tsOpen = True
         , tsPar = fromMaybe (tsPar st') (tsItem st)
         , tsMark = tsMark st }

emit :: Text -> TS -> TS
emit t st
  | T.null t  = st
  | tsWs st   = emitRaw (T.dropWhile (== ' ') t) st
  | otherwise = emitRaw t st

emitRaw :: Text -> TS -> TS
emitRaw t st
  | T.null t             = st
  | tsChar st >= maxOdfChars = st { tsCut = True }
  | otherwise =
      let st1 | T.null (tsMark st) = st
              | otherwise = (pushRun (tsMark st) defaultFmt st) { tsMark = T.empty }
          st2 = pushRun t (curF st1) st1
      in st2 { tsWs = " " `T.isSuffixOf` t || "\n" `T.isSuffixOf` t
             , tsAny = True, tsOpen = True
             , tsChar = tsChar st + T.length t }

pushRun :: Text -> RtfFmt -> TS -> TS
pushRun t f st
  | not (null (tsChars st)), f == tsRunF st = st { tsChars = t : tsChars st }
  | otherwise = let st' = flushRun st in st' { tsChars = [t], tsRunF = f }

flushRun :: TS -> TS
flushRun st = case tsChars st of
  [] -> st
  ps -> st { tsRuns = RtfRun (joinPieces ps) (tsRunF st) : tsRuns st, tsChars = [] }
  where
    -- Detached: one retained slice pins the whole decoded content.xml.
    joinPieces [x] = T.copy x
    joinPieces xs  = T.concat (reverse xs)

-- Close the paragraph in progress. Empty ones are dropped and every real one
-- is spaced, for the reason "Cmedit.Docx" gives: an OpenDocument file carries
-- its paragraph spacing in styles this reader does not fully resolve, so
-- honouring only its manual blank paragraphs gives an uneven document.
endPar :: TS -> TS
endPar st0' =
  let st = flushRun st0'
      tight = tsTbl st > 0 || tsList st > 0
      par = (tsPar st) { rpRuns = reverse (tsRuns st), rpSpace = not tight }
  in if not (tsAny st)
       then st { tsRuns = [], tsChars = [], tsPar = defaultPar, tsOpen = False
               , tsWs = True, tsAny = False }
       else st { tsOut = tsOut st |> par
               , tsRuns = [], tsChars = [], tsPar = defaultPar, tsOpen = False
               , tsWs = True, tsAny = False
               , tsCut = tsCut st || Seq.length (tsOut st) + 1 >= maxOdfPars }

------------------------------------------------------------------------------
-- Spreadsheets

-- | Map an @.ods@'s @content.xml@ to one grid per sheet, with the formulas
-- that came with no displayed value, and whether a bound cut it short.
--
-- The formula map is nearly always empty: OpenDocument stores each cell's
-- /displayed/ text, so a calculated cell already reads correctly (and with its
-- number format applied, which is more than an @.xlsx@ gives). It is there for
-- the same case "Cmedit.Xlsx" has — a file written by a library that did not
-- calculate.
--
-- Preferring the display text has one consequence worth knowing: it is also
-- what a formula evaluated here /reads/, so a reference to a
-- currency-formatted cell sees @$1,234.50@ and arithmetic on it comes out
-- @#VALUE!@ rather than a number. That is visible rather than silently wrong,
-- and the two halves barely meet in practice — a file with uncalculated
-- formulas was written by a script, and a script's other cells are plain.
odfSheets :: BS.ByteString -> ([(Text, Seq (Seq Text), Map (Int, Int) Text)], Bool)
odfSheets bs = finish (foldl' sstep ss0 evs)
  where
    -- No style table here: a spreadsheet's cell styles carry number formats,
    -- and the displayed text they produce is already in the file.
    evs = parseXmlBytes bs
    finish st = (reverse (ssDone (endTable st)), ssCut st)

data SS = SS
  { ssDone   :: ![(Text, Seq (Seq Text), Map (Int, Int) Text)]   -- ^ Finished sheets, reversed.
  , ssName   :: !Text
  , ssRows   :: ![(Seq Text, Int)]   -- ^ Finished rows and their repeat counts, reversed.
  , ssCells  :: ![(Text, Int, Bool)]
    -- ^ Cells of the row in progress, reversed: value, repeat count, and
    -- whether it must survive the trailing-empty trim. A cell with a formula
    -- and no value looks empty and is not: it is the one kind of cell
    -- "Cmedit.Formula" is going to fill in.
  , ssRowRep :: !Int                 -- ^ Repeat count of the row in progress.
  , ssVal    :: ![Text]              -- ^ Display text of the cell in progress, reversed.
  , ssRaw    :: !Text                -- ^ Its @office:value@ etc., as a fallback.
  , ssRep    :: !Int                 -- ^ Its repeat count.
  , ssForm   :: !Text                -- ^ Its formula, translated; empty if none.
  , ssCap    :: !Bool                -- ^ Capturing display text.
  , ssInCell :: !Bool
  , ssForms  :: !(Map (Int, Int) Text)
  , ssRowIx  :: !Int                 -- ^ Row index the next finished row will take.
  , ssCount  :: !Int                 -- ^ Cells materialised so far in this sheet.
  , ssCut    :: !Bool
  , ssOpen   :: !Bool                -- ^ A table is open.
  }

ss0 :: SS
ss0 = SS [] T.empty [] [] 1 [] T.empty 1 T.empty False False M.empty 0 0 False False

sstep :: SS -> XmlEvent -> SS
sstep st ev = case ev of
  XStart "table" as ->
    (endTable st) { ssName = fromMaybe (T.pack "Sheet") (xAttr "name" as), ssOpen = True }
  XEnd "table" -> endTable st

  XStart "table-row" as ->
    st { ssCells = [], ssRowRep = clampRep maxOdfRows (xAttrInt "number-rows-repeated" as) }
  XEnd "table-row" -> endRow st

  XStart "table-cell" as -> beginCell as st
  XStart "covered-table-cell" as -> beginCell as st
  XEnd "table-cell" -> endCell st
  XEnd "covered-table-cell" -> endCell st

  -- A cell's displayed text lives in its <text:p> children, which is what
  -- makes an .ods read better than an .xlsx: it is already formatted.
  XStart "p" _ | ssInCell st -> st { ssCap = True }
  XEnd "p"     | ssInCell st -> st { ssCap = False, ssVal = "\n" : ssVal st }
  XText t      | ssCap st    -> st { ssVal = t : ssVal st }
  _ -> st

beginCell :: [(Text, Text)] -> SS -> SS
beginCell as st = st
  { ssInCell = True, ssCap = False, ssVal = []
  , ssRep  = clampRep maxOdfCols (xAttrInt "number-columns-repeated" as)
  , ssRaw  = rawValue as
  , ssForm = maybe T.empty odfFormula (xAttr "formula" as)
  }

-- The cell's value as the file records it, for the rare cell with no displayed
-- text. Preferring the display text is the whole point (see 'odfSheets'), so
-- this is only ever a fallback.
rawValue :: [(Text, Text)] -> Text
rawValue as = case [ v | k <- keys, Just v <- [xAttr k as] ] of
  (v : _) -> v
  []      -> T.empty
  where keys = ["string-value", "date-value", "time-value", "boolean-value", "value"]

clampRep :: Int -> Maybe Int -> Int
clampRep hi = max 1 . min hi . fromMaybe 1

endCell :: SS -> SS
endCell st
  | not (ssInCell st) = st
  | otherwise = val `seq` st { ssCells = (val, ssRep st, not (T.null (ssForm st))) : ssCells st
                   , ssInCell = False, ssCap = False, ssVal = []
                   , ssForm = T.empty, ssRaw = T.empty, ssRep = 1
                   , ssForms = forms }
  where
    -- Forced by the 'seq' above before it is consed on: unforced, each cell is
    -- a thunk over the whole parser state, and a lazy 'T.copy' retains the
    -- decoded member it was written to release.
    shown = T.dropWhileEnd (== '\n') (T.copy (T.concat (reverse (ssVal st))))
    val | not (T.null shown) = shown
        | otherwise          = T.copy (ssRaw st)
    -- Only a formula the file did not already answer, and only for the first
    -- of a repeated run (a repeated formula cell would need its references
    -- shifted, which this does not do — so it is honestly not recorded).
    col = sum [ n | (_, n, _) <- ssCells st ]
    forms | not (T.null (ssForm st)), T.null val =
              M.insert (ssRowIx st, col) (ssForm st) (ssForms st)
          | otherwise = ssForms st

-- Close the row in progress, dropping a trailing run of empty cells rather
-- than materialising it — an .ods pads every row out to the sheet width with
-- one cell carrying number-columns-repeated="1024".
endRow :: SS -> SS
endRow st
  | null (ssCells st) = st { ssRowIx = ssRowIx st + ssRowRep st, ssRowRep = 1 }
  | otherwise =
      let cells = trimEnd (reverse (ssCells st))
          !row  = Seq.fromList (concat [ replicate n v | (v, n, _) <- cells ])
          n     = Seq.length row
      in st { ssRows = if n == 0 then ssRows st else (row, ssRowRep st) : ssRows st
            , ssCells = []
            , ssRowIx = ssRowIx st + ssRowRep st
            , ssRowRep = 1
            , ssCount = ssCount st + n * ssRowRep st
            , ssCut = ssCut st || ssCount st + n > maxOdfCells }
  where trimEnd = reverse . dropWhile (\(v, _, keep) -> T.null v && not keep) . reverse

-- Close the sheet in progress, dropping the trailing empty rows the same
-- padding produces.
endTable :: SS -> SS
endTable st
  | not (ssOpen st) = st { ssRows = [], ssCells = [], ssForms = M.empty, ssRowIx = 0 }
  | otherwise =
      let st'  = endRow st
          rows = reverse (ssRows st')
          grid = Seq.fromList (concat [ replicate n r | (r, n) <- rows ])
          w    = maximum (0 : map (Seq.length . fst) rows)
          padded = fmap (\r -> r <> Seq.replicate (max 0 (w - Seq.length r)) T.empty) grid
      in st' { ssDone = (ssName st', padded, ssForms st') : ssDone st'
             , ssRows = [], ssCells = [], ssForms = M.empty
             , ssRowIx = 0, ssOpen = False }

-- | Translate an OpenDocument formula into the syntax "Cmedit.Formula" reads.
--
-- ODF wraps every reference in brackets and prefixes a same-sheet one with a
-- dot: @of:=SUM([.A1:.B9])@ where Excel writes @=SUM(A1:B9)@. A cross-sheet
-- reference is @[Sheet2.A1]@, which becomes @Sheet2!A1@. The argument
-- separator is a semicolon, which the formula lexer already accepts.
--
-- Anything left over that does not look like a formula at all comes back
-- empty, and an empty formula is counted as one this reader could not do —
-- which is the honest answer, and the same one it gives for a function it
-- does not have.
odfFormula :: Text -> Text
odfFormula src0
  | T.null body = T.empty
  | otherwise   = T.pack (go (T.unpack body))
  where
    src  = T.strip src0
    body = T.dropWhile (== '=') (dropNs src)
    -- "of:=", "oooc:=", or bare "=".
    dropNs t = case T.breakOn ":=" t of
      (pre, rest) | not (T.null rest), T.all (/= ' ') pre -> T.drop 1 rest
      _ -> t
    go ('[' : rest) = ref (break (== ']') rest)
    go (c : rest)   = c : go rest
    go []           = []
    ref (inside, rest) = expand inside ++ go (drop 1 rest)
    -- Inside the brackets: ".A1", "Sheet2.A1", ".A1:.B9", or "#REF!".
    expand s = case break (== ':') s of
      (a, ':' : b) -> one a ++ ":" ++ one b
      _            -> one s
    one s = case break (== '.') s of
      ("", '.' : cell) -> cell                 -- same sheet
      (sh, '.' : cell) -> sheetName sh ++ "!" ++ cell
      _                -> s
    -- A sheet name with spaces is quoted with single quotes in ODF too, and
    -- that is exactly what the formula lexer expects.
    sheetName sh = sh
