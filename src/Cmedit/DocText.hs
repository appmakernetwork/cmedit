-- | Turning an already-parsed reading-view document into /searchable text/,
-- for the workspace search's "look inside documents" option.
--
-- The editor can already read a PDF, a Word or OpenDocument file, a workbook
-- and an e-book; each of those readers produces a view model, and each view
-- model can produce text. This module is the small piece in between: it flattens
-- a view model into lines a matcher can run over, and — the part that actually
-- matters — it says, for each of those lines, /where the reader would have to
-- go to show it/.
--
-- That second half is the whole design problem. A search result has to be
-- openable, and the obvious address (a line number) is worthless in these
-- views: their lines come out of 'Cmedit.Rtf.layoutRtf' \/ 'Cmedit.Pdf.layoutPdf'
-- and are a function of the terminal width, so the row a match sits on today is
-- a different row in a wider window. Every one of these formats does, however,
-- have a unit that is intrinsic to the document and that its view can already
-- navigate to — a page, a chapter, a paragraph, a sheet and cell — because
-- 'Cmedit.Editor' reinterprets Go To Line as exactly that in each of them. So
-- an extracted line is addressed by its unit, and 'DocUnit' is the result.
--
-- The module is pure and imports only the format readers, so the walker can
-- call it off the main thread and 'test\/Spec.hs' can exercise it without a
-- terminal or a file.
module Cmedit.DocText
  ( DocExtract(..)
  , DocUnit(..)
  , extractPdf
  , extractRtf
  , extractBook
  , docMatches
  , maxExtractLines
  , maxExtractChars
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Csv (CsvView)
import qualified Cmedit.Csv as Csv
import Cmedit.Pdf (PdfDoc)
import qualified Cmedit.Pdf as Pdf
import Cmedit.Rtf (RtfDoc)
import qualified Cmedit.Rtf as Rtf
import Cmedit.Search (DocKind(..), Match(..))
import qualified Cmedit.Search as S
import Cmedit.Xlsx (Workbook)
import qualified Cmedit.Xlsx as Xlsx

------------------------------------------------------------------------------
-- Caps

-- | Cap on extracted lines. Every reader already bounds its own work (pages,
-- cells, chapters), so this is the backstop for the product of those bounds —
-- a 2 000-page PDF of dense text, or a workbook at 'Cmedit.Xlsx.maxSheetCells'.
maxExtractLines :: Int
maxExtractLines = 200000

-- | Cap on total extracted characters, which is the honest measure for a
-- document whose text is a handful of enormous paragraphs rather than many
-- small ones.
maxExtractChars :: Int
maxExtractChars = 16 * 1024 * 1024

------------------------------------------------------------------------------
-- The extract

-- | Where one extracted line lives, in terms its reading view can navigate to.
--
-- @duIndex@ is what Go To Line means in that view — a page number for a PDF, a
-- chapter for an e-book, a sheet for a workbook, a paragraph for a document —
-- and @duLabel@ is the short string the results panel shows in place of a line
-- number. They are separate because the label is not always derivable from the
-- index: a workbook cell is @Sheet 2@ plus @B4@, and only the first of those
-- is a navigation target.
data DocUnit = DocUnit
  { duIndex :: !Int     -- ^ 1-based navigation target within the view.
  , duLabel :: !Text    -- ^ Short display label, e.g. @p.12@, @B4@, @\x00b6 31@.
  } deriving (Eq, Show)

-- | A document flattened for searching: the text, and one 'DocUnit' per line of
-- it. @dxUnits@ is the same length as the number of lines in @dxText@, so a
-- match's line number indexes straight into it.
data DocExtract = DocExtract
  { dxKind  :: !DocKind
  , dxText  :: !Text
  , dxUnits :: !(Seq DocUnit)
  } deriving (Eq, Show)

-- | Assemble an extract from tagged lines, applying both caps.
--
-- The lines are built lazily by the callers below and consumed once here, so a
-- document that blows the character cap stops being decoded at that point
-- rather than being fully materialised and then trimmed.
mkExtract :: DocKind -> [(Text, DocUnit)] -> DocExtract
mkExtract kind ls =
  let taken = takeChars 0 (take maxExtractLines ls)
  in DocExtract { dxKind  = kind
                , dxText  = T.intercalate nl (map fst taken)
                , dxUnits = Seq.fromList (map snd taken) }
  where
    nl = T.singleton '\n'
    takeChars _ [] = []
    takeChars !n (x@(t, _) : rest)
      | n > maxExtractChars = []
      | otherwise           = x : takeChars (n + T.length t + 1) rest

------------------------------------------------------------------------------
-- Running a search over an extract

-- | The matches of a per-line matcher in an extract, with each match's 'mUnit'
-- filled in.
--
-- 'Cmedit.Search.fileMatchesWith' does the matching, exactly as for a source
-- file — the only difference between searching a PDF and searching a @.hs@ is
-- what produced the text and how the answer is addressed. Taking the same
-- per-line function the walker already builds keeps it that way.
docMatches :: (Text -> [(Int, Int)]) -> DocExtract -> ([Match], Bool, Int)
docMatches perLine dx =
  let (ms, trunc, cnt) = S.fileMatchesWith perLine (dxText dx)
  in (map tag ms, trunc, cnt)
  where
    tag mt = case Seq.lookup (mLine mt) (dxUnits dx) of
      Just u  -> mt { mUnit = Just (duIndex u, duLabel u) }
      Nothing -> mt

------------------------------------------------------------------------------
-- PDF: the unit is the page

-- | Extract a PDF's text, one line per paragraph, addressed by page.
--
-- Paragraphs and not laid-out lines, for the reason 'Cmedit.Pdf.pdfPlainText'
-- gives: the laid-out ones are a function of the window width. It also makes
-- the search agree with the view's own in-file find, which runs over the same
-- reflowed paragraph text.
extractPdf :: PdfDoc -> DocExtract
extractPdf pd = mkExtract DKPdf
  [ (txt, DocUnit n (T.pack ("p." ++ show n)))
  | (n, pg) <- zip [1 ..] (toList (Pdf.pdPages pd))
  , txt <- pageParTexts pg
  , not (T.null (T.strip txt)) ]

-- One text per paragraph of a page (blank paragraphs included, and dropped by
-- the caller — they are layout, and there is nothing in them to match).
pageParTexts :: Pdf.PdfPage -> [Text]
pageParTexts pg =
  [ if Pdf.ppKind p == Pdf.PKBlank
      then T.empty
      else T.concat (map Pdf.psText (Pdf.ppRuns p))
  | p <- Pdf.pgPars pg ]

------------------------------------------------------------------------------
-- Formatted documents: the unit is the chapter, or failing that the paragraph

-- | Extract a formatted document (DOCX, ODT, EPUB or RTF), one line per
-- paragraph.
--
-- An e-book carries sections ('Cmedit.Rtf.rdSects' — its spine), and those are
-- both what the view navigates by and what a reader thinks in, so they win.
-- A Word or OpenDocument file has no such structure, and its paragraph index is
-- the honest remaining answer: intrinsic to the document, stable under a
-- resize, and something 'Cmedit.Rtf.rtfGoToPar' can act on.
extractRtf :: DocKind -> RtfDoc -> DocExtract
extractRtf kind rd = mkExtract kind
  [ (txt, unit)
  | (txt, unit) <- zip (map parText (toList (Rtf.rdPars rd))) units
  , not (T.null (T.strip txt)) ]
  where
    parText p = T.concat (map Rtf.rrText (Rtf.rpRuns p))

    -- One unit per paragraph, produced by walking the section starts alongside
    -- the paragraphs rather than searching them per paragraph: the naive
    -- version is quadratic in (paragraphs x chapters), which on a novel is
    -- millions of comparisons for an answer that advances monotonically.
    units = go 0 1 (toList (Rtf.rdSects rd))
      where
        go !i !k ss = case ss of
          ((start, _) : rest) | start <= i -> go i (k + 1) rest
          _ | null (Rtf.rdSects rd) -> DocUnit (i + 1) (parLabel (i + 1)) : go (i + 1) k ss
            | otherwise            -> DocUnit (max 1 (k - 1)) (chLabel (max 1 (k - 1)))
                                        : go (i + 1) k ss

    parLabel n = T.pack ("\x00b6" ++ show n)
    chLabel  n = T.pack ("ch." ++ show n)

------------------------------------------------------------------------------
-- Workbooks: the unit is the sheet, and the label is the cell

-- | Extract a workbook, one line per non-empty cell, addressed by sheet and
-- cell reference.
--
-- One line per /cell/ rather than per row, because a cell is the unit this
-- grid can put a cursor on — the same judgement 'Cmedit.EditorFind.csvFindWith'
-- makes for the table view's own find. The cost is that a term spanning two
-- columns is not found, which is correct: those are two values that happen to
-- be adjacent, not a phrase.
extractBook :: Workbook -> DocExtract
extractBook wb = mkExtract DKSheet
  [ (cell, DocUnit (s + 1) (Xlsx.cellName r c))
  | (s, v) <- zip [0 ..] (toList (Xlsx.wbSheets wb))
  , (r, c, cell) <- sheetCells v ]

-- Non-empty cells of one sheet, in row-major order.
sheetCells :: CsvView -> [(Int, Int, Text)]
sheetCells v =
  [ (r, c, cell)
  | r <- [0 .. Csv.nRows v - 1]
  , c <- [0 .. Csv.nCols v - 1]
  , let cell = Csv.cellAt r c v
  , not (T.null cell) ]
