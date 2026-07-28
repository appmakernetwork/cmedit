-- | The crash-safe edit journal: its file format, its naming, and the recovery
-- decision. Pure — every byte of IO (writing a journal, scanning the journal
-- directory, removing one) lives in "Cmedit.App", which is what lets the whole
-- format and the whole recovery rule be unit-tested without a terminal or a
-- filesystem.
--
-- __Why a journal at all.__ A long session's other risk, next to slowing down,
-- is evaporating: an SSH drop, a closed window, an OOM kill. The editor already
-- has @bracket@/@finally@ teardown and a SIGTERM handler that persists recents
-- and input history, but unsaved /buffer content/ is gone. A journal is the
-- cheap fix every comparable editor has (nano's @.save@, vim's swap files,
-- VS Code's hot exit), and it costs no new dependency and no new subsystem —
-- an 'Cmedit.EditorState.Effect', a driver handler, and this module.
--
-- __The never-endanger-the-real-file principle.__ A journal is a separate file
-- in a separate directory (@~\/.cache\/cmedit\/journal@); the save path is not
-- touched by any of this. Nothing here can rename, truncate or overwrite the
-- document it describes, and a journal write that fails is a status-line note
-- — never an error dialog, never a block, never a reason not to save. That is
-- also why 'jReadOnly' is recorded: the editor happily journals edits to a file
-- it cannot write (the user may want to Save As elsewhere), but recovery must
-- not then offer to write back, and the flag has to survive in the journal
-- because at recovery time there is no open document to ask.
--
-- __The format.__ A version line, @key: value@ headers, a @--@ separator line,
-- then the body — the whole buffer text, UTF-8, verbatim:
--
-- > cmedit-journal 1
-- > path: "/home/ben/work/x.py"
-- > mtime: 1721890123456000000000
-- > eol: lf
-- > bom: none
-- > final-newline: yes
-- > read-only: no
-- > cursor: 412:7
-- > --
-- > <the full buffer text, lines joined with LF, no trailing separator>
--
-- Full text, not a delta: simple, verifiable, and a buffer is small next to the
-- write budget (one write per 2 s of active typing). Compression would be
-- complexity with nothing to buy it.
--
-- Four decisions in that format are load-bearing:
--
-- * __The body is the lines joined with LF and nothing else.__ No trailing
--   separator is appended, so @lines == ["a", ""]@ (a buffer ending in a blank
--   line) serialises to @"a\\n"@ and @["a"]@ to @"a"@ — a distinction that is
--   lost the moment a writer adds a courtesy newline. The file's /own/ final
--   newline is metadata (@final-newline:@), exactly as 'Cmedit.TextBuffer'
--   models it, and never touches the body. Splitting the body back on @'\\n'@
--   is therefore an exact inverse, which also means a CR that somehow ended up
--   inside a line survives as an ordinary character rather than becoming a line
--   break.
--
-- * __The path is @show@-escaped.__ A POSIX filename may contain a newline, and
--   an unquoted path would then split the header block and desynchronise the
--   parse. Escaping it is the same trick the find/replace history file uses for
--   multi-line terms, and it keeps the header block pure ASCII.
--
-- * __The baseline mtime is an exact integer,__ picoseconds since the Unix
--   epoch, not the decimal the plan sketched. Recovery's central question is
--   "does this baseline still /equal/ the file's mtime?", and a lossy decimal
--   makes the answer no for a file nobody touched — the clean-recovery case
--   would simply never fire on a filesystem with sub-second timestamps.
--
-- * __Unknown header keys are ignored and missing ones default.__ A journal is
--   written by one version of the editor and read by whichever version starts
--   next; a key added later must not make an older-but-still-valid journal
--   unreadable. An unknown /version/ is a different matter and is rejected
--   outright: the body's meaning is what a version number governs.
module Cmedit.Journal
  ( -- * The record
    Journal(..)
  , journalVersion
  , journalMagic
    -- * Serialisation
  , serializeJournal
  , parseJournal
    -- * Buffer text
  , bufferJournalText
  , journalBuffer
    -- * Naming a journal file
  , journalExtension
  , journalFileName
  , isJournalFileName
  , untitledIndexOf
  , pathHash
  , sanitizeBase
    -- * Naming a session file / snapshot directory
  , sessionExtension
  , sessionKeyName
  , sessionFileName
  , isSessionFileName
    -- * The recovery decision
  , RecoveryCase(..)
  , classifyJournal
  , canWriteBack
  , recoveryNote
    -- * Timestamps
  , diskTimeToPicos
  , picosToDiskTime
  ) where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAlphaNum, ord)
import Data.Foldable (toList)
import Data.List (foldl', stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Calendar (Day(ModifiedJulianDay), toModifiedJulianDay)
import Data.Time.Clock (UTCTime(..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Word (Word64)
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

import Cmedit.Types (Pos(..))
import Cmedit.TextBuffer
  ( Buffer(bufLines), DiskTime, Encoding(..), LineEnding(..), fromText )

------------------------------------------------------------------------------
-- The record

-- | Everything a recovery needs to reconstruct a modified document, and
-- nothing else.
--
-- The metadata fields mirror what a /save/ needs — 'jEnc', 'jEol' and
-- 'jFinalNewline' are exactly 'Cmedit.TextBuffer.saveFile'\'s encoding,
-- line-ending and @final@ arguments (the last being
-- 'Cmedit.TextBuffer.lrFinalNewline': whether the file's last line is followed
-- by a line terminator). Recovering a document and pressing Ctrl+S must write
-- the same bytes the session would have written, which means the journal has to
-- carry the file's shape and not just its text.
data Journal = Journal
  { jPath         :: !(Maybe FilePath)
    -- ^ The document's canonical path; 'Nothing' for an untitled buffer.
  , jMtime        :: !(Maybe DiskTime)
    -- ^ The on-disk baseline at load (@edDiskMtime@), against which recovery
    -- asks whether the file has moved underneath these edits. 'Nothing' when
    -- the file did not exist (a named-but-never-saved buffer).
  , jEol          :: !LineEnding
  , jEnc          :: !Encoding
  , jFinalNewline :: !Bool
  , jReadOnly     :: !Bool
    -- ^ The document was loaded read-only. Journalled anyway (the edits are
    -- real and Save As can rescue them), but recovery must not offer to write
    -- back — see 'canWriteBack'.
  , jCursor       :: !Pos
  , jText         :: !Text
    -- ^ The whole buffer: its lines joined with LF, with no trailing
    -- separator. Build it with 'bufferJournalText' and undo it with
    -- 'journalBuffer'.
  } deriving (Eq, Show)

-- | The format version. Bump it when the body's /meaning/ changes; adding a
-- header key does not need it, because unknown keys are ignored on read.
journalVersion :: Int
journalVersion = 1

-- | The first token of the first line. Also what 'parseJournal' uses to reject
-- a file that is not a journal at all (the directory is ours, but a stray file
-- in @~\/.cache@ costs nothing to survive).
journalMagic :: String
journalMagic = "cmedit-journal"

------------------------------------------------------------------------------
-- Buffer text

-- | The buffer as a journal body: lines joined with LF, no trailing separator.
--
-- Deliberately not 'Cmedit.TextBuffer.bufferToText', which serialises for
-- /disk/ — it applies the file's line ending and its final newline, both of
-- which the journal records as metadata instead. Keeping the body in one
-- canonical shape means a document whose line ending is toggled between two
-- journal writes does not rewrite every byte of the body.
bufferJournalText :: Buffer -> Text
bufferJournalText b = T.intercalate (T.pack "\n") (toList (bufLines b))

-- | The inverse of 'bufferJournalText'.
--
-- The trailing @"\\n"@ is the sentinel that makes this exact:
-- 'Cmedit.TextBuffer.fromText' strips one final newline before splitting (that
-- is how it distinguishes a file ending in a newline from one that does not),
-- so without it a buffer whose last line is empty would come back one line
-- short. With it, @["a", ""]@ → @"a\\n"@ → @"a\\n\\n"@ → @["a", ""]@.
--
-- The one thing this does not preserve is a literal CR /inside/ a line:
-- 'Cmedit.TextBuffer.fromText' normalises newlines, so it would become a line
-- break. No loaded buffer can contain one (loading normalises when the file has
-- any CR at all) and no paste can (@insertText@ normalises too), which is why
-- the convenience is worth more here than a private buffer constructor.
journalBuffer :: Journal -> Buffer
journalBuffer j = fromText (jText j <> T.pack "\n")

------------------------------------------------------------------------------
-- Serialisation

-- | Render a journal to the bytes that go in the file. Total.
serializeJournal :: Journal -> BS.ByteString
serializeJournal j = BSL.toStrict (BB.toLazyByteString bld)
  where
    bld =
      hline (journalMagic ++ " " ++ show journalVersion)
        <> maybe mempty (\p -> hdr "path" (show p)) (jPath j)
        <> maybe mempty (\t -> hdr "mtime" (show (diskTimeToPicos t))) (jMtime j)
        <> hdr "eol" (eolWord (jEol j))
        <> hdr "bom" (bomWord (jEnc j))
        <> hdr "final-newline" (yesNo (jFinalNewline j))
        <> hdr "read-only" (yesNo (jReadOnly j))
        <> hdr "cursor" (show (posLine (jCursor j)) ++ ":" ++ show (posCol (jCursor j)))
        <> hline separatorLine
        <> BB.byteString (TE.encodeUtf8 (jText j))
    hdr k v = hline (k ++ ": " ++ v)
    hline s = BB.stringUtf8 s <> BB.word8 10

-- | Parse a journal file. 'Nothing' means "not a journal we can use" — a bad
-- magic line, an unknown version, a missing @--@ separator, or NUL bytes in the
-- body. Never throws: a corrupt journal is a journal we skip, not a crash on
-- the startup path.
--
-- The NUL rejection is not fussiness. The body becomes a text buffer, and
-- 'Cmedit.TextBuffer.looksBinary' would refuse such content coming from a real
-- file; a journal must not be the back door that gets it in. It is the one
-- whole-body scan here, and it is a single cheap pass.
parseJournal :: BS.ByteString -> Maybe Journal
parseJournal bs0 = do
  (verLine, rest0) <- takeLine bs0
  ver <- versionOf (decodeLine verLine)
  ensure (ver == journalVersion)
  (hdrs, body) <- headerBlock rest0 []
  ensure (not (0 `BS.elem` body))
  let look k       = lookup (T.pack k) hdrs
      str k        = look k >>= \v -> readMaybe (T.unpack v) :: Maybe String
      int k        = look k >>= \v -> readMaybe (T.unpack v) :: Maybe Integer
      word k d f   = fromMaybe d (look k >>= f)
      curs         = fromMaybe (Pos 0 0) (look "cursor" >>= parseCursor)
  pure Journal
    { jPath         = str "path"
    , jMtime        = picosToDiskTime <$> int "mtime"
    , jEol          = word "eol" LF wordEol
    , jEnc          = word "bom" Utf8 wordBom
    , jFinalNewline = word "final-newline" True wordYesNo
    , jReadOnly     = word "read-only" False wordYesNo
    , jCursor       = curs
    , jText         = TE.decodeUtf8With TEE.lenientDecode body
    }

-- | The line that ends the header block. Its own line, exactly, so a header
-- value can never be mistaken for it.
separatorLine :: String
separatorLine = "--"

-- | Split off one LF-terminated line. A journal always has more lines after the
-- one being read, so an unterminated tail here is a truncated file — 'Nothing'.
takeLine :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
takeLine bs = case BS.elemIndex 10 bs of
  Nothing -> Nothing
  Just i  -> Just (BS.take i bs, BS.drop (i + 1) bs)

-- | Read @key: value@ lines up to the separator, returning them and the raw
-- body bytes that follow. A missing separator is a truncated or foreign file.
-- Lines without a colon are skipped rather than fatal, for the same forward
-- compatibility reason unknown keys are.
headerBlock :: BS.ByteString -> [(Text, Text)]
            -> Maybe ([(Text, Text)], BS.ByteString)
headerBlock bs acc = do
  (raw, rest) <- takeLine bs
  let ln = decodeLine raw
  if ln == T.pack separatorLine
    then pure (reverse acc, rest)
    else headerBlock rest $ case T.breakOn (T.pack ":") ln of
           (k, v) | T.null v  -> acc
                  | otherwise -> (T.strip k, T.strip (T.drop 1 v)) : acc

-- | A header line as text. Leniently decoded (a journal with a mangled header
-- should still give up its body) and stripped of a trailing CR, so a journal
-- that has been through a CRLF-converting tool still parses.
decodeLine :: BS.ByteString -> Text
decodeLine = T.dropWhileEnd (== '\r') . TE.decodeUtf8With TEE.lenientDecode

versionOf :: Text -> Maybe Int
versionOf ln = case T.words ln of
  [m, v] | m == T.pack journalMagic -> readMaybe (T.unpack v)
  _                                 -> Nothing

parseCursor :: Text -> Maybe Pos
parseCursor v = case T.splitOn (T.pack ":") v of
  [a, b] -> Pos <$> readMaybe (T.unpack a) <*> readMaybe (T.unpack b)
  _      -> Nothing

ensure :: Bool -> Maybe ()
ensure c = if c then Just () else Nothing

eolWord :: LineEnding -> String
eolWord LF   = "lf"
eolWord CRLF = "crlf"
eolWord CR   = "cr"

wordEol :: Text -> Maybe LineEnding
wordEol v = lookup (T.unpack v) [("lf", LF), ("crlf", CRLF), ("cr", CR)]

bomWord :: Encoding -> String
bomWord Utf8    = "none"
bomWord Utf8Bom = "utf8"

wordBom :: Text -> Maybe Encoding
wordBom v = lookup (T.unpack v) [("none", Utf8), ("utf8", Utf8Bom)]

yesNo :: Bool -> String
yesNo True  = "yes"
yesNo False = "no"

wordYesNo :: Text -> Maybe Bool
wordYesNo v = lookup (T.unpack v) [("yes", True), ("no", False)]

------------------------------------------------------------------------------
-- Timestamps
--
-- The exactness requirement is the whole reason these exist: the recovery
-- decision compares a parsed baseline against a freshly stat'd 'UTCTime' with
-- (==), so the round trip must be the identity. Modified-Julian-day arithmetic
-- gives that by construction — it is integer arithmetic over the two fields
-- 'UTCTime' actually stores, with no calendar or leap-second machinery in the
-- way. (A time /at/ a leap second, @utctDayTime == 86400s@, normalises to
-- midnight of the next day; no filesystem timestamp is one.)

-- | Picoseconds in a day.
picosPerDay :: Integer
picosPerDay = 86400 * 1000000000000

-- | The Unix epoch as a Modified Julian Day.
epochMJD :: Integer
epochMJD = 40587

-- | An on-disk timestamp as picoseconds since the Unix epoch.
diskTimeToPicos :: DiskTime -> Integer
diskTimeToPicos t =
  (toModifiedJulianDay (utctDay t) - epochMJD) * picosPerDay
    + diffTimeToPicoseconds (utctDayTime t)

-- | Inverse of 'diskTimeToPicos'.
picosToDiskTime :: Integer -> DiskTime
picosToDiskTime p =
  let (d, r) = p `divMod` picosPerDay
  in UTCTime (ModifiedJulianDay (d + epochMJD)) (picosecondsToDiffTime r)

------------------------------------------------------------------------------
-- Naming a journal file

-- | The journal file extension.
journalExtension :: String
journalExtension = ".cmj"

-- | Does this directory entry look like one of ours? Used by the startup scan
-- so an unrelated file in the cache directory is left alone.
isJournalFileName :: FilePath -> Bool
isJournalFileName n = journalExtension `isSuffix` n
  where isSuffix s x = drop (length x - length s) x == s && length x > length s

-- | The basename of the journal file for a document — never a directory, so
-- the driver owns where the journal directory is.
--
-- For a document with a path the name is @\<hash\>-\<basename\>.cmj@: the hash
-- is what actually identifies the file (paths are long, contain separators, and
-- can hold anything a filename cannot), and the sanitised basename is there
-- purely so a human looking in @~\/.cache@ can tell which journal is which. The
-- @Int@ disambiguates untitled buffers, which have nothing to hash — it is the
-- document's stable per-session id (@edDocId@), not its position in the
-- document list, which shifts as files open and close.
--
-- The hash is FNV-1a 64-bit, matching 'Cmedit.Link.linkIdOf': there is no
-- crypto in the boot libraries, and none is needed. A collision would offer one
-- document's recovery under another's name; at 64 bits over the handful of
-- files one user edits, that is not a risk worth a dependency. The identity
-- that matters is checked anyway — the recovered @path:@ header, not the
-- filename, is what recovery reopens.
journalFileName :: Maybe FilePath -> Int -> FilePath
journalFileName Nothing n     = "untitled-" ++ show n ++ journalExtension
journalFileName (Just path) _ =
  pathHash path ++ "-" ++ sanitizeBase (takeFileName path) ++ journalExtension

-- | The exact inverse of @'journalFileName' Nothing@: the buffer number an
-- untitled journal is named for, or 'Nothing' for anything else.
--
-- It matters twice at startup. A recovered untitled buffer must take the id
-- back, or the write-behind would write it to a /second/ file and leave the
-- first for the next startup to offer again; and the document-id counter has
-- to be seeded past every untitled journal still on disk, or a fresh untitled
-- buffer would be numbered over one the user asked to keep.
untitledIndexOf :: FilePath -> Maybe Int
untitledIndexOf name = do
  rest <- stripPrefix "untitled-" (takeFileName name)
  let digits = takeWhile (/= '.') rest
  ensure (drop (length digits) rest == journalExtension)
  ensure (not (null digits) && all (\c -> c >= '0' && c <= '9') digits)
  readMaybe digits

-- | FNV-1a over the path's characters, as 16 lower-case hex digits.
pathHash :: FilePath -> String
pathHash = toHex . foldl' step fnvBasis
  where
    fnvBasis = 14695981039346656037 :: Word64
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211
    toHex w  = [ hexDigit (fromIntegral ((w `div` (16 ^ i)) `mod` 16))
               | i <- [15, 14 .. 0 :: Int] ]
    hexDigit d = ("0123456789abcdef" :: String) !! d

-- | A basename reduced to something safe on every filesystem we might be run
-- on, and short enough that the whole journal name stays well inside the
-- classic 255-byte limit. Only ever cosmetic — see 'journalFileName'.
maxBaseChars :: Int
maxBaseChars = 40

sanitizeBase :: FilePath -> String
sanitizeBase name =
  case take maxBaseChars (map keep name) of
    [] -> "file"
    s  -> s
  where
    keep c | isAlphaNum c && ord c < 128 = c
           | c `elem` (".-_" :: String)  = c
           | otherwise                   = '-'

------------------------------------------------------------------------------
-- Naming a session file and its snapshot directory
--
-- The same argument 'journalFileName' makes, made once more rather than
-- differently: the hash is the identity (a folder path is long, contains
-- separators and may contain anything a filename cannot) and the sanitised
-- basename is there purely so a human looking in ~/.config or ~/.cache can tell
-- which is which.
--
-- Naming it this way is what makes the cwd lookup O(1) and listing-free: given
-- a canonical $PWD the session file's *name* is computable, so @--restore@ is
-- one 'doesFileExist' and one read. Only the File menu, which genuinely wants
-- all of them, lists the directory.

-- | The per-workspace session file extension.
sessionExtension :: String
sessionExtension = ".session"

-- | The key identifying one session: a workspace folder, or 'Nothing' for the
-- folderless session. It names both @~\/.config\/cmedit\/sessions\/\<key\>.session@
-- and @~\/.cache\/cmedit\/snapshots\/\<key\>\/@, so the snapshot directory for a
-- session can be found from the session and vice versa without a parse.
sessionKeyName :: Maybe FilePath -> FilePath
sessionKeyName Nothing  = "no-folder"
sessionKeyName (Just p) = pathHash p ++ "-" ++ sanitizeBase (takeFileName p)

-- | The basename of the session file for a workspace folder — never a
-- directory, so the driver owns where the sessions directory is.
--
-- A folder that is renamed therefore loses its session, which is the honest
-- answer: the paths inside it are stale too, and it ages out under the
-- directory cap.
sessionFileName :: FilePath -> FilePath
sessionFileName p = sessionKeyName (Just p) ++ sessionExtension

-- | Does this directory entry look like one of ours? Used by the sessions
-- directory's listing and cap so an unrelated file is left alone.
isSessionFileName :: FilePath -> Bool
isSessionFileName n =
  length n > length sessionExtension
    && drop (length n - length sessionExtension) n == sessionExtension

------------------------------------------------------------------------------
-- The recovery decision

-- | Which of the four situations a journal found at startup is in. Total over
-- (has a path?, had a baseline?, is there a file now?) — see 'classifyJournal'.
data RecoveryCase
  = RecoverUntitled
    -- ^ No path: recover as an untitled buffer. There is nothing on disk to
    -- compare against and nothing that could have changed.
  | RecoverClean
    -- ^ The file is exactly as these edits left it (or, for a
    -- named-but-never-created file, still absent as it was). Offer recovery
    -- with no caveat.
  | RecoverChanged
    -- ^ The file has moved underneath the journal — someone else wrote it, or
    -- it appeared where the session had none. Still offer, but say so: the
    -- editor already communicates this concept with the @◆@ stale marker, and
    -- the user is the one who knows which version they want.
  | RecoverMissing
    -- ^ The file the journal describes is gone. Still offer — the journal may
    -- be the only surviving copy of that content, which is precisely when
    -- recovery earns its keep — but flag it, because "Save" will be creating
    -- the file rather than updating it.
  deriving (Eq, Show)

-- | The recovery decision: a journal plus the file's /current/ on-disk mtime
-- ('Nothing' meaning no such file now).
--
-- The two rows worth spelling out are the ones with no baseline. A journal with
-- a path but no @mtime:@ is a buffer that was named before it existed
-- (@cmedit newfile.txt@, or a Save As that never happened): if the file is
-- still absent, nothing has changed and that is 'RecoverClean' — reporting a
-- file "missing" that never existed would be a lie. If something is there now,
-- it appeared from elsewhere, and the user needs to be told before these edits
-- land on top of it.
classifyJournal :: Journal -> Maybe DiskTime -> RecoveryCase
classifyJournal j now = case jPath j of
  Nothing -> RecoverUntitled
  Just _  -> case (jMtime j, now) of
    (Nothing, Nothing)      -> RecoverClean
    (Nothing, Just _)       -> RecoverChanged
    (Just _,  Nothing)      -> RecoverMissing
    (Just base, Just cur)
      | base == cur         -> RecoverClean
      | otherwise           -> RecoverChanged

-- | May recovery offer to write this document back to its own path?
--
-- No for an untitled buffer (there is nowhere to write) and no for a read-only
-- one: the session journalled those edits so they could be rescued with Save
-- As, not so a recovery dialog could march them at a file the user cannot
-- write. Everything else — including a file that changed on disk or vanished —
-- is the user's call, which is what the flags in 'recoveryNote' are for.
canWriteBack :: Journal -> Bool
canWriteBack j = not (jReadOnly j) && jPath j /= Nothing

-- | The short caveat to show beside a recoverable file, if any.
recoveryNote :: RecoveryCase -> Maybe Text
recoveryNote RecoverUntitled = Nothing
recoveryNote RecoverClean    = Nothing
recoveryNote RecoverChanged  = Just (T.pack "changed on disk since these edits")
recoveryNote RecoverMissing  = Just (T.pack "no longer exists on disk")
