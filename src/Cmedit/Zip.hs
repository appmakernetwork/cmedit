-- | ZIP archives as a read-only listing: a file tree instead of @"binary
-- file — cannot be edited"@.
--
-- A @.zip@ (and everything built on it — @.jar@, @.docx@, @.epub@, @.apk@)
-- is binary, so 'Cmedit.TextBuffer.looksBinary' refuses it and the editor
-- shows nothing at all. But an archive already carries a complete,
-- self-describing table of contents: the /central directory/, a run of
-- fixed-layout records at the end of the file naming every member with its
-- sizes, timestamp and compression method. Reading it is a parse, not a
-- decompression — which is why this module needs no decompressor and why
-- entry encryption is irrelevant to it. Encrypted members are listed like any
-- other and flagged; only their /contents/ are locked, and this view never
-- looks at contents.
--
-- __The listing is text, deliberately.__ Rather than a sixth view mode with
-- its own state, key handler and renderer, 'zipListing' returns the tree as
-- a 'Text' document that "Cmedit.App" installs as an ordinary /read-only/
-- buffer — the same trick "Cmedit.Manual" plays with its pseudo-path. Search,
-- word wrap, selection and copy, the scroll bars and Go To Line all work
-- because there is nothing new for them to know about, and the read-only flag
-- (there is no serialiser back to ZIP, and could not be one) means the archive
-- can never be overwritten by its own table of contents. Compare "Cmedit.Rtf",
-- which is derived-and-read-only for the same reason but needs a real view
-- mode because it carries per-character formatting a buffer cannot hold.
--
-- __Reading is size-independent.__ The central directory lives at the /end/ of
-- the file and is proportional to the entry count, not the archive size, so
-- 'findEocd' works on a tail chunk and 'parseCentral' on the directory block
-- alone. A 10 GB archive costs the same two short reads as a 10 KB one; the
-- driver never slurps the file, and 'Cmedit.EditorState.maxOpenBytes' does not
-- apply.
module Cmedit.Zip
  ( -- * Sniffing
    zipMagic
  , archiveExtensions
    -- * The archive's own records
  , ZipEntry(..)
  , Eocd(..)
  , eocdSearchBytes
  , maxCentralBytes
  , findEocd
  , parseCentral
    -- * The listing
  , zipListing
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Char (chr, isControl)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Cmedit.Width (lineDisplayWidth)

------------------------------------------------------------------------------
-- Sniffing

-- | Does this file start with a ZIP local-file or end-of-central-directory
-- signature? Checked on the first four bytes, exactly like the image sniff, so
-- an archive is recognised by what it /is/ rather than what it is called —
-- a @.docx@ or a @.whl@ gets the listing too.
--
-- @PK\\3\\4@ is the usual first local header, @PK\\5\\6@ an archive with no
-- members (the end record is the whole file) and @PK\\7\\8@ the first segment
-- of a spanned archive. A self-extracting archive starts with its extractor
-- stub and so is not matched — nothing here can tell that from any other
-- executable without scanning the tail of every binary we are handed.
zipMagic :: BS.ByteString -> Bool
zipMagic bs =
  BS.length bs >= 4 && BS.take 2 bs == BS.pack [0x50, 0x4b]
  && BS.index bs 2 `elem` [3, 5, 7] && BS.index bs 3 == BS.index bs 2 + 1

-- | Extensions of the ZIP-based formats, for the listing's syntax lexer and
-- the explorer's file-kind colouring. Detection itself is by 'zipMagic'; this
-- list only decides what gets /coloured/ like an archive, so being incomplete
-- costs nothing but a plain-white listing.
archiveExtensions :: [String]
archiveExtensions =
  [ "zip", "jar", "war", "ear", "apk", "aar", "ipa", "xpi", "crx", "vsix"
  , "docx", "xlsx", "pptx", "docm", "xlsm", "pptm"
  , "odt", "ods", "odp", "odg"
  , "epub", "whl", "egg", "nupkg", "kmz", "cbz", "usdz"
  ]

-- | How much of the file's tail to hand 'findEocd'. The end record is at most
-- 22 bytes plus a 64 KiB comment, and the ZIP64 records that may precede it
-- add another ~76, so this window always contains all of them.
eocdSearchBytes :: Int
eocdSearchBytes = 128 * 1024

-- | Ceiling on the central directory read. Roughly 46 bytes plus a name per
-- member, so this is about a million entries — far past anything a terminal
-- listing is useful for, and a bound on what a corrupt end record can make the
-- driver allocate.
maxCentralBytes :: Integer
maxCentralBytes = 64 * 1024 * 1024

------------------------------------------------------------------------------
-- The records

-- | One member of the archive, as the central directory describes it. Sizes
-- are 'Integer' because ZIP64 makes them 64-bit.
data ZipEntry = ZipEntry
  { zeName      :: !Text              -- ^ Path within the archive, always @\/@-separated (ZIP mandates it).
  , zeSize      :: !Integer           -- ^ Uncompressed size in bytes.
  , zePacked    :: !Integer           -- ^ Compressed size in bytes.
  , zeMethod    :: !Int               -- ^ Compression method code; see 'methodName'.
  , zeEncrypted :: !Bool              -- ^ General-purpose bit 0: the /contents/ are encrypted (the name and sizes never are).
  , zeDir       :: !Bool              -- ^ An explicit directory member.
  , zeTime      :: !(Maybe (Int, Int, Int, Int, Int))  -- ^ MS-DOS modification stamp as @(year, month, day, hour, minute)@; 'Nothing' when unset.
  } deriving (Eq, Show)

-- | The end-of-central-directory record: where the table of contents is and
-- how big it is. ZIP64 values are already folded in.
data Eocd = Eocd
  { ecCount   :: !Int      -- ^ Member count the archive claims (advisory; 'parseCentral' walks to exhaustion).
  , ecCdSize  :: !Integer  -- ^ Byte length of the central directory.
  , ecCdOff   :: !Integer  -- ^ Byte offset of the central directory from the start of the file.
  , ecComment :: !Text     -- ^ The archive comment, or empty.
  } deriving (Eq, Show)

sigEocd, sigEocd64, sigLoc64, sigCentral :: Integer
sigEocd    = 0x06054b50
sigEocd64  = 0x06064b50
sigLoc64   = 0x07064b50
sigCentral = 0x02014b50

-- Little-endian readers. Out-of-range reads yield 0 rather than throwing: the
-- input is a file we did not write, and every caller below re-checks the field
-- it cares about (a truncated record fails its signature test, not its bounds
-- test). Widths above 16 bits go through 'Integer' so a 32-bit size can never
-- come back negative on a 32-bit Int.
u8 :: BS.ByteString -> Int -> Int
u8 bs i | i >= 0 && i < BS.length bs = fromIntegral (BS.index bs i)
        | otherwise                  = 0

u16 :: BS.ByteString -> Int -> Int
u16 bs i = u8 bs i .|. (u8 bs (i + 1) `shiftL` 8)

u32 :: BS.ByteString -> Int -> Integer
u32 bs i = toInteger (u16 bs i) .|. (toInteger (u16 bs (i + 2)) `shiftL` 16)

u64 :: BS.ByteString -> Int -> Integer
u64 bs i = u32 bs i .|. (u32 bs (i + 4) `shiftL` 32)

-- | Locate the end-of-central-directory record in the tail of the file.
-- @base@ is the absolute file offset of @bs@'s first byte, so the returned
-- 'ecCdOff' is absolute even though only a window was read.
--
-- The record has no fixed position: a trailing comment of up to 64 KiB may
-- follow it, so it has to be found by scanning backwards for the signature.
-- Those four bytes can also occur inside a comment or a stored member, hence
-- two passes — first demanding that the record's declared comment length
-- account for exactly the rest of the file (which a coincidence essentially
-- never does), then, for archives whose length field is wrong, accepting the
-- last signature outright.
findEocd :: BS.ByteString -> Integer -> Either String Eocd
findEocd bs base
  | BS.length bs < 22 = Left "not a zip archive (file is too short)"
  | otherwise = case scan True start of
      Just p  -> Right (build p)
      Nothing -> case scan False start of
        Just p  -> Right (build p)
        Nothing -> Left "no zip end-of-central-directory record (truncated or not an archive?)"
  where
    start = BS.length bs - 22
    scan _     i | i < 0 = Nothing
    scan exact i
      | u32 bs i == sigEocd
      , not exact || u16 bs (i + 20) == BS.length bs - (i + 22) = Just i
      | otherwise = scan exact (i - 1)

    build p = case zip64At (p - 20) of
        -- ZIP64 supersedes the 32-bit fields wholesale; its offsets are
        -- already absolute and its directory is preceded by the two ZIP64
        -- records, so the prefix correction below must not be applied to it.
        Just (n, sz, off) -> Eocd n sz off comment
        Nothing           -> Eocd count cdSize cdOff comment
      where
        count  = u16 bs (p + 10)
        cdSize = u32 bs (p + 12)
        -- A self-extracting archive is its extractor stub followed by an
        -- otherwise ordinary ZIP, whose recorded offsets are relative to the
        -- ZIP rather than to the file. The directory always ends where the end
        -- record begins, so when the recorded offset does not agree, the
        -- difference is the stub and subtraction finds the real position.
        cdOff | eocdAbs >= cdSize && eocdAbs - cdSize /= recorded = eocdAbs - cdSize
              | otherwise                                         = recorded
        recorded = u32 bs (p + 16)
        eocdAbs  = base + toInteger p
        comment  = cleanText (BS.take (u16 bs (p + 20)) (BS.drop (p + 22) bs))

    -- The ZIP64 locator sits immediately before the end record and points at
    -- the ZIP64 end record. Both are within the tail window in every archive
    -- that is not corrupt, so a pointer outside it is treated as absent.
    zip64At q
      | q >= 0, u32 bs q == sigLoc64
      , let r = fromInteger (u64 bs (q + 8) - base)
      , r >= 0, r + 56 <= BS.length bs
      , u32 bs r == sigEocd64
      = Just (fromInteger (u64 bs (r + 32)), u64 bs (r + 40), u64 bs (r + 48))
      | otherwise = Nothing

-- | Parse the central directory block into one 'ZipEntry' per member.
--
-- The block is walked to exhaustion rather than to the end record's claimed
-- count: that count is 16-bit, so a ZIP64 archive without a locator reports
-- 65535 regardless, and a truncated directory would otherwise be a hang
-- waiting for entries that are not there.
parseCentral :: BS.ByteString -> Either String [ZipEntry]
parseCentral bs
  | BS.null bs                 = Right []      -- an archive with no members
  | u32 bs 0 /= sigCentral     = Left "the zip central directory is not where the archive says it is"
  | otherwise                  = Right (go 0)
  where
    go i
      | i + 46 > BS.length bs   = []
      | u32 bs i /= sigCentral  = []
      | otherwise               = entry : go (i + 46 + nlen + elen + clen)
      where
        flags = u16 bs (i + 8)
        nlen  = u16 bs (i + 28)
        elen  = u16 bs (i + 30)
        clen  = u16 bs (i + 32)
        attrs = u32 bs (i + 38)
        name  = decodeName (flags .&. 0x800 /= 0) (BS.take nlen (BS.drop (i + 46) bs))
        extra = BS.take elen (BS.drop (i + 46 + nlen) bs)
        (usz, csz) = zip64Sizes extra (u32 bs (i + 24)) (u32 bs (i + 20))
        entry = ZipEntry
          { zeName      = name
          , zeSize      = usz
          , zePacked    = csz
          , zeMethod    = u16 bs (i + 10)
          , zeEncrypted = flags .&. 0x1 /= 0
            -- Trailing slash is the portable marker; the MS-DOS directory
            -- attribute is a fallback for archivers that omit it.
          , zeDir       = "/" `T.isSuffixOf` name || (attrs .&. 0x10 /= 0 && usz == 0)
          , zeTime      = dosTime (u16 bs (i + 14)) (u16 bs (i + 12))
          }

-- Both 32-bit size fields are 0xFFFFFFFF sentinels when the real values live
-- in the ZIP64 extra field, where they appear in a fixed order and only when
-- their sentinel is set — so which of them is present depends on the header
-- fields, and the offsets have to be walked rather than indexed.
zip64Sizes :: BS.ByteString -> Integer -> Integer -> (Integer, Integer)
zip64Sizes extra usz csz = walk 0
  where
    walk j
      | j + 4 > BS.length extra = (usz, csz)
      | u16 extra j == 0x0001   = (pick usz dat, pick csz (dat + if usz == mask32 then 8 else 0))
      | otherwise               = walk (j + 4 + u16 extra (j + 2))
      where dat = j + 4
    pick v at | v == mask32 && at + 8 <= BS.length extra = u64 extra at
              | otherwise                               = v
    mask32 = 0xFFFFFFFF

-- The MS-DOS stamp: a packed date and time, with 1980 as year zero and
-- two-second resolution (the seconds are dropped — this is a listing).
-- A zero date means the archiver did not record one.
dosTime :: Int -> Int -> Maybe (Int, Int, Int, Int, Int)
dosTime date time
  | date == 0 || mon == 0 || day == 0 = Nothing
  | otherwise = Just (1980 + (date `shiftR` 9), mon, day, time `shiftR` 11, (time `shiftR` 5) .&. 0x3f)
  where mon = (date `shiftR` 5) .&. 0xf
        day = date .&. 0x1f

------------------------------------------------------------------------------
-- Text decoding

-- | Member names are UTF-8 only when general-purpose bit 11 says so; without
-- it the encoding is CP437, the original IBM PC character set. Guessing wrong
-- turns accented names into replacement characters, so honour the flag.
decodeName :: Bool -> BS.ByteString -> Text
decodeName utf8 bs
  | utf8      = cleanText bs
  | otherwise = T.filter (not . isControl) (T.pack (map (cp437 . fromIntegral) (BS.unpack bs)))

cleanText :: BS.ByteString -> Text
cleanText = T.filter (not . isControl) . TE.decodeUtf8With TEE.lenientDecode

-- CP437's upper half, in code-point order from 0x80. The lower half is ASCII.
cp437 :: Int -> Char
cp437 n | n < 0x80  = chr n
        | otherwise = T.index cp437High (n - 0x80)

cp437High :: Text
cp437High = T.pack
  "\199\252\233\226\228\224\229\231\234\235\232\239\238\236\196\197\
  \\201\230\198\244\246\242\251\249\255\214\220\162\163\165\8359\402\
  \\225\237\243\250\241\209\170\186\191\8976\172\189\188\161\171\187\
  \\9617\9618\9619\9474\9508\9569\9570\9558\9557\9571\9553\9559\9565\9564\9563\9488\
  \\9492\9524\9516\9500\9472\9532\9566\9567\9562\9556\9577\9574\9568\9552\9580\9575\
  \\9576\9572\9573\9561\9560\9554\9555\9579\9578\9496\9484\9608\9604\9612\9616\9600\
  \\945\223\915\960\931\963\181\964\934\920\937\948\8734\966\949\8745\
  \\8801\177\8805\8804\8992\8993\247\8776\176\8729\183\8730\8319\178\9632\160"

------------------------------------------------------------------------------
-- The listing

-- | Render the archive as a document: a summary, a column header, and an
-- indented tree of its members.
--
-- @size@ is the archive's own size on disk. The tree is built from the member
-- names rather than from directory entries, because plenty of archivers write
-- only files and leave their parents implied — so intermediate directories are
-- synthesised, and every directory reports the total of what is under it.
zipListing :: FilePath -> Integer -> [ZipEntry] -> Text -> Text
zipListing name size entries comment =
  T.unlines (summary ++ [""] ++ if null rows then ["(this archive is empty)"] else columns)
  where
    files  = [e | e <- entries, not (zeDir e)]
    nFiles = length files
    nDirs  = countDirs root
    total  = sum (map zeSize files)
    packed = sum (map zePacked files)
    nEnc   = length (filter zeEncrypted files)

    summary =
      [ T.pack (name ++ "  \x2014  " ++ humanBytes size ++ " archive")
      , T.pack (plural nFiles "file" ++ " in " ++ plural nDirs "folder"
                ++ "  \x00b7  " ++ humanBytes total ++ " uncompressed" ++ savedNote)
      ] ++
      [ T.pack (plural' nEnc "entry" "entries"
                ++ " encrypted \x2014 names and sizes are listed, contents are not readable")
      | nEnc > 0 ] ++
      [ "Comment: " <> comment | not (T.null comment) ]

    savedNote
      | total > 0 && packed < total =
          "  \x00b7  " ++ show (percentSaved total packed) ++ "% saved"
      | otherwise = ""

    -- Column widths come from the content, so a shallow archive of short names
    -- does not get a listing padded out to some worst case.
    root = foldr insertEntry emptyNode entries
    rows = kidRows "" root
    wName  = maximum (7 : map (cellWidth . rwName) rows)
    wSize  = maximum (4 : map (cellWidth . rwSize) rows)
    wSaved = maximum (5 : map (cellWidth . rwSaved) rows)
    wDate  = maximum (8 : map (cellWidth . rwDate) rows)

    columns = header : rule : map line rows
    header  = line (Row "Name" "Size" "Saved" "Modified" "")
    rule    = T.replicate (wName + wSize + wSaved + wDate + 6) "\x2500"
    line r  = T.stripEnd $ T.concat
      [ padR wName  (rwName r), "  "
      , padL wSize  (rwSize r), "  "
      , padL wSaved (rwSaved r), "  "
      , padR wDate  (rwDate r)
      , if T.null (rwNote r) then "" else "  " <> rwNote r
      ]

-- One rendered row of the tree. Kept as separate cells so the columns can be
-- padded to the widest value once all of them are known.
data Row = Row
  { rwName  :: !Text   -- ^ Tree prefix and member name.
  , rwSize  :: !Text
  , rwSaved :: !Text
  , rwDate  :: !Text
  , rwNote  :: !Text   -- ^ Encryption and unusual compression methods.
  }

-- The tree under construction: a member may or may not have an entry of its
-- own (a directory that is only implied by its children has none).
data Node = Node { ndKids :: !(M.Map Text Node), ndEnt :: !(Maybe ZipEntry) }

emptyNode :: Node
emptyNode = Node M.empty Nothing

insertEntry :: ZipEntry -> Node -> Node
insertEntry e = go (filter (not . T.null) (T.split (== '/') (zeName e)))
  where
    go []       nd = nd
    go [c]      nd = nd { ndKids = M.insertWith merge c (emptyNode { ndEnt = Just e }) (ndKids nd) }
    go (c : cs) nd = nd { ndKids = M.insertWith keepKids c (go cs emptyNode) (ndKids nd) }
    -- A leaf may already exist as an implied parent (its children were seen
    -- first) and a parent may already exist as an explicit directory entry;
    -- neither insertion may discard what the other put there.
    merge new old    = old { ndEnt = ndEnt new }
    keepKids new old = old { ndKids = M.unionWith keepKids (ndKids new) (ndKids old) }

countDirs :: Node -> Int
countDirs nd = sum [ 1 + countDirs k | k <- M.elems (ndKids nd), isDirNode k ]

isDirNode :: Node -> Bool
isDirNode nd = not (M.null (ndKids nd)) || maybe False zeDir (ndEnt nd)

-- Total uncompressed bytes and file count beneath a node.
nodeTotals :: Node -> (Integer, Int)
nodeTotals nd
  | isDirNode nd = foldr add (0, 0) (M.elems (ndKids nd))
  | otherwise    = (maybe 0 zeSize (ndEnt nd), 1)
  where add k (b, n) = let (b', n') = nodeTotals k in (b + b', n + n')

-- Rows for a node's children, deepest-first within each branch. Directories
-- sort before files and then case-insensitively, which is what makes the tree
-- readable — archive order is whatever the archiver happened to walk.
kidRows :: Text -> Node -> [Row]
kidRows prefix nd = concat (zipWith one [1 ..] kids)
  where
    kids = sortBy (comparing (\(n, k) -> (not (isDirNode k), T.toLower n, n))) (M.toList (ndKids nd))
    n    = length kids
    one i (nm, k)
      | isDirNode k = Row (prefix <> branch <> nm <> "/") (T.pack (humanBytes bytes)) ""
                          "" (T.pack (plural nf "file"))
                      : kidRows (prefix <> indent) k
      | otherwise   = [fileRow (prefix <> branch <> nm) (ndEnt k)]
      where
        (bytes, nf) = nodeTotals k
        branch = if i == n then "\x2514\x2500\x2500 " else "\x251c\x2500\x2500 "
        indent = if i == n then "    " else "\x2502   "

fileRow :: Text -> Maybe ZipEntry -> Row
fileRow nm Nothing  = Row nm "" "" "" ""
fileRow nm (Just e) = Row nm (T.pack (humanBytes (zeSize e))) saved (stamp (zeTime e)) note
  where
    saved | zeSize e > 0 && zePacked e < zeSize e =
              T.pack (show (percentSaved (zeSize e) (zePacked e)) ++ "%")
          | otherwise = ""
    -- Stored and deflated members are the overwhelming majority; naming them
    -- on every row would be noise, so only the unusual method is called out.
    note = T.intercalate ", " ([ "encrypted" | zeEncrypted e ]
                               ++ [ T.pack (methodName (zeMethod e)) | zeMethod e `notElem` [0, 8] ])

stamp :: Maybe (Int, Int, Int, Int, Int) -> Text
stamp Nothing = ""
stamp (Just (y, mo, d, h, mi)) =
  T.pack (show y ++ "-" ++ pad2 mo ++ "-" ++ pad2 d ++ " " ++ pad2 h ++ ":" ++ pad2 mi)
  where pad2 v = let s = show v in if length s < 2 then '0' : s else s

methodName :: Int -> String
methodName m = case m of
  0  -> "stored"
  1  -> "shrunk"
  6  -> "imploded"
  8  -> "deflate"
  9  -> "deflate64"
  12 -> "bzip2"
  14 -> "lzma"
  93 -> "zstd"
  95 -> "xz"
  96 -> "jpeg"
  98 -> "ppmd"
  99 -> "aes"
  _  -> "method " ++ show m

------------------------------------------------------------------------------
-- Small formatting helpers

-- Percentage of the uncompressed size that compression removed.
percentSaved :: Integer -> Integer -> Integer
percentSaved total packed = 100 - (packed * 100 `div` max 1 total)

-- Matches 'Cmedit.EditorState.humanSize' (this module is a leaf and does not
-- import the editor state).
humanBytes :: Integer -> String
humanBytes n
  | n < 1024                = show n ++ " B"
  | n < 1024 * 1024         = oneDp 1024 ++ " KB"
  | n < 1024 * 1024 * 1024  = oneDp (1024 * 1024) ++ " MB"
  | otherwise               = oneDp (1024 * 1024 * 1024) ++ " GB"
  where oneDp unit = let whole = n `div` unit
                         tenth = (n * 10 `div` unit) `mod` 10
                     in show whole ++ "." ++ show tenth

plural :: Int -> String -> String
plural n one = plural' n one (one ++ "s")

plural' :: Int -> String -> String -> String
plural' n one many = show n ++ " " ++ (if n == 1 then one else many)

-- Padding is in display cells, not characters: archive members are routinely
-- named in scripts whose glyphs are two cells wide, and 'T.justifyLeft' would
-- misalign every column below one.
cellWidth :: Text -> Int
cellWidth t = lineDisplayWidth 8 t

padR :: Int -> Text -> Text
padR w t = t <> T.replicate (max 0 (w - cellWidth t)) " "

padL :: Int -> Text -> Text
padL w t = T.replicate (max 0 (w - cellWidth t)) " " <> t
