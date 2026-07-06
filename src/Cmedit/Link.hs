-- | OSC 8 hyperlink targets: recognising URLs in document text and turning
-- file paths into @file://@ URIs. Pure and dependency-free (imports only
-- boot libraries) so both the renderer and the tests can use it; the escape
-- emission itself lives in "Cmedit.Ansi" / "Cmedit.Render".
module Cmedit.Link
  ( urlSpans
  , filePathUri
  , escapeUri
  , linkIdOf
  ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Char (isAlphaNum, isAscii, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

-- | The @http(s)://@ URL spans of one line as @(startChar, endChar, uri)@,
-- end exclusive, in character (not display-cell) indices. The scan is a
-- single left-to-right pass, so it stays cheap enough to run per visible
-- line per frame. A URL runs until whitespace/control/quote characters and
-- then drops trailing punctuation that almost always belongs to the prose
-- (@.,;:!?@ and closers), so "see https://x.example/y." links without the
-- final dot. The returned uri has non-ASCII characters percent-encoded so it
-- is safe to embed in an OSC string byte-for-byte.
urlSpans :: Text -> [(Int, Int, Text)]
urlSpans line = go 0 (T.unpack line)
  where
    go _ [] = []
    go i cs@(_ : rest)
      | Just schemeLen <- prefixLen cs =
          let body = takeWhile urlChar (drop schemeLen cs)
              full = schemeLen + length body
              trimmed = full - trailing (take full cs)
          in if trimmed > schemeLen   -- require a non-empty rest after ://
               then (i, i + trimmed, escapeUri (T.pack (take trimmed cs)))
                      : go (i + trimmed) (drop trimmed cs)
               else go (i + 1) rest
      | otherwise = go (i + 1) rest
    prefixLen cs
      | isPrefix "https://" cs = Just 8
      | isPrefix "http://" cs = Just 7
      | otherwise = Nothing
    isPrefix p cs = take (length p) cs == p
    urlChar c = c > ' ' && c /= '\DEL' && c `notElem` ("\"'<>`" :: String)
    -- How many characters to drop from the end: trailing punctuation, plus
    -- an unbalanced closing bracket ("(https://a/b)" keeps b, drops ')').
    trailing s =
      let dropPunct r@(c : cs')
            | c `elem` (".,;:!?" :: String) = dropPunct cs'
            | c `elem` (")]}" :: String)
            , not (balanced (matchOf c) c s) = dropPunct cs'
            | otherwise = r
          dropPunct [] = []
      in length s - length (dropPunct (reverse s))
    matchOf ')' = '('
    matchOf ']' = '['
    matchOf _   = '{'
    balanced o c s = length (filter (== o) s) >= length (filter (== c) s)

-- | A @file://@ URI for an absolute path (percent-encoded), or 'Nothing' for
-- relative paths and pseudo-paths like @cmedit://Manual.md@ — a link target
-- must be resolvable by the terminal, which has no idea what our working
-- directory or internal schemes are. Windows drive paths become
-- @file:///C:/dir/file@ per RFC 8089.
filePathUri :: FilePath -> Maybe Text
filePathUri p
  | take 1 p == "/" = Just (T.pack "file://" <> escapeUri (T.pack p))
  | isDrivePath p =
      let fwd = map (\c -> if c == '\\' then '/' else c) p
      in Just (T.pack "file:///" <> escapeUri (T.pack fwd))
  | otherwise = Nothing
  where
    isDrivePath (d : ':' : s : _) = isAsciiLetter d && (s == '\\' || s == '/')
    isDrivePath _ = False
    isAsciiLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | Percent-encode a URI (or URI path): unreserved characters and the
-- delimiters that commonly appear verbatim in URLs pass through, everything
-- else — including all non-ASCII — becomes @%XX@ per UTF-8 byte. Applying it
-- to an already-typed URL is safe for fetching because the characters it
-- escapes are ones a browser would have escaped on the wire anyway.
escapeUri :: Text -> Text
escapeUri = T.concatMap enc
  where
    enc c
      | isAscii c && (isAlphaNum c || c `elem` keep) = T.singleton c
      | otherwise = T.pack (concatMap pct (utf8Bytes (ord c)))
    keep = "-._~/:?#[]@!$&'()*+,;=%" :: String
    pct b = '%' : hexDigit (b `div` 16) : [hexDigit (b `mod` 16)]
    hexDigit n = if n < 10 then toEnum (ord '0' + n) else toEnum (ord 'A' + n - 10)
    utf8Bytes cp
      | cp < 0x80 = [cp]
      | cp < 0x800 = [0xC0 + cp `div` 64, 0x80 + cp `mod` 64]
      | cp < 0x10000 = [ 0xE0 + cp `div` 4096
                       , 0x80 + (cp `div` 64) `mod` 64
                       , 0x80 + cp `mod` 64 ]
      | otherwise = [ 0xF0 + cp `div` 262144
                    , 0x80 + (cp `div` 4096) `mod` 64
                    , 0x80 + (cp `div` 64) `mod` 64
                    , 0x80 + cp `mod` 64 ]

-- | A short stable id for OSC 8's @id=@ parameter (FNV-1a over the URI), so
-- a link that spans several cells, rows or frames is hovered/underlined as
-- one unit by terminals that group on it.
linkIdOf :: Text -> String
linkIdOf t = toHex (T.foldl' step fnvBasis t)
  where
    fnvBasis = 14695981039346656037 :: Word64
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211
    toHex 0 = "0"
    toHex n = go n []
      where
        go 0 acc = acc
        go m acc = go (m `shiftR` 4) (digit (fromIntegral (m .&. 15)) : acc)
        digit d = if d < (10 :: Int) then toEnum (ord '0' + d)
                                     else toEnum (ord 'a' + d - 10)
