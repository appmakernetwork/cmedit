-- | A hand-written input decoder. Raw bytes from the tty are turned into
-- 'Key' events: printable UTF-8, Ctrl/Alt combinations, arrow and editing
-- keys with xterm modifier encodings, function keys, SGR mouse reports and
-- bracketed paste. A lone ESC is disambiguated from an escape sequence with a
-- short read timeout.
module Cmedit.Input
  ( ByteSource(..)
  , mkHandleSource
  , nextKey
  ) where

import Control.Exception (SomeException, catch)
import Data.Bits ((.&.))
import Data.Char (chr)
import Data.IORef
import Data.List (isSuffixOf)
import Data.Word (Word8)
import System.IO (Handle, hWaitForInput)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Cmedit.Caps (parseOscColor)
import Cmedit.Types

-- | A byte stream with timeout-bounded reads, used by the parser to tell
-- @ESC@-the-key from @ESC@-the-sequence-prefix.
data ByteSource = ByteSource
  { srcNext        :: IO (Maybe Word8)        -- ^ Block for the next byte (Nothing on EOF).
  , srcNextTimeout :: Int -> IO (Maybe Word8) -- ^ Wait at most N ms for a byte.
  }

-- How long to wait for a byte after ESC before deciding it was a bare Esc.
escDelayMs :: Int
escDelayMs = 25

-- Per-byte timeout while consuming an OSC/DCS/APC reply payload; generous,
-- but bounded so a malformed reply can never wedge the parser.
stringSeqDelayMs :: Int
stringSeqDelayMs = 50

-- Longest OSC/DCS/APC payload we will buffer before giving up on it.
maxStringSeqBytes :: Int
maxStringSeqBytes = 8192

-- | Build a 'ByteSource' over a 'Handle' (typically stdin). Bytes are read in
-- chunks into an internal buffer for efficiency; timeouts use 'hWaitForInput'.
mkHandleSource :: Handle -> IO ByteSource
mkHandleSource h = do
  ref <- newIORef BS.empty
  let fill :: IO Bool
      fill = do
        chunk <- BS.hGetSome h 4096
        if BS.null chunk
          then pure False
          else do modifyIORef' ref (<> chunk); pure True
      next :: IO (Maybe Word8)
      next = do
        pend <- readIORef ref
        case BS.uncons pend of
          Just (b, rest) -> writeIORef ref rest >> pure (Just b)
          Nothing -> do
            ok <- fill
            if ok then next else pure Nothing
      nextTO :: Int -> IO (Maybe Word8)
      nextTO ms = do
        pend <- readIORef ref
        case BS.uncons pend of
          Just (b, rest) -> writeIORef ref rest >> pure (Just b)
          Nothing -> do
            ready <- hWaitForInput h ms `catch` \(_ :: SomeException) -> pure False
            if ready then next else pure Nothing
  pure (ByteSource next nextTO)

-- | Read and decode a single key event. Blocks until at least one byte is
-- available. Returns 'KUnknown' @[]@ to signal end-of-input.
nextKey :: ByteSource -> IO Key
nextKey src = do
  mb <- srcNext src
  case mb of
    Nothing -> pure (KUnknown [])     -- EOF sentinel
    Just w  -> decodeByte src w

decodeByte :: ByteSource -> Word8 -> IO Key
decodeByte src w
  | w == 0x1b = decodeEsc src
  | w == 0x0d || w == 0x0a = pure KEnter
  | w == 0x09 = pure KTab
  | w == 0x7f = pure KBackspace            -- DEL: plain Backspace
  -- 0x08 (BS / Ctrl-H) is left to fall through to KCtrlChar 'h', which many
  -- terminals send for Ctrl+Backspace (handled as delete-word-left).
  | w == 0x00 = pure (KCtrlChar ' ')          -- Ctrl-Space / NUL
  | w >= 0x01 && w <= 0x1a = pure (KCtrlChar (chr (fromIntegral w + 0x60)))  -- ^A..^Z
  | w >= 0x1c && w <= 0x1f = pure (KCtrlChar (chr (fromIntegral w + 0x40)))  -- ^\ ^] ^^ ^_
  | w < 0x80  = pure (KChar (chr (fromIntegral w)))
  | otherwise = KChar <$> decodeUtf8Lead src w

-- ESC has been consumed: decide between bare Esc, Alt+key, or a CSI/SS3
-- escape sequence. The OSC/DCS/APC introducers double as legacy Alt+]/P/_
-- encodings; 'parseStringSeq' disambiguates by whether payload bytes follow.
decodeEsc :: ByteSource -> IO Key
decodeEsc src = do
  mb <- srcNextTimeout src escDelayMs
  case mb of
    Nothing -> pure KEsc                  -- nothing followed -> the Esc key
    Just c
      | c == fromIntegral (fromEnum '[') -> parseCSI src
      | c == fromIntegral (fromEnum 'O') -> parseSS3 src
      | c == fromIntegral (fromEnum ']') -> parseStringSeq src ']'
      | c == fromIntegral (fromEnum 'P') -> parseStringSeq src 'P'
      | c == fromIntegral (fromEnum '_') -> parseStringSeq src '_'
      | c == 0x1b -> pure KEsc             -- ESC ESC -> treat the first as Esc
      | c == 0x7f || c == 0x08 -> pure (KAltChar '\DEL')  -- Alt+Backspace
      | c >= 0x01 && c <= 0x1a -> pure (KAltChar (chr (fromIntegral c + 0x60)))
      | c < 0x80  -> pure (KAltChar (chr (fromIntegral c)))
      | otherwise -> KAltChar <$> decodeUtf8Lead src c

-- Parse the body of a CSI sequence (ESC [ already consumed).
parseCSI :: ByteSource -> IO Key
parseCSI src = do
  bs <- readSeq src []
  case bs of
    [] -> pure (KUnknown [0x1b, fromIntegral (fromEnum '[')])
    _  ->
      let final = last bs
          body  = init bs
      in if not (null body) && head body == fromIntegral (fromEnum '<')
           then pure (parseMouse (tail body) final)
           else case interpretCSI body final of
                  CsiPasteStart -> readPaste src
                  CsiKey k      -> pure k

-- Parse an OSC (ESC ]), DCS (ESC P) or APC (ESC _) string sequence: a payload
-- terminated by ST (ESC \) or, for OSC, BEL. These only arrive as replies to
-- the driver's startup queries, so an introducer with *no* payload byte inside
-- the ESC timeout is decoded as the legacy Alt+intro key instead. The payload
-- read is byte-timeout-bounded and length-capped so a malformed reply can
-- never wedge the parser; anything unrecognised comes back as a (non-empty)
-- 'KUnknown', which the editor ignores.
parseStringSeq :: ByteSource -> Char -> IO Key
parseStringSeq src intro = do
  mb <- srcNextTimeout src escDelayMs
  case mb of
    Nothing -> pure (KAltChar intro)
    Just b0 -> go [b0]
  where
    go acc
      | length acc > maxStringSeqBytes = pure (unknown acc)
      | otherwise = do
          mb <- srcNextTimeout src stringSeqDelayMs
          case mb of
            Nothing -> pure (unknown acc)
            Just 0x07 | intro == ']' -> pure (interpret acc)   -- BEL terminator
            Just 0x1b -> do
              mb2 <- srcNextTimeout src stringSeqDelayMs
              case mb2 of
                Just 0x5c -> pure (interpret acc)              -- ESC \ (ST)
                Just b    -> go (b : 0x1b : acc)
                Nothing   -> pure (unknown acc)
            Just b -> go (b : acc)
    unknown acc = KUnknown (0x1b : fromIntegral (fromEnum intro) : reverse acc)
    interpret acc =
      let s = map (chr . fromIntegral) (reverse acc)
      in case intro of
           ']' -> case break (== ';') s of
                    -- OSC 11 colour report: "11;rgb:RRRR/GGGG/BBBB".
                    ("11", ';' : val) ->
                      maybe (unknown' s) (\(r, g, b) -> KReply (TrBgColor r g b))
                            (parseOscColor val)
                    _ -> unknown' s
           -- XTVERSION reply: DCS > | name/version ST.
           'P' -> case s of
                    ('>' : '|' : v) -> KReply (TrTermVersion v)
                    _               -> unknown' s
           -- Kitty graphics probe reply: APC G i=..;OK ST (or an error text).
           '_' -> case s of
                    ('G' : rest) -> KReply (TrKittyGfx (";OK" `isSuffixOf` (';' : rest)))
                    _            -> unknown' s
           _   -> unknown' s
      where unknown' str = KUnknown (0x1b : fromIntegral (fromEnum intro)
                                       : map (fromIntegral . fromEnum) str)

-- Parse an SS3 sequence (ESC O already consumed): function keys F1..F4 and
-- application-mode cursor keys.
parseSS3 :: ByteSource -> IO Key
parseSS3 src = do
  mb <- srcNext src
  case mb of
    Nothing -> pure KEsc
    Just c -> pure $ case chr (fromIntegral c) of
      'P' -> KFn 1 noMods
      'Q' -> KFn 2 noMods
      'R' -> KFn 3 noMods
      'S' -> KFn 4 noMods
      'A' -> KArrow DUp noMods
      'B' -> KArrow DDown noMods
      'C' -> KArrow DRight noMods
      'D' -> KArrow DLeft noMods
      'H' -> KHome noMods
      'F' -> KEnd noMods
      _   -> KUnknown [0x1b, fromIntegral (fromEnum 'O'), c]

-- Read CSI bytes up to and including the final byte (0x40..0x7e). Capped to
-- avoid blocking forever on a malformed stream.
readSeq :: ByteSource -> [Word8] -> IO [Word8]
readSeq src acc
  | length acc > 48 = pure (reverse acc)
  | otherwise = do
      mb <- srcNext src
      case mb of
        Nothing -> pure (reverse acc)
        Just b
          | b >= 0x40 && b <= 0x7e -> pure (reverse (b : acc))
          | otherwise              -> readSeq src (b : acc)

data CsiResult = CsiKey Key | CsiPasteStart

interpretCSI :: [Word8] -> Word8 -> CsiResult
interpretCSI body final =
  let s      = map (chr . fromIntegral) body
      params = parseParams s
      m       = modsFromParam (paramAt 2 1 params)
      -- A '?'-prefixed body marks a reply to one of our private queries
      -- (DECXCPR, DA1); it can never be a keypress.
      private = take 1 s == "?"
  in case chr (fromIntegral final) of
       'A' -> CsiKey (KArrow DUp m)
       'B' -> CsiKey (KArrow DDown m)
       'C' -> CsiKey (KArrow DRight m)
       'D' -> CsiKey (KArrow DLeft m)
       'H' -> CsiKey (KHome m)
       'F' -> CsiKey (KEnd m)
       'Z' -> CsiKey KBackTab
       'I' -> CsiKey (KFocus True)    -- terminal focus-in (CSI ?1004h reporting)
       'O' -> CsiKey (KFocus False)   -- terminal focus-out
       'P' -> CsiKey (KFn 1 m)
       'Q' -> CsiKey (KFn 2 m)
       -- DECXCPR reply (CSI ? row;col R) vs modified F3 (xterm CSI 1;mods R):
       -- the '?' prefix keeps the REP probe's answer out of the key stream.
       'R' | private   -> CsiKey (KReply (TrCursorPos (paramAt 1 1 params) (paramAt 2 1 params)))
           | otherwise -> CsiKey (KFn 3 m)
       -- Primary device attributes reply (CSI ? .. c).
       'c' | private   -> CsiKey (KReply (TrDA1 params))
           | otherwise -> CsiKey (KUnknown (0x1b : fromIntegral (fromEnum '[') : body ++ [final]))
       -- XTWINOPS replies: CSI 4;h;w t (text area px), CSI 6;h;w t (cell px).
       't' -> case params of
                (4 : h : w : _) -> CsiKey (KReply (TrTextPx h w))
                (6 : h : w : _) -> CsiKey (KReply (TrCellPx h w))
                _ -> CsiKey (KUnknown (0x1b : fromIntegral (fromEnum '[') : body ++ [final]))
       'S' -> CsiKey (KFn 4 m)
       -- CSI u (Kitty / fixterms): CSI code ; mods u
       'u' -> CsiKey (otherKey (paramAt 1 0 params) m)
       '~' -> case paramAt 1 0 params of
                200 -> CsiPasteStart
                -- xterm modifyOtherKeys: CSI 27 ; mods ; code ~
                27  -> CsiKey (otherKey (paramAt 3 0 params) (modsFromParam (paramAt 2 1 params)))
                n   -> CsiKey (tildeKey n m)
       _   -> CsiKey (KUnknown (0x1b : fromIntegral (fromEnum '[') : body ++ [final]))

-- A key reported by its character code + modifiers (CSI u and the xterm
-- modifyOtherKeys CSI 27;mods;code ~ form). Used both for Enter (so a modified
-- Enter is distinguishable from a plain one) and for the disambiguated keys the
-- Kitty keyboard protocol emits once we enable it, so it must map Ctrl/Alt
-- combos back to the same keys their legacy bytes would have produced.
otherKey :: Int -> Mods -> Key
otherKey code m
  | code == 13 || code == 10                  = if modified then KModEnter else KEnter
  | code == 9                                 = if hasShift m then KBackTab else KTab
  | code == 27                                = KEsc
  | code == 8 || code == 127                  = if hasAlt m then KAltChar '\DEL' else KBackspace
  -- Ctrl+Shift+letter (needs the Kitty protocol; legacy bytes can't express it):
  -- report it distinctly so workspace-wide Find/Replace (Ctrl+Shift+F/H) can be
  -- told apart from the in-file Ctrl+F/H. Handle both the shifted (upper) and
  -- unshifted (lower) key-code forms terminals may send.
  | hasCtrl m && hasShift m && code >= 0x41 && code <= 0x5a = KCtrlShiftChar (chr (code + 0x20))
  | hasCtrl m && hasShift m && code >= 0x61 && code <= 0x7a = KCtrlShiftChar (chr code)
  | hasCtrl m && code >= 0x41 && code <= 0x5a = KCtrlChar (chr (code + 0x20))   -- Ctrl+A..Z
  | hasCtrl m && code >= 0x61 && code <= 0x7a = KCtrlChar (chr code)            -- Ctrl+a..z
  -- Ctrl+punctuation (e.g. Ctrl+/ for toggle-comment): legacy bytes fold these
  -- into the C0 range, so map the disambiguated form to the same KCtrlChar.
  | hasCtrl m && code >= 0x20 && code <  0x7f = KCtrlChar (chr code)
  | hasAlt  m && code >= 0x20 && code <  0x7f = KAltChar (chr code)             -- Alt+printable
  | code >= 0x20                              = KChar (chr code)
  | otherwise                                 = KUnknown [fromIntegral (code .&. 0xff), 0x75]
  where modified = hasShift m || hasCtrl m || hasAlt m

-- The ESC [ n ~ family.
tildeKey :: Int -> Mods -> Key
tildeKey n m = case n of
  1  -> KHome m
  2  -> KInsert m
  3  -> KDelete m
  4  -> KEnd m
  5  -> KPageUp m
  6  -> KPageDown m
  7  -> KHome m
  8  -> KEnd m
  11 -> KFn 1 m
  12 -> KFn 2 m
  13 -> KFn 3 m
  14 -> KFn 4 m
  15 -> KFn 5 m
  17 -> KFn 6 m
  18 -> KFn 7 m
  19 -> KFn 8 m
  20 -> KFn 9 m
  21 -> KFn 10 m
  23 -> KFn 11 m
  24 -> KFn 12 m
  -- A non-empty payload: KUnknown [] is reserved as the EOF sentinel, so an
  -- unrecognised tilde sequence must never collapse to it (that would quit).
  _  -> KUnknown [fromIntegral (n .&. 0xff), 0x7e]

-- SGR mouse: body is "b;x;y" (with the leading '<' already stripped) and the
-- final byte is 'M' (press/motion) or 'm' (release).
parseMouse :: [Word8] -> Word8 -> Key
parseMouse body final =
  let s        = map (chr . fromIntegral) body
      params   = parseParams s
      cb       = paramAt 1 0 params
      x        = paramAt 2 1 params
      y        = paramAt 3 1 params
      pressed  = final == fromIntegral (fromEnum 'M')
      drag     = (cb .&. 0x20) /= 0
      wheel    = (cb .&. 0x40) /= 0
      base     = cb .&. 0x03
      button
        | wheel     = case base of        -- SGR 64..67: up, down, wheel-left, wheel-right
            0 -> MBWheelUp
            1 -> MBWheelDown
            2 -> MBWheelLeft
            _ -> MBWheelRight
        | otherwise = case base of
            0 -> MBLeft
            1 -> MBMiddle
            2 -> MBRight
            _ -> MBNone
      mods = Mods { modShift = (cb .&. 0x04) /= 0
                  , modAlt   = (cb .&. 0x08) /= 0
                  , modCtrl  = (cb .&. 0x10) /= 0 }
  in KMouse MouseEvent
       { meButton  = button
       , meCol     = max 0 (x - 1)
       , meRow     = max 0 (y - 1)
       , mePressed = pressed
       , meDrag    = drag
       , meMods    = mods
       , meClicks  = 1          -- the driver upgrades this to 2/3 for multi-clicks
       }

-- Read a bracketed-paste payload until the ESC [ 201 ~ terminator.
readPaste :: ByteSource -> IO Key
readPaste src = go []
  where
    terminator = reverse [0x1b, 0x5b, 0x32, 0x30, 0x31, 0x7e] -- bytes of ESC[201~, reversed
    go acc = do
      mb <- srcNext src
      case mb of
        Nothing -> pure (finish acc)
        Just b ->
          let acc' = b : acc
          in if take 6 acc' == terminator
               then pure (finish (drop 6 acc'))
               else go acc'
    finish acc = KPaste (decodeUtf8Bytes (reverse acc))

------------------------------------------------------------------------------
-- Parameter helpers

-- Split a parameter string like "1;5" into [1,5]; "?2004" tolerated.
parseParams :: String -> [Int]
parseParams s = map readInt (splitSemi (filter keep s))
  where
    keep c = c == ';' || (c >= '0' && c <= '9')
    readInt "" = 0
    readInt xs = foldl (\a c -> a * 10 + (fromEnum c - fromEnum '0')) 0 xs

splitSemi :: String -> [String]
splitSemi s = case break (== ';') s of
  (a, ';' : rest) -> a : splitSemi rest
  (a, _)          -> [a]

paramAt :: Int -> Int -> [Int] -> Int
paramAt i def ps = case drop (i - 1) ps of
  (x : _) | x /= 0 || i == 1 -> x
  _                          -> def

-- Mods are encoded as 1 + bitmask(shift=1, alt=2, ctrl=4, meta=8).
modsFromParam :: Int -> Mods
modsFromParam p
  | p <= 1    = noMods
  | otherwise =
      let b = p - 1
      in Mods { modShift = (b .&. 1) /= 0
              , modAlt   = (b .&. 2) /= 0
              , modCtrl  = (b .&. 4) /= 0 }

------------------------------------------------------------------------------
-- UTF-8 decoding

-- Decode a character whose UTF-8 lead byte @w@ has been read; pull the
-- continuation bytes from the source.
decodeUtf8Lead :: ByteSource -> Word8 -> IO Char
decodeUtf8Lead src w = do
  let need
        | w >= 0xf0 = 3
        | w >= 0xe0 = 2
        | w >= 0xc0 = 1
        | otherwise = 0
  rest <- readN src need
  pure (decodeOne (w : rest))

readN :: ByteSource -> Int -> IO [Word8]
readN _   0 = pure []
readN src n = do
  mb <- srcNext src
  case mb of
    Nothing -> pure []
    Just b  -> (b :) <$> readN src (n - 1)

decodeOne :: [Word8] -> Char
decodeOne ws =
  let t = decodeUtf8Bytes ws
  in if T.null t then '\xFFFD' else T.head t

decodeUtf8Bytes :: [Word8] -> T.Text
decodeUtf8Bytes = TE.decodeUtf8With TEE.lenientDecode . BS.pack
