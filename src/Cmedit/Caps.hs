-- | Terminal capability tracking. The driver sends a burst of queries at
-- startup ("Cmedit.Ansi"); the input parser decodes the replies to
-- 'TermReply' values; this module folds them into a 'TermCaps' record. It is
-- deliberately pure (imports "Cmedit.Types" only) so the parsing and the
-- fingerprint policy are unit-testable.
--
-- The guiding rule: every capability defaults to *off*, and a terminal that
-- never answers a query simply keeps the portable behaviour — no feature here
-- may degrade a terminal that stays silent.
module Cmedit.Caps
  ( TermCaps(..)
  , defaultCaps
  , applyReply
  , renderCapsOf
    -- * Reply payload parsing (used by the input parser)
  , parseOscColor
    -- * Policy helpers
  , isDarkRgb
  , supportsUndercurl
  , repProbeResult
  ) where

import Data.Char (isHexDigit, digitToInt, toLower)
import Data.List (isInfixOf, isPrefixOf)
import Data.Word (Word8)

import Cmedit.Types (TermReply(..), RenderCaps(..))

-- | Everything the driver has learned about the connected terminal.
data TermCaps = TermCaps
  { tcSixel     :: !Bool           -- ^ DA1 advertised sixel graphics (attribute 4).
  , tcKittyGfx  :: !Bool           -- ^ The kitty graphics probe came back OK.
  , tcUndercurl :: !Bool           -- ^ Fingerprinted as understanding SGR colon forms (4:3).
  , tcRep       :: !Bool           -- ^ REP confirmed working by the cursor-position probe.
  , tcVersion   :: !(Maybe String) -- ^ XTVERSION reply, for the fingerprint and diagnostics.
  } deriving (Eq, Show)

defaultCaps :: TermCaps
defaultCaps = TermCaps
  { tcSixel     = False
  , tcKittyGfx  = False
  , tcUndercurl = False
  , tcRep       = False
  , tcVersion   = Nothing
  }

-- | Fold one terminal reply into the capability record. Replies that carry
-- state for the editor rather than the emitter (background colour, pixel
-- sizes) are handled by the driver separately; they leave the caps untouched.
applyReply :: TermReply -> TermCaps -> TermCaps
applyReply rep caps = case rep of
  TrDA1 ps        -> caps { tcSixel = tcSixel caps || 4 `elem` ps }
  TrTermVersion v -> caps { tcVersion   = Just v
                          , tcUndercurl = tcUndercurl caps || supportsUndercurl v }
  TrKittyGfx ok   -> caps { tcKittyGfx = ok }
  TrCursorPos r c -> case repProbeResult r c of
                       Just ok -> caps { tcRep = ok }
                       Nothing -> caps
  _               -> caps

-- | The slice of 'TermCaps' the pure escape emitter needs.
renderCapsOf :: TermCaps -> RenderCaps
renderCapsOf caps = RenderCaps { rcUndercurl = tcUndercurl caps
                               , rcRep       = tcRep caps }

------------------------------------------------------------------------------
-- OSC colour replies

-- | Parse an OSC 10/11 colour payload. Terminals answer with XParseColor
-- syntax — most commonly @rgb:RRRR/GGGG/BBBB@ with 1–4 hex digits per
-- channel — and occasionally @#RRGGBB@. Each channel is scaled to 8 bits.
parseOscColor :: String -> Maybe (Word8, Word8, Word8)
parseOscColor s0
  | "rgb:" `isPrefixOf` s0 =
      case splitOn '/' (drop 4 s0) of
        [r, g, b] -> (,,) <$> chan r <*> chan g <*> chan b
        _         -> Nothing
  | ('#' : r1 : r2 : g1 : g2 : b1 : b2 : []) <- s0
  , all isHexDigit [r1, r2, g1, g2, b1, b2] =
      let byte a b = fromIntegral (digitToInt a * 16 + digitToInt b)
      in Just (byte r1 r2, byte g1 g2, byte b1 b2)
  | otherwise = Nothing
  where
    chan ds
      | n >= 1 && n <= 4 && all isHexDigit ds =
          -- Scale an n-digit channel to 8 bits: v / (16^n - 1) * 255.
          let v = foldl (\a c -> a * 16 + digitToInt c) 0 ds
              m = 16 ^ n - 1
          in Just (fromIntegral ((v * 255 + m `div` 2) `div` m))
      | otherwise = Nothing
      where n = length ds

splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
  (a, _ : rest) -> a : splitOn sep rest
  (a, [])       -> [a]

-- | Is this background colour dark? (Rec. 601 luma against the midpoint.)
isDarkRgb :: Word8 -> Word8 -> Word8 -> Bool
isDarkRgb r g b =
  (299 * fromIntegral r + 587 * fromIntegral g + 114 * fromIntegral b) < (128 * 1000 :: Int)

------------------------------------------------------------------------------
-- Fingerprints and probes

-- | Does this XTVERSION string belong to a terminal known to parse SGR colon
-- sub-parameters (curly underline)? Deliberately a whitelist: a terminal that
-- naively splits @4:3@ on the colon would read it as underline + italic, so
-- the graceful fallback for anything unrecognised is the plain SGR 4 form.
supportsUndercurl :: String -> Bool
supportsUndercurl v =
  let lv = map toLower v
  in any (`isInfixOf` lv)
       ["kitty", "wezterm", "ghostty", "foot", "iterm2", "contour", "vte", "alacritty"]

-- | Interpret a DECXCPR reply as the outcome of 'Cmedit.Ansi.repProbe': the
-- probe homes the cursor, prints two spaces and asks for two repeats, so on a
-- terminal with REP the cursor answers from column 5, and from column 3 on
-- one that ignored it. Any other position is somebody else's reply.
repProbeResult :: Int -> Int -> Maybe Bool
repProbeResult row col
  | row == 1 && col == 5 = Just True
  | row == 1 && col == 3 = Just False
  | otherwise            = Nothing
