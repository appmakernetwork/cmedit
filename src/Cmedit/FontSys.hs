-- | System font substitution: which installed font files should stand in
-- for a font a document names but does not embed (PDF's standard 14 and
-- every unembedded TrueType, SVG/CSS @font-family@ lists).
--
-- Pure by design: the driver lists the font directories ('fontDirectories'
-- says where) and hands over the file paths; this module only ranks them.
-- Decisions that carry weight:
--
-- * __Matching is by well-known file names, not by opening fonts.__ Reading
--   the @name@ table of several hundred files to answer one request would
--   cost more than rendering the page; the substitutes that matter ship
--   under stable names on every platform (URW base35 / Nimbus, Liberation,
--   DejaVu, Noto, FreeFont, and the Windows/macOS core fonts), so a table
--   of names is both cheaper and more predictable.
--
-- * __Metric compatibility first, design second, coverage last.__ For the
--   PostScript core fonts the URW clones are the same designs at the same
--   widths; Liberation is metric-compatible with Arial/Times/Courier New;
--   DejaVu, Noto and FreeFont are fallbacks that at least cover the
--   characters. The PDF renderer stretches a substitute's advances to the
--   document's widths anyway, so a metric-compatible face is what makes
--   the result look /right/, not merely legible.
--
-- * __Any family is also tried literally__ ("Noto Serif Display" bold →
--   @NotoSerifDisplay-Bold.ttf@) by normalising both sides to lower-case
--   alphanumerics, so families outside the tables still resolve when they
--   are installed.
--
-- * __The design is honoured before the style.__ Candidates are ranked:
--   the faithful substitutes in the exact style, then the faithful
--   substitutes in the other styles (nearest first), and only then the
--   generic fallbacks. So Helvetica-Bold prefers Liberation Sans Bold to
--   NimbusSans-Regular (both are Helvetica-shaped, one has the weight),
--   but on a machine with only NimbusSans-Regular it still gets that
--   rather than DejaVu Sans Bold: the caller can embolden a regular face,
--   it cannot redraw the wrong one.
module Cmedit.FontSys
  ( FontRequest(..)
  , fontRequest
  , OS(..)
  , substituteFor
  , fontDirectories
  , fontEnvVars
  , parseFamilyName
  , FontClass(..)
  , classifyFamily
  ) where

import Data.Char (isAlphaNum, toLower, isUpper)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, nub, sortOn, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | What a document asks for. The flags come from wherever the caller has
-- them (a PDF @/FontDescriptor@'s @/Flags@, CSS @font-weight@/@font-style@)
-- and are OR-ed with what the family name itself says ("-BoldOblique").
data FontRequest = FontRequest
  { frFamily   :: !Text   -- ^ As written: "Helvetica-BoldOblique", "ArialMT", "sans-serif", "ABCDEF+Calibri".
  , frBold     :: !Bool
  , frItalic   :: !Bool
  , frMono     :: !Bool   -- ^ Fixed pitch (PDF flag 1).
  , frSerif    :: !Bool   -- ^ Serif (PDF flag 2).
  , frSymbolic :: !Bool   -- ^ Symbolic (PDF flag 3): a non-Latin glyph set.
  } deriving (Eq, Show)

-- | A request with no flags set.
fontRequest :: Text -> FontRequest
fontRequest fam = FontRequest fam False False False False False

data OS = OSLinux | OSMac | OSWindows deriving (Eq, Show)

-- | Where fonts live, for the environment (the driver passes the variables it
-- read; this module stays pure) and a home directory. The driver lists these
-- recursively; a missing directory is simply empty. Windows is located through
-- @%WINDIR%@ (or @%SystemRoot%@; Windows need not live on @C:@) and the
-- per-user fonts through @%LOCALAPPDATA%@ (redirectable, so not assumed to be
-- under the profile), falling back to the defaults when unset.
fontDirectories :: OS -> [(String, String)] -> FilePath -> [FilePath]
fontDirectories os env home = case os of
  OSLinux -> [ "/usr/share/fonts", "/usr/local/share/fonts", home ++ "/.local/share/fonts"
             , home ++ "/.fonts", "/usr/share/X11/fonts/Type1", "/usr/X11R6/lib/X11/fonts" ]
  OSMac -> [ "/System/Library/Fonts", "/System/Library/Fonts/Supplemental", "/Library/Fonts"
           , home ++ "/Library/Fonts" ]
  OSWindows -> [ winDir ++ "\\Fonts", localData ++ "\\Microsoft\\Windows\\Fonts" ]
  where
    var k = case lookup k env of
      Just v | not (null v) -> Just (trimSep v)
      _ -> Nothing
    trimSep = reverse . dropWhile (`elem` ("\\/" :: String)) . reverse
    winDir = fromMaybe "C:\\Windows" (maybe (var "SystemRoot") Just (var "WINDIR"))
    localData = fromMaybe (home ++ "\\AppData\\Local") (var "LOCALAPPDATA")

-- | The environment variables 'fontDirectories' reads.
fontEnvVars :: [String]
fontEnvVars = ["WINDIR", "SystemRoot", "LOCALAPPDATA"]

------------------------------------------------------------------------------
-- Families

-- | What a family name is a request /for/, once the vendor spellings are
-- folded together.
data FontClass
  = ClsHelvetica | ClsHelveticaNarrow | ClsArial | ClsTimes | ClsTimesNewRoman
  | ClsCourier | ClsCourierNew | ClsSymbol | ClsDingbats
  | ClsBookman | ClsAvantGarde | ClsPalatino | ClsNewCentury | ClsZapfChancery
  | ClsCalibri | ClsCambria | ClsGeorgia | ClsVerdana | ClsTahoma | ClsSegoe
  | ClsGaramond | ClsConsolas
  | ClsSans | ClsSerif | ClsMono | ClsCursive | ClsFantasy
  | ClsOther
  deriving (Eq, Ord, Show)

-- | Split a family name as written into its base family and the style the
-- name itself carries: @"ABCDEF+TimesNewRomanPS-BoldItalicMT"@ →
-- @("timesnewroman", bold, italic)@. The base is normalised (lower-case,
-- alphanumerics only, the PostScript @MT@/@PS@/@PSMT@ vendor suffixes and
-- a subset tag removed).
parseFamilyName :: Text -> (String, Bool, Bool)
parseFamilyName t0 = (base, bold, italic)
  where
    raw = T.unpack (dropSubset (T.strip t0))
    dropSubset t = case T.breakOn "+" t of
      (pre, rest) | T.length pre == 6, T.all isUpper pre, not (T.null rest) -> T.drop 1 rest
      _ -> t
    low = map toLower raw
    -- The family part ends at the first '-' or ',' (PostScript and PDF
    -- style separators); spaces join words of a CSS name.
    (famPart, stylePart) = break (`elem` ['-', ',']) low
    norm = filter isAlphaNum
    styleWords = norm stylePart
    famN0 = norm famPart
    -- Style may also be glued on ("ArialBold", "Arial Bold", "TimesItalic").
    (famN1, glued) = stripStyleSuffix famN0
    famN = stripVendor famN1
    allStyle = styleWords ++ glued
    bold = any (`isInfixOf` allStyle) ["bold", "black", "heavy", "semibold", "demibold", "demi"]
           || any (`isSuffixOf` allStyle) ["bd", "bdit", "bdita"]
    italic = any (`isInfixOf` allStyle) ["italic", "oblique", "ital", "slanted", "kursiv"]
             || any (`isSuffixOf` allStyle) ["it", "bdit", "bdita"]
    base = famN

stripVendor :: String -> String
stripVendor s = fromMaybe s (firstJust [ stripSuffix v s | v <- ["psmt", "mt", "ps"], length s > length v + 3 ])
  where stripSuffix suf x = if suf `isSuffixOf` x then Just (take (length x - length suf) x) else Nothing

stripStyleSuffix :: String -> (String, String)
stripStyleSuffix s = go s ""
  where
    -- Only words no family name ends in: "Nimbus Roman" and "URW Gothic
    -- Book" must keep their last word.
    sufs = ["bolditalic", "boldoblique", "bold", "italic", "oblique", "regular", "demi", "black"]
    go x acc = case firstJust [ fmap (\b -> (b, suf)) (stripSuf suf x) | suf <- sufs, length x > length suf + 2 ] of
      Just (b, suf) -> go b (suf ++ acc)
      Nothing -> (x, acc)
    stripSuf suf x = if suf `isSuffixOf` x then Just (take (length x - length suf) x) else Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust (Just x : _) = Just x
firstJust (_ : xs) = firstJust xs
firstJust [] = Nothing

-- | The class a normalised family belongs to.
classifyFamily :: String -> FontClass
classifyFamily f
  | f `elem` ["helveticanarrow", "arialnarrow", "nimbussansnarrow", "liberationsansnarrow"] = ClsHelveticaNarrow
  | "helvetica" `isPrefixOf` f || f `elem` ["nimbussans", "nimbussansl", "texgyreheros", "freesans", "n019003l"] = ClsHelvetica
  | f `elem` ["arial", "liberationsans", "arimo", "arialunicode"] = ClsArial
  | f `elem` ["times", "timesroman", "nimbusroman", "nimbusromanno9l", "texgyretermes", "freeserif"] = ClsTimes
  | "timesnewroman" `isPrefixOf` f || f `elem` ["liberationserif", "tinos", "tnr"] = ClsTimesNewRoman
  | f `elem` ["courier", "nimbusmonops", "nimbusmono", "nimbusmonol", "texgyrecursor", "freemono"] = ClsCourier
  | "couriernew" `isPrefixOf` f || f `elem` ["liberationmono", "cousine"] = ClsCourierNew
  | f `elem` ["symbol", "standardsymbolsps", "standardsyml", "symbolmt"] = ClsSymbol
  | "zapfdingbats" `isPrefixOf` f || f `elem` ["dingbats", "itczapfdingbats", "d050000l"] = ClsDingbats
  | "bookman" `isPrefixOf` f || "itcbookman" `isPrefixOf` f || f == "urwbookman" || f == "urwbookmanl" = ClsBookman
  | "avantgarde" `isPrefixOf` f || "itcavantgarde" `isPrefixOf` f || f == "urwgothic" || f == "urwgothicl" = ClsAvantGarde
  | "palatino" `isPrefixOf` f || f `elem` ["p052", "urwpalladio", "urwpalladiol", "bookantiqua", "texgyrepagella"] = ClsPalatino
  | "newcentury" `isPrefixOf` f || "centuryschoolbook" `isPrefixOf` f || f `elem` ["c059", "centuryschl", "texgyreschola"] = ClsNewCentury
  | "zapfchancery" `isPrefixOf` f || "itczapfchancery" `isPrefixOf` f || f `elem` ["z003", "urwchanceryl"] = ClsZapfChancery
  | f `elem` ["calibri", "carlito"] = ClsCalibri
  | f `elem` ["cambria", "caladea", "cambriamath"] = ClsCambria
  | f `elem` ["georgia", "gelasio"] = ClsGeorgia
  | f `elem` ["verdana"] = ClsVerdana
  | f `elem` ["tahoma"] = ClsTahoma
  | "segoeui" `isPrefixOf` f = ClsSegoe
  | "garamond" `isPrefixOf` f || "ebgaramond" `isPrefixOf` f || "adobegaramond" `isPrefixOf` f = ClsGaramond
  | f `elem` ["consolas", "inconsolata", "menlo", "monaco", "lucidaconsole"] = ClsConsolas
  | f `elem` ["sansserif", "sans", "systemui", "uisansserif", "helveticaneue", "lucidagrande", "lucidasans"] = ClsSans
  | f `elem` ["serif", "uiserif"] = ClsSerif
  | f `elem` ["monospace", "mono", "uimonospace"] = ClsMono
  | f `elem` ["cursive", "comicsansms"] = ClsCursive
  | f `elem` ["fantasy", "impact"] = ClsFantasy
  | otherwise = ClsOther

------------------------------------------------------------------------------
-- Candidate tables

-- | Style index: 0 regular, 1 bold, 2 italic, 3 bold italic.
type Style = Int

-- | A file family: the normalised file-name stems of its four styles, in
-- 'Style' order. Several stems per style cover the vendor spellings.
data Fam = Fam [String] [String] [String] [String]

stems :: Style -> Fam -> [String]
stems s (Fam r b i bi) = case s of
  0 -> r; 1 -> b; 2 -> i; _ -> bi

-- Four standard style suffixes on a stem, the common naming scheme.
std :: String -> Fam
std n = Fam [n ++ "regular", n, n ++ "roman", n ++ "book"] [n ++ "bold"]
            [n ++ "italic", n ++ "oblique"] [n ++ "bolditalic", n ++ "boldoblique"]

-- The URW base35 names, new (2017+) and old (gsfonts) spellings.
nimbusSans, nimbusSansNarrow, nimbusRoman, nimbusMono, urwBookman, urwGothic, p052, c059, z003 :: Fam
nimbusSans = Fam ["nimbussansregular", "nimbussanslregu", "n019003l"] ["nimbussansbold", "nimbussanslbold", "n019004l"]
                 ["nimbussansitalic", "nimbussansoblique", "nimbussanslreguital", "n019023l"]
                 ["nimbussansbolditalic", "nimbussansboldoblique", "nimbussanslboldital", "n019024l"]
nimbusSansNarrow = Fam ["nimbussansnarrowregular", "n019043l"] ["nimbussansnarrowbold", "n019044l"]
                       ["nimbussansnarrowoblique", "nimbussansnarrowitalic", "n019063l"]
                       ["nimbussansnarrowboldoblique", "nimbussansnarrowbolditalic", "n019064l"]
nimbusRoman = Fam ["nimbusromanregular", "nimbusromno9lregu", "n021003l"] ["nimbusromanbold", "nimbusromno9lmedi", "n021004l"]
                  ["nimbusromanitalic", "nimbusromno9lreguital", "n021023l"]
                  ["nimbusromanbolditalic", "nimbusromno9lmediital", "n021024l"]
nimbusMono = Fam ["nimbusmonopsregular", "nimbusmonolregu", "n022003l"] ["nimbusmonopsbold", "nimbusmonolbold", "n022004l"]
                 ["nimbusmonopsitalic", "nimbusmonolreguobli", "n022023l"]
                 ["nimbusmonopsbolditalic", "nimbusmonolboldobli", "n022024l"]
urwBookman = Fam ["urwbookmanlight", "urwbookmanllight", "b018012l"] ["urwbookmandemi", "urwbookmanldemibold", "b018015l"]
                 ["urwbookmanlightitalic", "urwbookmanllightital", "b018032l"]
                 ["urwbookmandemiitalic", "urwbookmanldemiboldital", "b018035l"]
urwGothic = Fam ["urwgothicbook", "urwgothiclbook", "a010013l"] ["urwgothicdemi", "urwgothicldemi", "a010015l"]
                ["urwgothicbookoblique", "urwgothiclbookobli", "a010033l"]
                ["urwgothicdemioblique", "urwgothicldemiobli", "a010035l"]
p052 = Fam ["p052roman", "p052regular", "urwpalladiolroma", "p052003l"] ["p052bold", "urwpalladiolbold", "p052004l"]
           ["p052italic", "urwpalladiolital", "p052023l"] ["p052bolditalic", "urwpalladiolboldital", "p052024l"]
c059 = Fam ["c059roman", "c059regular", "centuryschlroma", "c059013l"] ["c059bold", "centuryschlbold", "c059016l"]
           ["c059italic", "centuryschlital", "c059033l"] ["c059bdita", "c059bolditalic", "centuryschlboldital", "c059036l"]
z003 = Fam ["z003mediumitalic", "urwchancerylmediital", "z003034l"] [] [] []

symbolFam, dingbatsFam :: Fam
symbolFam = Fam ["standardsymbolsps", "standardsymlregu", "s050000l", "symbol", "symbolmt"] [] [] []
dingbatsFam = Fam ["d050000l", "dingbats", "zapfdingbats", "zapfdingbatsitc"] [] [] []

liberationSans, liberationSansNarrow, liberationSerif, liberationMono :: Fam
liberationSans = std "liberationsans"
liberationSansNarrow = std "liberationsansnarrow"
liberationSerif = std "liberationserif"
liberationMono = std "liberationmono"

-- Windows core fonts (8.3-style names) and their macOS spellings.
arialFam, timesNRFam, courierNewFam, georgiaFam, verdanaFam, tahomaFam, calibriFam, cambriaFam, consolasFam, segoeFam :: Fam
arialFam = Fam ["arial", "arialmt", "arialregular"] ["arialbd", "arialbold", "arialboldmt"]
               ["ariali", "arialitalic", "arialitalicmt"] ["arialbi", "arialbolditalic", "arialbolditalicmt"]
timesNRFam = Fam ["times", "timesnewroman", "timesnewromanpsmt", "timesnewromanregular"]
                 ["timesbd", "timesnewromanbold", "timesnewromanpsboldmt"]
                 ["timesi", "timesnewromanitalic", "timesnewromanpsitalicmt"]
                 ["timesbi", "timesnewromanbolditalic", "timesnewromanpsbolditalicmt"]
courierNewFam = Fam ["cour", "couriernew", "couriernewpsmt"] ["courbd", "couriernewbold"]
                    ["couri", "couriernewitalic"] ["courbi", "couriernewbolditalic"]
georgiaFam = Fam ["georgia"] ["georgiab", "georgiabold"] ["georgiai", "georgiaitalic"] ["georgiaz", "georgiabolditalic"]
verdanaFam = Fam ["verdana"] ["verdanab", "verdanabold"] ["verdanai", "verdanaitalic"] ["verdanaz", "verdanabolditalic"]
tahomaFam = Fam ["tahoma"] ["tahomabd", "tahomabold"] ["tahoma"] ["tahomabd"]
calibriFam = Fam ["calibri", "carlitoregular", "carlito"] ["calibrib", "carlitobold"] ["calibrii", "carlitoitalic"] ["calibriz", "carlitobolditalic"]
cambriaFam = Fam ["cambria", "caladearegular", "caladea"] ["cambriab", "caladeabold"] ["cambriai", "caladeaitalic"] ["cambriaz", "caladeabolditalic"]
consolasFam = Fam ["consola", "consolas", "inconsolataregular", "inconsolata", "menlo", "monaco"]
                  ["consolab", "consolasbold", "inconsolatabold", "menlobold"]
                  ["consolai", "consolasitalic", "menloitalic"] ["consolaz", "consolasbolditalic", "menlobolditalic"]
segoeFam = Fam ["segoeui"] ["segoeuib", "segoeuibold"] ["segoeuii", "segoeuiitalic"] ["segoeuiz", "segoeuibolditalic"]

-- macOS collections carry every style in one file; the caller picks the
-- face by 'Cmedit.Font.fontStyle'.
macHelvetica, macTimes, macCourier :: Fam
-- A @#ext@ suffix restricts a stem to one extension, which is what tells
-- macOS's Times.ttc apart from Windows's times.ttf.
macHelvetica = let s = ["helvetica#.ttc", "helveticaneue#.ttc"] in Fam s s s s
macTimes = let s = ["times#.ttc", "timesroman#.ttc"] in Fam s s s s
macCourier = let s = ["courier#.ttc"] in Fam s s s s

dejavuSans, dejavuSerif, dejavuMono, notoSans, notoSerif, notoMono, freeSans, freeSerif, freeMono :: Fam
dejavuSans = Fam ["dejavusans"] ["dejavusansbold"] ["dejavusansoblique"] ["dejavusansboldoblique"]
dejavuSerif = Fam ["dejavuserif"] ["dejavuserifbold"] ["dejavuserifitalic"] ["dejavuserifbolditalic"]
dejavuMono = Fam ["dejavusansmono"] ["dejavusansmonobold"] ["dejavusansmonooblique"] ["dejavusansmonoboldoblique"]
notoSans = std "notosans"
notoSerif = std "notoserif"
notoMono = Fam ["notosansmonoregular", "notomonoregular", "notosansmono"] ["notosansmonobold"] [] []
freeSans = Fam ["freesans"] ["freesansbold"] ["freesansoblique"] ["freesansboldoblique"]
freeSerif = Fam ["freeserif"] ["freeserifbold"] ["freeserifitalic"] ["freeserifbolditalic"]
freeMono = Fam ["freemono"] ["freemonobold"] ["freemonooblique"] ["freemonoboldoblique"]

sansFallback, serifFallback, monoFallback :: [Fam]
sansFallback = [dejavuSans, notoSans, freeSans, liberationSans, nimbusSans, arialFam]
serifFallback = [dejavuSerif, notoSerif, freeSerif, liberationSerif, nimbusRoman, timesNRFam]
monoFallback = [dejavuMono, notoMono, freeMono, liberationMono, nimbusMono, courierNewFam]

-- | Ranked file families for a class: the faithful substitutes, and the
-- generic fallbacks for its kind.
familiesFor :: FontClass -> Bool -> Bool -> ([Fam], [Fam])
familiesFor cls mono serif = splitFallback $ case cls of
  ClsHelvetica -> [nimbusSans, macHelvetica, liberationSans, arialFam] ++ sansFallback
  ClsHelveticaNarrow -> [nimbusSansNarrow, liberationSansNarrow, nimbusSans, liberationSans] ++ sansFallback
  ClsArial -> [liberationSans, arialFam, nimbusSans, macHelvetica] ++ sansFallback
  ClsTimes -> [nimbusRoman, macTimes, liberationSerif, timesNRFam] ++ serifFallback
  ClsTimesNewRoman -> [liberationSerif, timesNRFam, nimbusRoman, macTimes] ++ serifFallback
  ClsCourier -> [nimbusMono, macCourier, liberationMono, courierNewFam] ++ monoFallback
  ClsCourierNew -> [liberationMono, courierNewFam, nimbusMono, macCourier] ++ monoFallback
  ClsSymbol -> [symbolFam] ++ sansFallback
  ClsDingbats -> [dingbatsFam] ++ sansFallback
  ClsBookman -> [urwBookman] ++ serifFallback
  ClsAvantGarde -> [urwGothic] ++ sansFallback
  ClsPalatino -> [p052] ++ serifFallback
  ClsNewCentury -> [c059] ++ serifFallback
  ClsZapfChancery -> [z003] ++ serifFallback
  ClsCalibri -> [calibriFam] ++ sansFallback
  ClsCambria -> [cambriaFam] ++ serifFallback
  ClsGeorgia -> [georgiaFam] ++ serifFallback
  ClsVerdana -> [verdanaFam, dejavuSans] ++ sansFallback
  ClsTahoma -> [tahomaFam, dejavuSans] ++ sansFallback
  ClsSegoe -> [segoeFam] ++ sansFallback
  ClsGaramond -> [std "ebgaramond", std "garamond", p052] ++ serifFallback
  ClsConsolas -> [consolasFam] ++ monoFallback
  ClsSans -> sansFallback
  ClsSerif -> serifFallback
  ClsMono -> monoFallback
  ClsCursive -> [z003] ++ serifFallback
  ClsFantasy -> sansFallback
  ClsOther
    | mono -> monoFallback
    | serif -> serifFallback
    | otherwise -> sansFallback
  where
    -- Everything before the generic list is a faithful substitute.
    splitFallback fs = let k = length fs - genericLen in (take k fs, drop k fs)
    genericLen = length sansFallback   -- all three generic lists are this long

------------------------------------------------------------------------------
-- Ranking

fontFileExts :: [String]
fontFileExts = [".otf", ".ttf", ".ttc", ".otc", ".t1", ".pfb", ".pfa"]

-- | Ranked candidate files for a request (best first, no duplicates). Empty
-- only when the list holds no usable font file at all.
substituteFor :: [FilePath] -> FontRequest -> [FilePath]
substituteFor files req = nub (literal ++ tabled ++ anyFont)
  where
    (famN, nameBold, nameItalic) = parseFamilyName (frFamily req)
    bold = frBold req || nameBold
    italic = frItalic req || nameItalic
    want = (if bold then 1 else 0) + (if italic then 2 else 0) :: Style
    -- Style preference: exact, then the nearest (drop italic, drop bold,
    -- regular), then whatever is left.
    styleOrder = nub [want, want `mod` 2, (want `div` 2) * 2, 0, 1, 2, 3]
    cls = classifyFamily famN
    index :: M.Map String [FilePath]
    index = M.fromListWith (flip (++))
              [ (stemOf f, [f]) | f <- files, extRank f < length fontFileExts ]
    lookupStem s = case break (== '#') s of
      (st, '#' : ext) -> [ f | f <- lookupStem st, map toLower (reverse (take (length ext) (reverse f))) == ext ]
      _ -> sortOn extRank (M.findWithDefault [] s index)
    -- The family as written, with each style spelling.
    literalStems st = case st of
      0 -> [famN ++ "regular", famN, famN ++ "roman", famN ++ "book", famN ++ "medium"]
      1 -> [famN ++ "bold", famN ++ "bd", famN ++ "b"]
      2 -> [famN ++ "italic", famN ++ "oblique", famN ++ "it", famN ++ "i"]
      _ -> [famN ++ "bolditalic", famN ++ "boldoblique", famN ++ "bi", famN ++ "z"]
    literal | length famN < 3 = []
            | otherwise = concat [ concatMap lookupStem (literalStems st) | st <- styleOrder ]
    (faithful, generic) = familiesFor cls (frMono req) (frSerif req)
    -- Exact style across the faithful substitutes, then their other styles,
    -- and only then the generic fallbacks (exact style first again): the
    -- right design in the wrong weight beats the wrong design.
    tabled = concat [ concatMap lookupStem (stems st fam) | st <- styleOrder, fam <- faithful ]
          ++ concat [ concatMap lookupStem (stems st fam) | st <- styleOrder, fam <- generic ]
    -- Last resort: any font file at all, so a caller always has something.
    anyFont = sortOn extRank [ f | f <- files, extRank f < length fontFileExts ]

-- | Lower-case alphanumerics of the file name without its extension.
stemOf :: FilePath -> String
stemOf f = filter isAlphaNum (map toLower (dropExt (baseName f)))

baseName :: FilePath -> FilePath
baseName = reverse . takeWhile (\c -> c /= '/' && c /= '\\') . reverse

dropExt :: FilePath -> FilePath
dropExt n = case break (== '.') (reverse n) of
  (_, '.' : rest) | not (null rest) -> reverse rest
  _ -> n

-- | Preference among the same design's file formats: OpenType/TrueType
-- (which carry a Unicode cmap) before Type 1. Unknown extensions sort past
-- the end, which is how non-font files are excluded.
extRank :: FilePath -> Int
extRank f = let e = map toLower (extOf (baseName f))
            in fromMaybe (length fontFileExts) (lookup e (zip fontFileExts [0 ..]))
  where extOf n = case break (== '.') (reverse n) of
          (e, '.' : _) -> '.' : reverse e
          _ -> ""

