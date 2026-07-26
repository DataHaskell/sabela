{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Grammar.Synth (
    Surface (..),
    cardCounts,
    exclusivityViolations,
    sanitizeTypeText,
    synthesizeGrammar,
    synthesizeGrammarBounded,
    synthesizeGrammarProven,
    usedNames,
) where

import Data.Char (isAlphaNum, isDigit, isLower)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar (
    BrowseEntry (..),
    Envelope (..),
    ImportStyle (..),
    displayEnvelope,
    normalizeName,
    parseBrowse,
 )

data Surface = Surface
    { surfModule :: Text
    , surfStyle :: ImportStyle
    , surfBrowse :: Text
    }
    deriving (Eq, Show)

synthesizeGrammar :: [Surface] -> Text
synthesizeGrammar = synthesizeGrammarProven []

synthesizeGrammarProven :: [Text] -> [Surface] -> Text
synthesizeGrammarProven proven surfaces =
    renderSections [(sec, length (secTerminals sec)) | sec <- secs]
  where
    secs = map (renderSection proven) surfaces

synthesizeGrammarBounded :: Int -> [Text] -> [Surface] -> Text
synthesizeGrammarBounded budget proven surfaces = shrink initialKept
  where
    secs = map (renderSection proven) surfaces
    initialKept = [(sec, length (secTerminals sec)) | sec <- secs]
    shrink kept
        | T.length (renderSections kept) <= budget = renderSections kept
        | otherwise = case dropLastTerminal kept of
            Just kept' -> shrink kept'
            Nothing -> renderSections kept

data Section = Section
    { secModule :: Text
    , secHeading :: [Text]
    , secTerminals :: [Text]
    }

renderSection :: [Text] -> Surface -> Section
renderSection proven (Surface m style browse) =
    Section
        m
        ["", "### " <> m <> "  (" <> importHint style <> ")"]
        (map (renderTerminal style) (filter keep (parseBrowse browse)))
  where
    keep e = provenEntry e || not (isNoise e)
    provenEntry (BrowseEntry name _) = lastSeg name `elem` proven

renderSections :: [(Section, Int)] -> Text
renderSections kept =
    T.unlines (grammarHeader : concatMap renderOne kept)
  where
    renderOne (sec, n) =
        secHeading sec
            ++ take n (secTerminals sec)
            ++ omittedLine sec n

omittedLine :: Section -> Int -> [Text]
omittedLine sec n
    | omitted <= 0 = []
    | otherwise =
        [ "  … "
            <> tShow omitted
            <> " of "
            <> tShow total
            <> " exports omitted — narrow with discover module="
            <> secModule sec
        ]
  where
    total = length (secTerminals sec)
    omitted = total - n

dropLastTerminal :: [(Section, Int)] -> Maybe [(Section, Int)]
dropLastTerminal kept = case break ((> 0) . snd) (reverse kept) of
    (post, (sec, n) : pre) ->
        Just (reverse pre ++ (sec, n - 1) : reverse post)
    _ -> Nothing

grammarHeader :: Text
grammarHeader =
    "## Live API grammar (synthesised from :browse — verified names, not exhaustive; the search tools cover the rest)"

cardCounts :: Text -> (Int, Int, Int)
cardCounts card = (shown, omitted, total)
  where
    ls = T.lines card
    sigCount sec = length [l | l <- sec, " :: " `T.isInfixOf` l]
    shown = sigCount ls
    sections = splitSections ls
    pairOf sec = case mapMaybe omittedPair sec of
        (p : _) -> Just p
        [] -> Nothing
    omitted = sum [o | sec <- sections, Just (o, _) <- [pairOf sec]]
    total = sum [maybe (sigCount sec) snd (pairOf sec) | sec <- sections]

splitSections :: [Text] -> [[Text]]
splitSections = go
  where
    isHeading = T.isPrefixOf "### "
    go [] = []
    go (l : rest) =
        let (body, rest') = break isHeading rest
         in (l : body) : go rest'

omittedPair :: Text -> Maybe (Int, Int)
omittedPair l
    | "exports omitted" `T.isInfixOf` l =
        case T.words l of
            (_ : o : "of" : t : _) -> (,) <$> readInt o <*> readInt t
            _ -> Nothing
    | otherwise = Nothing
  where
    readInt w =
        if not (T.null w) && T.all isDigit w
            then Just (read (T.unpack w))
            else Nothing

exclusivityViolations :: Text -> [Text]
exclusivityViolations t =
    [p | p <- exclusivityBanned, p `T.isInfixOf` T.toLower t]

exclusivityBanned :: [Text]
exclusivityBanned =
    [ "use only these"
    , "only these names"
    , "only the names"
    , "nothing else"
    , "no other names"
    , "do not use any other"
    ]

usedNames :: Text -> [Text]
usedNames src = nub (filter valueIdent (T.split (not . identChar) src))
  where
    identChar c = isAlphaNum c || c == '_' || c == '\''
    valueIdent t = case T.uncons t of
        Just (c, _) -> isLower c || c == '_'
        Nothing -> False

isNoise :: BrowseEntry -> Bool
isNoise (BrowseEntry name typ) =
    resultHead typ == "String"
        || "trace" `T.isSuffixOf` lastSeg name
        || "Error" `T.isSuffixOf` lastSeg name
        || any (`T.isInfixOf` typ) internalMarkers

internalMarkers :: [Text]
internalMarkers =
    ["CondVec", "CondCache", "NumExpr", "CartFeature", "CarePoint"]

resultHead :: Text -> Text
resultHead typ =
    case T.words (T.filter (`notElem` ("[]()" :: String)) res) of
        (t : _) -> lastSeg t
        [] -> ""
  where
    res = last (T.splitOn "->" (last (T.splitOn "=>" typ)))

lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."

renderTerminal :: ImportStyle -> BrowseEntry -> Text
renderTerminal style (BrowseEntry name typ) =
    "  "
        <> normalizeName style (sanitizeTypeText name)
        <> " :: "
        <> sanitizeTypeText typ
        <> envHint (displayEnvelope typ)

sanitizeTypeText :: Text -> Text
sanitizeTypeText = mapIdentRuns sanitizeToken
  where
    sanitizeToken t
        | Just rest <- stripVersionQualifier t = sanitizeToken rest
        | ".Internal." `T.isInfixOf` t = lastSeg t
        | otherwise = t

stripVersionQualifier :: Text -> Maybe Text
stripVersionQualifier t = case T.breakOn ":" t of
    (pre, post)
        | not (T.null post)
        , versionSuffixed pre ->
            Just (T.drop 1 post)
    _ -> Nothing
  where
    versionSuffixed pre = case T.splitOn "-" pre of
        parts@(_ : _ : _) ->
            let v = last parts
             in not (T.null v)
                    && T.all (\c -> isDigit c || c == '.') v
        _ -> False

mapIdentRuns :: (Text -> Text) -> Text -> Text
mapIdentRuns f = T.concat . map apply . T.groupBy sameClass
  where
    identChar c = isAlphaNum c || c `elem` ("._':-" :: String)
    sameClass a b = identChar a == identChar b
    apply run
        | T.all identChar run && T.any isAlphaNum run = f run
        | otherwise = run

importHint :: ImportStyle -> Text
importHint Unqualified = "imported unqualified — drop the module prefix"
importHint (QualifiedAs p) =
    "imported qualified as " <> p <> " — keep the " <> p <> ". prefix"

envHint :: Envelope -> Text
envHint DisplaySvgUnpack = "   -- show: displaySvg (T.unpack (…))"
envHint DisplayHtml = "   -- show: displayHtml (…)"
envHint Bare = "   -- returns IO (); call it directly"
envHint Hole = ""

tShow :: Int -> Text
tShow = T.pack . show
