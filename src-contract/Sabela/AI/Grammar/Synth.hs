{-# LANGUAGE OverloadedStrings #-}

{- | Synthesise a notebook-DSL grammar from live @:browse@ output. Where
'Sabela.AI.Grammar' carries the static cold-start block, this turns the real,
version-current surface a @:browse M@ returned into a grammar fragment: each
value binding becomes a terminal written in the form the cell imports it
(normalised), tagged with the display envelope its result type selects, under the
verb-to-sketch start symbol. Injected after a discover/browse so the model
predicts against names that actually exist rather than inventing them.
-}
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

{- | One browsed module: its name, how the cell imports it (which decides
qualifier normalisation), and the raw @:browse@ text it returned.
-}
data Surface = Surface
    { surfModule :: Text
    , surfStyle :: ImportStyle
    , surfBrowse :: Text
    }
    deriving (Eq, Show)

{- | Render the live grammar for one or more browsed surfaces: a header naming
the start symbol, then per surface its discovered terminals in normalised write
form, each tagged with the display envelope its result type selects. A surface
whose browse holds no value bindings contributes only its heading.
-}
synthesizeGrammar :: [Surface] -> Text
synthesizeGrammar = synthesizeGrammarProven []

{- | 'synthesizeGrammar' with compile-proven names (R9.7): a name the target
cell uses is exempt from the noise filter, so a card can never contradict a
compile the model just landed.
-}
synthesizeGrammarProven :: [Text] -> [Surface] -> Text
synthesizeGrammarProven proven surfaces =
    renderSections [(sec, length (secTerminals sec)) | sec <- secs]
  where
    secs = map (renderSection proven) surfaces

{- | 'synthesizeGrammarProven' under a hard character budget (R3.9): the card
shrinks by shedding trailing terminals into a per-surface reconciling
"omitted" line — counts always reconcile, never blind truncation.
-}
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

-- | One rendered surface: its heading, and every terminal it can show.
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

{- | Render sections, each keeping its first @n@ terminals; a shortfall gets
the reconciling omitted line.
-}
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

-- | Shed the last terminal of the last surface that still shows any.
dropLastTerminal :: [(Section, Int)] -> Maybe [(Section, Int)]
dropLastTerminal kept = case break ((> 0) . snd) (reverse kept) of
    (post, (sec, n) : pre) ->
        Just (reverse pre ++ (sec, n - 1) : reverse post)
    _ -> Nothing

grammarHeader :: Text
grammarHeader =
    "## Live API grammar (synthesised from :browse — verified names, not exhaustive; the search tools cover the rest)"

{- | The (shown, omitted, total) counts a rendered card reconciles (R3.9):
shown counts signature lines; omitted/total parse back from each surface's
omitted line, so the triple cross-checks the renderer's arithmetic.
-}
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

-- | Group a card's lines into its preamble and per-@###@-surface sections.
splitSections :: [Text] -> [[Text]]
splitSections = go
  where
    isHeading = T.isPrefixOf "### "
    go [] = []
    go (l : rest) =
        let (body, rest') = break isHeading rest
         in (l : body) : go rest'

-- | Parse "… O of T exports omitted" back into its pair, if the line is one.
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

{- | Exclusivity phrases found in a card (R9.7, search-api.md §6): a
synthesised card must never claim its listing is the complete surface.
-}
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

{- | Lexical identifier tokens of a cell source, for the proven-name set: the
extraction stays lexical because a token only counts once intersected with a
browse's export set.
-}
usedNames :: Text -> [Text]
usedNames src = nub (filter valueIdent (T.split (not . identChar) src))
  where
    identChar c = isAlphaNum c || c == '_' || c == '\''
    valueIdent t = case T.uncons t of
        Just (c, _) -> isLower c || c == '_'
        Nothing -> False

{- | Drop bindings that are never part of the notebook DSL and only bloat the
grammar (it rides in context every turn): error/diagnostic @String@ helpers,
debug @trace@, and internal type-currency (CART/condition-synthesis plumbing).
Keeps the user-facing surface — readers, transforms, @Expr@/chart builders.
-}
isNoise :: BrowseEntry -> Bool
isNoise (BrowseEntry name typ) =
    resultHead typ == "String"
        || "trace" `T.isSuffixOf` lastSeg name
        || "Error" `T.isSuffixOf` lastSeg name
        || any (`T.isInfixOf` typ) internalMarkers

-- | Internal type names that mark a binding as library plumbing, not DSL.
internalMarkers :: [Text]
internalMarkers =
    ["CondVec", "CondCache", "NumExpr", "CartFeature", "CarePoint"]

{- | Head type constructor of a signature's RESULT, after class context and
arrows, stripped of list/tuple punctuation and any module qualifier.
-}
resultHead :: Text -> Text
resultHead typ =
    case T.words (T.filter (`notElem` ("[]()" :: String)) res) of
        (t : _) -> lastSeg t
        [] -> ""
  where
    res = last (T.splitOn "->" (last (T.splitOn "=>" typ)))

-- | Last dot-separated segment of a (possibly qualified) name.
lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."

-- | The ONE rendering seam: every name and type is sanitised here (R3.10).
renderTerminal :: ImportStyle -> BrowseEntry -> Text
renderTerminal style (BrowseEntry name typ) =
    "  "
        <> normalizeName style (sanitizeTypeText name)
        <> " :: "
        <> sanitizeTypeText typ
        <> envHint (displayEnvelope typ)

{- | Strip package-version qualifiers (@aeson-2.3.1.0:M.N.Value@) and reduce
internal-module paths (@Data.Aeson.Types.Internal.Value@) to the
import-usable public name — no rendered surface may carry either token.
-}
sanitizeTypeText :: Text -> Text
sanitizeTypeText = mapIdentRuns sanitizeToken
  where
    sanitizeToken t
        | Just rest <- stripVersionQualifier t = sanitizeToken rest
        | ".Internal." `T.isInfixOf` t = lastSeg t
        | otherwise = t

-- | @pkg-1.2.3:Rest@ -> @Rest@; 'Nothing' when the token is not one.
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

{- | Apply a rewrite to each maximal identifier-shaped run, preserving the
punctuation between runs.
-}
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
