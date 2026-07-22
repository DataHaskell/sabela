{- | @mode:"inventory"@ (docs/discover/search-api.md section 3, defect M6):
answer "what is available for this topic" as ONE bounded card of candidate
packages across the three honest install states — installed, hidden (with the
cabal line), absent-but-on-Hackage (with the cabal line) — over the same
union-merge catalogue the search mode uses. No topic-to-library table:
membership is lexical over names the sources returned.
-}
module Siza.Agent.Discover.Inventory (
    inventoryEnvelope,
    inventoryRows,
    topicTokens,
) where

import Data.Aeson (Value)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ModuleResolve (isNoiseModule)
import Siza.Agent.Discover.Guidance (cabalLine)
import Siza.Agent.Discover.Merge (envelopeFrom, mergedHits)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo,
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv,
    Scope,
    SourceAnswer,
 )

-- | The lexical topic terms of a query: prose terms, else the resolved name.
topicTokens :: Interpreted -> [Text]
topicTokens interp = case iTerms interp of
    [] -> [T.toLower (iName interp)]
    ts -> ts

{- | The inventory answer: one package row per candidate, best install state
first, rendered through the same bounded envelope as search (R3.4, R3.9).
-}
inventoryEnvelope ::
    NotebookEnv ->
    Interpreted ->
    Scope ->
    Int ->
    [SourceAnswer] ->
    HackageInfo ->
    [Text] ->
    Value
inventoryEnvelope env interp scope limit answers hk lexical =
    envelopeFrom env interp scope limit answers hk Nothing rows
  where
    rows = inventoryRows (mergedHits env interp answers hk) interp lexical

{- | One row per package: its best-evidenced install state, any known
version, its lead public module, and the cabal line whenever the state needs
one — the three states are never conflated (R1.3).
-}
inventoryRows :: [DHit] -> Interpreted -> [Text] -> [DHit]
inventoryRows merged interp lexical =
    sortOn (\h -> (fromEnum (dhInstall h), dhPackage h)) (pkgRows ++ lexRows)
  where
    grouped = groupByPackage merged
    pkgRows = [row p hs | (p, hs) <- grouped]
    lexRows =
        [ DHit
            n
            ""
            "(not installed)"
            n
            ""
            InstAbsentKnown
            (matchOf n)
            "hackage"
            (Just (cabalLine n))
            Nothing
        | n <- lexical
        , n `notElem` map fst grouped
        ]
    row p hs =
        DHit
            { dhName = p
            , dhType = ""
            , dhModule = leadModule hs
            , dhPackage = p
            , dhVersion = firstNonEmpty (map dhVersion hs)
            , dhInstall = state
            , dhKind = matchOf p
            , dhOrigin = firstNonEmpty (map dhOrigin hs)
            , dhCabal = cabalFor p state hs
            , dhUse = Nothing
            }
      where
        state = minimum (map dhInstall hs)
    cabalFor p state hs
        | state `elem` [InstHidden, InstAbsentKnown] =
            case mapMaybe dhCabal hs of
                (c : _) -> Just c
                [] -> Just (cabalLine p)
        | otherwise = Nothing
    -- Public modules lead; internal ones are a last resort (section 7).
    leadModule hs =
        case sortOn (\m -> (isNoiseModule m, T.length m)) (mods hs) of
            (m : _) -> m
            [] -> "(package)"
    mods hs = [m | h <- hs, let m = dhModule h, not (T.null m), m /= "(not installed)"]
    matchOf p =
        if any (`T.isInfixOf` T.toLower p) (topicTokens interp)
            then MkExact
            else MkSubstring

groupByPackage :: [DHit] -> [(Text, [DHit])]
groupByPackage = foldr add []
  where
    add h acc
        | T.null (dhPackage h) = acc
        | otherwise = case break ((== dhPackage h) . fst) acc of
            (pre, (p, hs) : post) -> pre ++ (p, h : hs) : post
            _ -> acc ++ [(dhPackage h, [h])]

firstNonEmpty :: [Text] -> Text
firstNonEmpty ts = case filter (not . T.null) ts of
    (t : _) -> t
    [] -> ""
