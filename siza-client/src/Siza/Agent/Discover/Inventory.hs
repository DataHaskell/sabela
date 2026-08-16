module Siza.Agent.Discover.Inventory (
    inventoryEnvelope,
    inventoryRows,
    topicTokens,
) where

import Data.Aeson (Value)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ModuleResolve (isNoiseModule)
import Siza.Agent.Discover.CabalFacts (PkgFacts, pfVersion)
import Siza.Agent.Discover.Guidance (cabalLine, cabalLineV)
import Siza.Agent.Discover.Merge (envelopeFrom, mergedHits)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv,
    Scope,
    SourceAnswer (..),
 )

topicTokens :: Interpreted -> [Text]
topicTokens interp = case iTerms interp of
    [] -> [T.toLower (iName interp)]
    ts -> ts

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
    envelopeFrom env interp scope limit answers hk card rows
  where
    rows =
        inventoryRows
            (concatMap saPkgModules answers)
            (mergedHits env interp answers hk)
            interp
            lexical
            (hiFacts hk)
    -- Inventory is the mode the miss guidance recommends for "what is
    -- available"; dropping the card here answered that question with a bare
    -- row naming the package and nothing it contains.
    card = listToMaybe (mapMaybe saCard answers)

{- | One row per package the answers know, over the modules they named. A row
whose sources named no module states none, and a package row with no module
states nothing a caller can import, so it is dropped rather than padded.
-}
inventoryRows ::
    [(Text, [Text])] ->
    [DHit] ->
    Interpreted ->
    [Text] ->
    [(Text, PkgFacts)] ->
    [DHit]
inventoryRows pkgMods merged interp lexical facts =
    sortOn (\h -> (fromEnum (dhInstall h), dhPackage h)) (pkgRows ++ lexRows)
  where
    grouped = groupByPackage merged
    pkgRows = [r | (p, hs) <- grouped, Just r <- [row p hs]]
    lexRows =
        [ DHit
            { dhName = n
            , dhType = ""
            , dhModule = notInstalled
            , dhPackage = n
            , dhVersion = ""
            , dhInstall = InstAbsentKnown
            , dhKind = matchOf n
            , dhOrigin = "hackage"
            , dhCabal = Just (cabalLineV n (pfVersion <$> lookup n facts))
            , dhUse = Nothing
            , dhClash = Nothing
            , dhFacts = lookup n facts
            }
        | n <- lexical
        , n `notElem` map fst grouped
        ]
    row p hs = rowOver <$> leadModule p hs
      where
        state = minimum (map dhInstall hs)
        rowOver m =
            DHit
                { dhName = p
                , dhType = ""
                , dhModule = m
                , dhPackage = p
                , dhVersion = firstNonEmpty (map dhVersion hs)
                , dhInstall = state
                , dhKind = matchOf p
                , dhOrigin = firstNonEmpty (map dhOrigin hs)
                , dhCabal = cabalFor p state hs
                , dhUse = Nothing
                , dhClash = Nothing
                , dhFacts = factsFor p state
                }
    -- Only a package the session cannot speak for needs the index's words.
    factsFor p state
        | state `elem` [InstAbsentKnown, InstAbsentUnknown] = lookup p facts
        | otherwise = Nothing
    cabalFor p state hs
        | state `elem` [InstHidden, InstAbsentKnown] =
            case mapMaybe dhCabal hs of
                (c : _) -> Just c
                [] -> Just (cabalLine p)
        | otherwise = Nothing
    leadModule p hs =
        listToMaybe (sortOn (\m -> (isNoiseModule m, T.length m)) (mods p hs))
    mods p hs =
        [ m
        | m <- map dhModule hs ++ concat [ms | (p', ms) <- pkgMods, p' == p]
        , not (T.null m)
        , m /= notInstalled
        ]
    matchOf p =
        if T.toLower p `elem` topicTokens interp
            then MkExact
            else MkSubstring

{- | The stand-in a catalogue-only row carries where a module would go. It is
not a module, so no module is read out of it; recorded here so the one reader
and the one writer agree.
-}
notInstalled :: Text
notInstalled = "(not installed)"

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
