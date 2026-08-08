module Siza.Agent.Discover.Merge (
    discoverEnvelope,
    discoverEnvelopeRecent,
    discoverEnvelopeScoped,
    envelopeFrom,
    mergedHits,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, partition, sortOn)
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.RepairDispatch (DiagClass (ClassHiddenPackage), diagClassText)
import Siza.Agent.Discover.Absent (
    absentKnownHits,
    absentScopeNote,
    withIndexFacts,
 )
import Siza.Agent.Discover.Affordance (
    markClashes,
    scopeUse,
    withCardClashes,
 )
import Siza.Agent.Discover.CardAuthority (
    cardAnswers,
    cardField,
    cardInScope,
 )
import Siza.Agent.Discover.Guidance (actionNext, cabalLine, missNext)
import Siza.Agent.Discover.Installed (
    moduleState,
    packageState,
    sessionFacts,
 )
import Siza.Agent.Discover.ModuleList (ModuleView (..))
import Siza.Agent.Discover.Rank (
    demotedCount,
    fuse,
    rankKeyRecent,
    samePackageVariants,
 )
import Siza.Agent.Discover.Render (
    consultedJson,
    dedupSources,
    hackageJson,
    interpretedJson,
 )
import Siza.Agent.Discover.Request (scopeDisclosure)
import Siza.Agent.Discover.ScopeFilter (
    absentTailNote,
    attributeFrom,
    attributedKeep,
    capAbsentKnown,
    contestedNote,
    removedByScope,
    scopeRemovedNote,
 )
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
    emptyScope,
    hitJsonView,
 )

discoverEnvelope ::
    NotebookEnv -> Interpreted -> Int -> [SourceAnswer] -> HackageInfo -> Value
discoverEnvelope env interp = discoverEnvelopeScoped env interp emptyScope

discoverEnvelopeScoped ::
    NotebookEnv ->
    Interpreted ->
    Scope ->
    Int ->
    [SourceAnswer] ->
    HackageInfo ->
    Value
discoverEnvelopeScoped = discoverEnvelopeRecent []

discoverEnvelopeRecent ::
    [Text] ->
    NotebookEnv ->
    Interpreted ->
    Scope ->
    Int ->
    [SourceAnswer] ->
    HackageInfo ->
    Value
discoverEnvelopeRecent recentPkgs env interp scope limit answers hk =
    envelopeFrom env interp scope limit answers hk card ranked
  where
    ranked = mergedHitsRecent recentPkgs env interp answers hk
    card = listToMaybe (mapMaybe saCard answers)

envelopeFrom ::
    NotebookEnv ->
    Interpreted ->
    Scope ->
    Int ->
    [SourceAnswer] ->
    HackageInfo ->
    Maybe Value ->
    [DHit] ->
    Value
envelopeFrom env interp scope limit answers hk card rankedAll =
    object $
        [ "query" .= iRaw interp
        , "interpreted" .= interpretedJson interp
        , "state" .= state
        , "hits" .= map (\h -> hitJsonView (moduleView h) h) shownHits
        , "shown" .= length shownHits
        , "omitted" .= (total - length shownHits)
        , "total" .= total
        , "consulted"
            .= (map consultedJson (dedupSources answers) ++ [hackageJson hk])
        ]
            <> ["card" .= c | Just c <- [shownCard]]
            <> ["next" .= n | Just n <- [next]]
            <> ["narrow" .= n | Just n <- [narrowNote]]
  where
    scopedCard = case card of
        Just c | not (cardInScope scope c) -> Nothing
        _ -> withCardClashes env <$> card
    (answering, unanswering) =
        partition (cardAnswers interp) (maybe [] pure scopedCard)
    cardIsEvidence = not (null answering)
    shownCard = listToMaybe answering
    {- A request scoped to a package asked about that package, which is the
    follow-up 'restOfModules' and 'absentScopeNote' name: it gets the whole
    structure, where a wider search gets the leading modules. -}
    moduleView h
        | Just p <- scPackage scope, p == dhPackage h = ModuleWhole
        | otherwise = ModuleLead
    ranked = markClashes (filter (attributedKeep rankedAll scope) rankedAll)
    total = length ranked
    shownHits = capAbsentKnown (max 1 limit) ranked
    state :: Text
    state = if null ranked && not cardIsEvidence then "not_found" else "found"
    next
        | state == "not_found" =
            Just (missNext env interp scope (dedupSources answers) hk)
        | otherwise = actionNext shownHits
    scopeNote = case absentScopeNote scope answers hk of
        Just n -> Just n
        Nothing -> scopeDisclosure scope total (length rankedAll)
    removedNote = scopeRemovedNote scope (removedByScope rankedAll scope)
    absentNote = absentTailNote ranked shownHits
    demoted = demotedCount (drop (max 1 limit) ranked)
    demoteNote
        | demoted > 0 =
            Just $
                tShow demoted
                    <> " internal-module hits demoted below the public API"
                    <> " (counted in omitted; a higher limit does not reveal them)"
        | otherwise = Nothing
    contestNote = contestedNote answers ranked
    otherNotes = [scopeNote, removedNote, absentNote, demoteNote, contestNote]
    narrowNote = case catMaybes (cardOmittedNote unanswering : otherNotes) of
        [] -> Nothing
        notes -> Just (T.intercalate "; " notes)

{- | A listing that does not answer the query is not carried. What went is
stated as a count beside the card's own denial, so its absence never reads as
"nothing was enumerated".
-}
cardOmittedNote :: [Value] -> Maybe Text
cardOmittedNote [] = Nothing
cardOmittedNote cs = Just (tShow (length cs) <> " card omitted" <> denial)
  where
    denial = case [d | c <- cs, Just d <- [cardField "matched" c]] of
        (d : _) -> " (" <> d <> ")"
        [] -> ""

mergedHits ::
    NotebookEnv -> Interpreted -> [SourceAnswer] -> HackageInfo -> [DHit]
mergedHits = mergedHitsRecent []

mergedHitsRecent ::
    [Text] -> NotebookEnv -> Interpreted -> [SourceAnswer] -> HackageInfo -> [DHit]
mergedHitsRecent recentPkgs env interp answers hk =
    sortOn (rankKeyRecent recentPkgs importedPkgs env interp)
        -- The index speaks only where the session could not, so it is asked
        -- after 'finalise' has settled what the session knows.
        . map (withIndexFacts hk . enrichVersion)
        . fuseAll
        $ map finalise allHits
  where
    -- A hit whose module the answers attribute to one package is not
    -- package-less: left blank, a session hit is invisible to every band,
    -- filter and disclosure that reads a package.
    allHits =
        map
            (attributeFrom pkgModules)
            (concatMap saHits answers ++ absentKnownHits interp answers hk)
    importedPkgs =
        nub $
            [ p
            | a <- answers
            , (p, mods) <- saPkgModules a
            , any (`elem` importTargets) mods
            ]
                ++ [ dhPackage h
                   | h <- allHits
                   , dhModule h `elem` importTargets
                   , not (T.null (dhPackage h))
                   ]
    hiddenPkgs =
        nub $
            [ p
            | h <- concatMap saHits answers
            , dhInstall h == InstHidden
            , let p = dhPackage h
            ]
                ++ [ p
                   | a <- answers
                   , Just (Object o) <- [saCard a]
                   , Just (String s) <- [KM.lookup "status" o]
                   , s == diagClassText ClassHiddenPackage
                   , Just (String p) <- [KM.lookup "package" o]
                   ]
    sessionEvidence = sessionFacts answers
    pkgModules = concatMap saPkgModules answers
    pkgVersions =
        [ (dhPackage h, dhVersion h)
        | h <- allHits
        , not (T.null (dhPackage h))
        , not (T.null (dhVersion h))
        ]
    fuseAll = foldl fuseIn []
    fuseIn acc h = case break (sameKey h) acc of
        (pre, x : post) -> pre ++ [fuse env interp x h] ++ post
        _ -> acc ++ [h]
    sameKey a b =
        (dhName a == dhName b && dhModule a == dhModule b)
            || samePackageVariants a b
    finalise h
        | dhInstall h == InstHidden =
            promote h{dhCabal = fillCabal h}
        | dhInstall h `elem` [InstBuiltin, InstNotebook] = promote h
        | dhPackage h `elem` hiddenPkgs =
            promote
                h
                    { dhInstall = InstHidden
                    , dhCabal = fillCabal h
                    }
        | dhOrigin h == "session" = promote h{dhInstall = InstInstalled}
        | Just st <- moduleState pkgModules sessionEvidence h =
            promote (stated st h)
        | Just st <- packageState sessionEvidence pkgModules h =
            promote (stated st h)
        | dhPackage h `elem` hiKnown hk =
            promote
                h
                    { dhInstall = InstAbsentKnown
                    , dhCabal = fillCabal h
                    }
        | dhOrigin h == "hoogle" && not (T.null (dhPackage h)) =
            promote
                h
                    { dhInstall = InstAbsentKnown
                    , dhCabal = fillCabal h
                    }
        | otherwise = promote h
    -- The state the session's evidence carried, no stronger: a module known
    -- only through a hidden package is present, not loaded.
    stated st h
        | st == InstHidden = h{dhInstall = st, dhCabal = fillCabal h}
        | otherwise = h{dhInstall = st}
    fillCabal h = case dhCabal h of
        Just c -> Just c
        Nothing
            | T.null (dhPackage h) -> Nothing
            | otherwise -> Just (cabalLine (dhPackage h))
    enrichVersion h
        | not (T.null (dhVersion h)) = h
        | Just v <- lookup (dhPackage h) pkgVersions = h{dhVersion = v}
        | otherwise = h
    promote h
        | isNothing (dhUse h) = h{dhUse = scopeUse importTargets (neAliases env) h}
        | otherwise = h
    importTargets = map snd (neAliases env) ++ neImports env
tShow :: Int -> Text
tShow = T.pack . show
