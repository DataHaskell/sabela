module Siza.Agent.Discover.Merge (
    discoverEnvelope,
    discoverEnvelopeRecent,
    discoverEnvelopeScoped,
    envelopeFrom,
    mergedHits,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.RepairDispatch (DiagClass (ClassHiddenPackage), diagClassText)
import Siza.Agent.Discover.Guidance (actionNext, cabalLine, missNext)
import Siza.Agent.Discover.Interpret (stripVersion)
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
    attributedKeep,
    capAbsentKnown,
    removedByScope,
    scopeRemovedNote,
 )
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
    emptyScope,
    hitJson,
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
        , "hits" .= map hitJson shownHits
        , "shown" .= length shownHits
        , "omitted" .= (total - length shownHits)
        , "total" .= total
        , "consulted"
            .= (map consultedJson (dedupSources answers) ++ [hackageJson hk])
        ]
            <> ["card" .= c | Just c <- [scopedCard]]
            <> ["next" .= n | Just n <- [next]]
            <> ["narrow" .= n | Just n <- [narrowNote]]
  where
    scopedCard = case (scModule scope, card) of
        (Just m, Just c) | cardModule c /= Just m -> Nothing
        _ -> card
    ranked = filter (attributedKeep rankedAll scope) rankedAll
    total = length ranked
    shownHits = capAbsentKnown (max 1 limit) ranked
    state :: Text
    state = if null ranked && isNothing card then "not_found" else "found"
    next
        | state == "not_found" =
            Just (missNext env interp scope (dedupSources answers) hk)
        | otherwise = actionNext shownHits
    scopeNote = scopeDisclosure scope total (length rankedAll)
    removedNote = scopeRemovedNote scope (removedByScope rankedAll scope)
    absentNote = absentTailNote ranked shownHits
    demoted = demotedCount (drop (max 1 limit) ranked)
    demoteNote
        | demoted > 0 =
            Just $
                tShow demoted
                    <> " internal-module hits demoted below the public API"
                    <> " (counted in omitted; raise limit to see them)"
        | otherwise = Nothing
    narrowNote =
        case catMaybes [scopeNote, removedNote, absentNote, demoteNote] of
            [] -> Nothing
            notes -> Just (T.intercalate "; " notes)

mergedHits ::
    NotebookEnv -> Interpreted -> [SourceAnswer] -> HackageInfo -> [DHit]
mergedHits = mergedHitsRecent []

mergedHitsRecent ::
    [Text] -> NotebookEnv -> Interpreted -> [SourceAnswer] -> HackageInfo -> [DHit]
mergedHitsRecent recentPkgs env interp answers hk =
    sortOn (rankKeyRecent recentPkgs importedPkgs env interp)
        . map enrichVersion
        . fuseAll
        $ map finalise allHits
  where
    allHits = concatMap saHits answers ++ hackageOnlyHit
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
    sessionMods =
        nub $
            [dhModule h | a <- answers, saSource a == "session", h <- saHits a]
                ++ [ m
                   | a <- answers
                   , Just (Object o) <- [saCard a]
                   , Just (String "ok") <- [KM.lookup "status" o]
                   , Just (String m) <- [KM.lookup "module" o]
                   ]
    sessionPkgs =
        nub
            [ p
            | a <- answers
            , (p, mods) <- saPkgModules a
            , any (`elem` sessionMods) mods
            ]
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
        | dhModule h `elem` sessionMods && not (T.null (dhModule h)) =
            promote h{dhInstall = InstInstalled}
        | dhPackage h `elem` sessionPkgs && not (T.null (dhPackage h)) =
            promote h{dhInstall = InstInstalled}
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
        | dhKind h == MkExact
        , dhModule h `elem` importTargets
        , isNothing (dhUse h) =
            h{dhUse = Just (importUse (dhModule h))}
        | otherwise = h
    importTargets = map snd (neAliases env) ++ neImports env
    importUse m = case [a | (a, m') <- neAliases env, m' == m] of
        (a : _) -> "already imported as " <> a <> " (notebook import)"
        [] -> "already imported by the notebook"
    hackageOnlyHit =
        [ DHit
            pkg
            ""
            "(not installed)"
            pkg
            "unknown"
            InstAbsentKnown
            MkExact
            "hackage"
            (Just (cabalLine pkg))
            Nothing
        | iShape interp `elem` ["name", "package"]
        , let pkg = stripVersion (iName interp)
        , pkg `elem` hiKnown hk
        , pkg `notElem` [dhPackage h | a <- answers, h <- saHits a]
        ]

tShow :: Int -> Text
tShow = T.pack . show

cardModule :: Value -> Maybe Text
cardModule (Object o) = case KM.lookup "module" o of
    Just (String m) | not (T.null m) -> Just m
    _ -> Nothing
cardModule _ = Nothing
