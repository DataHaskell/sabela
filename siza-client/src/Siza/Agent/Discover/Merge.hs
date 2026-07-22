{- | The discover union merge (docs/discover/search-api.md sections 5-8): all
sources' answers are unioned — never laddered — classified into the four-plus-
two install states, ranked exact-name-first in total-order strata (R3.2), and
rendered as ONE bounded envelope with cap disclosure (R3.3, R3.4).
-}
module Siza.Agent.Discover.Merge (
    discoverEnvelope,
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

import Siza.Agent.Discover.Guidance (actionNext, cabalLine, missNext)
import Siza.Agent.Discover.Interpret (stripVersion)
import Siza.Agent.Discover.Rank (
    demotedCount,
    fuse,
    rankKeyIn,
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
    Scope,
    SourceAnswer (..),
    emptyScope,
    hitJson,
 )

-- | Build the single response envelope from every source's answer.
discoverEnvelope ::
    NotebookEnv -> Interpreted -> Int -> [SourceAnswer] -> HackageInfo -> Value
discoverEnvelope env interp = discoverEnvelopeScoped env interp emptyScope

{- | 'discoverEnvelope' under the request's scope filters (R2.7): the filter
is honoured at the merge — after ranking, before the limit — and a filter
that matched nothing or changed nothing says so in @narrow@, never silently.
-}
discoverEnvelopeScoped ::
    NotebookEnv ->
    Interpreted ->
    Scope ->
    Int ->
    [SourceAnswer] ->
    HackageInfo ->
    Value
discoverEnvelopeScoped env interp scope limit answers hk =
    envelopeFrom env interp scope limit answers hk card ranked
  where
    ranked = mergedHits env interp answers hk
    card = listToMaybe (mapMaybe saCard answers)

{- | Render the one envelope shape from an already-ranked hit list: scope
filter honoured with disclosure, limit with reconciling counts, and demoted
internal hits summarised — never dropped (R3.1-R3.4). Shared by the search
and inventory modes.
-}
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
            <> ["card" .= c | Just c <- [card]]
            <> ["next" .= n | Just n <- [next]]
            <> ["narrow" .= n | Just n <- [narrowNote]]
  where
    -- Section 3.3: the scope filter is a post-union predicate over each
    -- hit's attributed modules/packages; removals are disclosed below.
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

-- | Union, finalise install states, fuse duplicates, rank by strata.
mergedHits ::
    NotebookEnv -> Interpreted -> [SourceAnswer] -> HackageInfo -> [DHit]
mergedHits env interp answers hk =
    sortOn (rankKeyIn importedPkgs env interp)
        . map enrichVersion
        . fuseAll
        $ map finalise allHits
  where
    allHits = concatMap saHits answers ++ hackageOnlyHit
    -- Packages the notebook imports, by union evidence (R4.5): a disclosed
    -- module list or a hit module that matches an imported module.
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
                   , Just (String "hidden-package") <- [KM.lookup "status" o]
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
    -- A package is session-proven installed when ANY of its disclosed
    -- modules browsed ok — one probed module vouches for its siblings.
    sessionPkgs =
        nub
            [ p
            | a <- answers
            , (p, mods) <- saPkgModules a
            , any (`elem` sessionMods) mods
            ]
    -- Any version any source holds for a package (the live package env wins
    -- by arriving first), so a session hit can be enriched at the merge.
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
    -- Duplicate keys: same (name, module), or same-package name-variant
    -- stubs (R6-T1) — dedup, never a drop, so R3.3 conservation holds.
    sameKey a b =
        (dhName a == dhName b && dhModule a == dhModule b)
            || samePackageVariants a b
    -- Install state by evidence class: environment > session > hidden card >
    -- hackage membership > hoogle-implied upstream; never by library name.
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
    -- Session-hit version enrichment (carryover 6): fill from the package
    -- env's union knowledge; an unknown version is left empty (suppressed),
    -- never the raw "unknown: not in package env" provenance leak (R3.10).
    enrichVersion h
        | not (T.null (dhVersion h)) = h
        | Just v <- lookup (dhPackage h) pkgVersions = h{dhVersion = v}
        | otherwise = h
    -- The notebook's imports make one candidate the obvious referent (R4.5).
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
