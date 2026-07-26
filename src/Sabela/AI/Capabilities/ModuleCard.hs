{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The module card discover answers a module-shaped query with: what the
module IS, which package ships it, and everything it exports.

Discover dispatches on what the query is. For a NAME it returns ranked hits,
each naming its module so the caller can reach for it; for a MODULE it should
return the module itself. That second half went dark whenever the package was
installed but not exposed, so an episode wanting summary statistics probed
@readCsv@, @describe@, @mean@, @summary@, @stats@, @columns@, @colName@ and
@column@ in turn, guessing at a module whose export list settles it in one
call (@live_test34_wine@).

The live session cannot @:browse@ what its environment does not expose, so a
hidden package's exports come from a short-lived @ghci@ against the store
database instead (about 1.5s, no network, no haddocks needed).
-}
module Sabela.AI.Capabilities.ModuleCard (
    hiddenModuleCard,
    resolveInstalledModule,
    resolveInstalledModules,
    storeModuleNames,
    packageSynopsis,
    browseHidden,
    matchesOutcomeWithDocs,
    docSynopsis,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isAlpha)
import Data.List (nub, partition, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.SessionTypes (SessionBackend (..))
import System.Exit (ExitCode (ExitSuccess))

import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    Match (..),
    defaultSynonyms,
    parseCapabilities,
    relevanceScore,
    unqualify,
 )
import Sabela.AI.ModuleResolve (closestModules, isNoiseModule)
import Sabela.AI.PackageIndex (
    PackageEntry (..),
    installedPackages,
    packagesExposingModule,
    storePackageDb,
 )
import System.Process (readProcessWithExitCode)

-- | Most exports listed. Past this the list stops being readable context.
exportCap :: Int
exportCap = 60

{- | The card for a module the live session does not expose: which installed
package ships it, that package's synopsis, the @-- cabal:@ line that brings it
into scope, and its exports with types. 'Nothing' when no installed package
provides the module, so the caller keeps its own miss handling.
-}
hiddenModuleCard :: Maybe Text -> Text -> IO (Maybe Value)
hiddenModuleCard mQuery modName = do
    mDb <- storePackageDb
    case mDb of
        Nothing -> pure Nothing
        Just db -> do
            pkgs <- installedPackages db
            case resolveModule pkgs modName of
                Nothing -> pure Nothing
                Just (resolved, pkg) ->
                    cardFor
                        mQuery
                        db
                        pkg
                        resolved
                        (if resolved == modName then Nothing else Just modName)

{- | The installed module a query names, EXACTLY or by near spelling, with the
package exposing it.

Exact matching alone answers nothing for a one-character miss: @Data.Frame@ is
a dot away from @DataFrame@, and an episode that queried it was told only that
no such module exists, so it went off to hand-roll the work
(@live_test32_wine@). Reuses the same trigram resolver the add-import repair
trusts, over the whole installed set — hidden packages included, which is
where the near-miss usually points.
-}
resolveModule :: [PackageEntry] -> Text -> Maybe (Text, PackageEntry)
resolveModule pkgs modName = case packagesExposingModule pkgs modName of
    (p : _) -> Just (modName, p)
    [] -> case closestModules 1 moduleNearness modName allModules of
        (near : _) -> case packagesExposingModule pkgs near of
            (p : _) -> Just (near, p)
            [] -> Nothing
        [] -> Nothing
  where
    allModules = concatMap peModules pkgs

{- | 'resolveModule' over the store index, loaded on demand: the installed
module a (possibly misspelt) name denotes, and the package exposing it. The
repair rungs key on this — a wrong module name is only repairable against the
universe of modules that actually exist.
-}
resolveInstalledModule :: Text -> IO (Maybe (Text, PackageEntry))
resolveInstalledModule = fmap listToMaybe . resolveInstalledModules 1

{- | The K nearest installed modules for a (possibly misspelt) name, each with
its package: an exact name answers alone; otherwise nearest-name candidates,
PUBLIC modules ranked ahead of @Internal@\/@Test@ noise. Extensive repair
iterates these until one compiles, instead of betting everything on the
single top guess.
-}
resolveInstalledModules :: Int -> Text -> IO [(Text, PackageEntry)]
resolveInstalledModules k modName = do
    mDb <- storePackageDb
    case mDb of
        Nothing -> pure []
        Just db -> do
            pkgs <- installedPackages db
            case packagesExposingModule pkgs modName of
                (p : _) -> pure [(modName, p)]
                [] ->
                    pure
                        [ (near, p)
                        | near <- rankNear pkgs
                        , p <- take 1 (packagesExposingModule pkgs near)
                        ]
  where
    rankNear pkgs =
        let pool = nub (concatMap peModules pkgs)
            (noise, public) = partition isNoiseModule pool
         in take k (closestModules k moduleNearness modName public)
                <> take k (closestModules k moduleNearness modName noise)

{- | Every installed module name, hidden packages included — the store half
of a rename-candidate pool. Live-session completion only lists EXPOSED
modules, so a misspelling of a hidden module (@Data.Frame@ for @DataFrame@)
had no candidate anywhere and the rename fixer never fired (live_test40).
-}
storeModuleNames :: IO [Text]
storeModuleNames = do
    mDb <- storePackageDb
    case mDb of
        Nothing -> pure []
        Just db -> concatMap peModules <$> installedPackages db

{- | Trigram floor for resolving a module name to a near spelling.
@Data.Frame@ scores 0.5 against @DataFrame@; below this the answer would be a
guess rather than a correction.
-}
moduleNearness :: Double
moduleNearness = 0.4

{- | The card for one resolved module. @resolvedFrom@ records the caller's own
spelling when it differed, so a near-miss answer never reads as an exact one.
-}
cardFor ::
    Maybe Text -> FilePath -> PackageEntry -> Text -> Maybe Text -> IO (Maybe Value)
cardFor mQuery db pkg modName asked = do
    raw <- browseHidden db (peName pkg) modName
    syn <- packageSynopsis db (peName pkg)
    pure
        ( Just
            ( object
                ( [ "module" .= modName
                  , "package" .= peName pkg
                  , "version" .= peVersion pkg
                  , "status" .= ("hidden-package" :: Text)
                  , "cabal" .= ("-- cabal: build-depends: " <> peName pkg)
                  ]
                    <> ["resolvedFrom" .= a | Just a <- [asked]]
                    <> ["synopsis" .= syn | not (T.null syn)]
                    <> exportPairs mQuery modName raw
                )
            )
        )

{- | The export list as @name :: type@ lines, bounded, plus how many were
omitted — a truncated list that does not say so reads as a complete one.

Named functions lead; operators follow. @:browse@ sorts operators first, so a
first-N cut led the DataFrame card with twelve lines of @Expr@ operator
algebra while @readCsv@ and @summarize@ sat in the 342 omitted. Replayed
against the live model, that ordering is the difference between reaching for
the package and drifting to one it knows: useful-exports-first imported
DataFrame 3/4, the operator soup 1/4, no exports 0/4 (probe-ollama,
live_test40). A caller decides from evidence the module answers its task, and
operators are evidence of machinery.
-}
exportPairs :: Maybe Text -> Text -> Text -> [Pair]
exportPairs mQuery modName raw =
    ["exports" .= map render shown]
        <> ["omitted" .= omitted | omitted > 0]
  where
    caps = queryFirst (rankExports modName raw (parseCapabilities "" raw))
    {- The QUERY outranks every static tier: a bounded card only helps if the
    few exports it carries are the ones the question was about, and the
    scorer is the one every search already ranks with — never a second
    notion of relevance. Static namesake order breaks ties and the no-query
    case. Adding more exports instead would spend context without choosing.
    -}
    queryFirst caps0 = case mQuery of
        Just q
            | not (T.null (T.strip q)) ->
                map snd
                    . sortOn (Down . fst)
                    $ [(relevanceScore defaultSynonyms q c, c) | c <- caps0]
        _ -> caps0
    shown = take exportCap caps
    omitted = length caps - length shown
    render c = capName c <> " :: " <> capType c

{- | Core API first: named exports whose signature mentions the module's
NAMESAKE type (module @DataFrame@ → type @DataFrame@), then other named
exports, then operators. Derived from the queried module name alone — a
visibility judgement, never a library one. Browse order put twelve @Expr@
operators first and left @readCsv@ beyond the cap entirely; replayed live,
exports that show the task being answered are what make the model reach for
the package at all (probe-ollama: 3/4 vs 1/4 vs 0/4).
-}
rankExports :: Text -> Text -> [Capability] -> [Capability]
rankExports modName raw caps = core ++ named ++ internal ++ ops
  where
    namesake = T.takeWhileEnd (/= '.') modName
    (alpha0, ops) = partition isNamed caps
    -- The existing visibility judgement, applied here: what @:browse@
    -- attributes to an @.Internal.@ module is plumbing, below the public API
    -- (the same demotion discover's ranked hits already disclose).
    (internal, alpha) = partition (\c -> capName c `Set.member` internalNames) alpha0
    (core, named) = partition mentionsNamesake alpha
    isNamed c = maybe False (isAlpha . fst) (T.uncons (capName c))
    mentionsNamesake c = namesake `T.isInfixOf` capType c
    internalNames =
        Set.fromList
            [ unqualify w
            | l <- T.lines raw
            , (w : _) <- [T.words l]
            , ".Internal." `T.isInfixOf` w
            ]

{- | A package's one-line synopsis from @ghc-pkg@. Empty when unavailable:
the card is still worth having without it.
-}
packageSynopsis :: FilePath -> Text -> IO Text
packageSynopsis db pkg = do
    r <-
        try
            ( readProcessWithExitCode
                "ghc-pkg"
                ["--package-db=" ++ db, "field", T.unpack pkg, "synopsis"]
                ""
            )
    pure $ case r of
        Left (_ :: SomeException) -> ""
        Right (ExitSuccess, out, _) -> firstSynopsis (T.pack out)
        Right _ -> ""
  where
    firstSynopsis t =
        case [T.strip v | l <- T.lines t, Just v <- [T.stripPrefix "synopsis:" l]] of
            (s : _) -> s
            [] -> ""

{- | @:browse@ a module of a package the live session does not expose, in a
throwaway @ghci@ against the store database. Empty on any failure — a card
without exports beats a tool error.
-}
browseHidden :: FilePath -> Text -> Text -> IO Text
browseHidden db pkg modName = do
    r <-
        try
            ( readProcessWithExitCode
                "ghci"
                [ "-v0"
                , "-package-db=" ++ db
                , "-package"
                , T.unpack pkg
                ]
                (":browse " <> T.unpack modName <> "\n")
            )
    pure $ case r of
        Left (_ :: SomeException) -> ""
        Right (ExitSuccess, out, _) -> T.pack out
        Right _ -> ""

-- | One ranked hit, with its haddock synopsis when one was fetched.
hitJSON :: Text -> Hit -> Value
hitJSON doc h =
    object
        ( [ "module" .= capModule (hitCap h)
          , "name" .= capName (hitCap h)
          , "type" .= capType (hitCap h)
          , "via" .= matchName (hitVia h)
          ]
            <> ["doc" .= doc | not (T.null doc)]
        )

{- | 'matchesOutcome' with the haddock synopsis of the leading hits attached.

A type alone does not say which of @describeColumns@, @summarize@ and @mean@
answers "summary statistics"; the docs do, and the session already holds them
(@:doc@ answers for store packages). Carried on the search result because the
separate lookup tool went unused in every recorded episode.
-}
matchesOutcomeWithDocs :: SessionBackend -> Text -> [Hit] -> IO ToolOutcome
matchesOutcomeWithDocs backend q hits = do
    let (lead, rest) = splitAt docAttachCap hits
    withDocs <- mapM attach lead
    pure
        ( okOutcome
            (object ["query" .= q, "matches" .= (withDocs <> map (hitJSON "") rest)])
        )
  where
    attach h = do
        raw <- sbQueryDoc backend (capName (hitCap h))
        pure (hitJSON (docSynopsis raw) h)

{- | How many leading hits carry a synopsis. Each costs one session query, and
past the first few the model is choosing from the type anyway.
-}
docAttachCap :: Int
docAttachCap = 3

-- | Longest synopsis carried on a hit; docs run to pages, the choice does not.
docSynopsisChars :: Int
docSynopsisChars = 240

{- | The first prose of a @:doc@ answer: GHCi echoes the identifier and an
@-- Identifier defined in@ line before the haddock body, and the body arrives
wrapped in @{\-| … -\}@ or led by @-- |@. Empty when there is no prose (a
not-in-scope name answers with a message, never documentation).
-}
docSynopsis :: Text -> Text
docSynopsis raw = case prose of
    [] -> ""
    ls -> T.take docSynopsisChars (T.unwords ls)
  where
    prose =
        take
            3
            [ l
            | l <- map (T.strip . strip) (T.lines raw)
            , not (T.null l)
            , not ("Identifier defined in" `T.isInfixOf` l)
            , not ("::" `T.isInfixOf` l)
            , not (T.isPrefixOf "<" l)
            ]
    strip =
        T.replace "{-|" ""
            . T.replace "-}" ""
            . T.replace "-- |" ""
            . T.replace "-- " ""

matchName :: Match -> Text
matchName ByName = "name"
matchName ByType = "type"
matchName BySynonym = "synonym"
matchName ByModule = "module"
