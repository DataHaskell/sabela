{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.ModuleCard (
    hiddenModuleCard,
    hiddenPackageCard,
    packageCardOf,
    resolveInstalledModule,
    resolveInstalledModules,
    candidateModules,
    storeModuleNames,
    packageSynopsis,
    browseHidden,
    matchesOutcomeWithDocs,
    importLineFor,
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

exportCap :: Int
exportCap = 60

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

resolveModule :: [PackageEntry] -> Text -> Maybe (Text, PackageEntry)
resolveModule pkgs modName = case packagesExposingModule pkgs modName of
    (p : _) -> Just (modName, p)
    [] -> case candidateModules 1 modName allModules of
        (near : _) -> case packagesExposingModule pkgs near of
            (p : _) -> Just (near, p)
            [] -> Nothing
        [] -> Nothing
  where
    allModules = concatMap peModules pkgs

candidateModules :: Int -> Text -> [Text] -> [Text]
candidateModules k modName mods =
    take
        k
        ( named
            <> [m | m <- closestModules k moduleNearness modName mods, m `notElem` named]
        )
  where
    named = rankPublic (componentMatches modName mods)

componentMatches :: Text -> [Text] -> [Text]
componentMatches q mods
    | T.null q || T.any (== '.') q = []
    | otherwise = endsWith <> [m | m <- carries, m `notElem` endsWith]
  where
    endsWith = [m | m <- nub mods, lastComponent m == q]
    carries = [m | m <- nub mods, q `elem` T.splitOn "." m]
    lastComponent m = case reverse (T.splitOn "." m) of
        (c : _) -> c
        [] -> m

rankPublic :: [Text] -> [Text]
rankPublic ms =
    let (noise, public) = partition isNoiseModule ms
     in sortOn T.length public <> sortOn T.length noise

resolveInstalledModule :: Text -> IO (Maybe (Text, PackageEntry))
resolveInstalledModule = fmap listToMaybe . resolveInstalledModules 1

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
         in candidateModules k modName public
                <> candidateModules k modName noise

storeModuleNames :: IO [Text]
storeModuleNames = do
    mDb <- storePackageDb
    case mDb of
        Nothing -> pure []
        Just db -> concatMap peModules <$> installedPackages db

moduleNearness :: Double
moduleNearness = 0.4

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

exportPairs :: Maybe Text -> Text -> Text -> [Pair]
exportPairs mQuery modName raw =
    ["exports" .= map render shown]
        <> ["omitted" .= omitted | omitted > 0]
  where
    caps = queryFirst (rankExports modName raw (parseCapabilities "" raw))
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

rankExports :: Text -> Text -> [Capability] -> [Capability]
rankExports modName raw caps = core ++ named ++ internal ++ ops
  where
    namesake = T.takeWhileEnd (/= '.') modName
    (alpha0, ops) = partition isNamed caps
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

hitJSON :: Text -> Hit -> Value
hitJSON doc h =
    object
        ( [ "module" .= capModule (hitCap h)
          , "name" .= capName (hitCap h)
          , "type" .= capType (hitCap h)
          , "via" .= matchName (hitVia h)
          ]
            <> ["doc" .= doc | not (T.null doc)]
            <> ["import" .= imp | Just imp <- [importLineFor (hitCap h)]]
        )

importLineFor :: Capability -> Maybe Text
importLineFor c
    | T.null m || T.null n = Nothing
    | T.all (`elem` operatorChars) n = Just (imp ("(" <> n <> ")"))
    | otherwise = Just (imp n)
  where
    m = capModule c
    n = capName c
    imp entity = "import " <> m <> " (" <> entity <> ")"

operatorChars :: String
operatorChars = "!#$%&*+./<=>?@\\^|-~:"

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

docAttachCap :: Int
docAttachCap = 3

docSynopsisChars :: Int
docSynopsisChars = 240

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

hiddenPackageCard :: Text -> IO (Maybe Value)
hiddenPackageCard pkgName
    | T.null pkgName = pure Nothing
    | otherwise = do
        mDb <- storePackageDb
        case mDb of
            Nothing -> pure Nothing
            Just db -> do
                pkgs <- installedPackages db
                case [p | p <- pkgs, peName p == pkgName] of
                    [] -> pure Nothing
                    (p : _) -> do
                        syn <- packageSynopsis db (peName p)
                        pure (Just (packageCard p syn))

packageCardOf :: [PackageEntry] -> Text -> Text -> Maybe Value
packageCardOf pkgs pkgName syn = case [p | p <- pkgs, peName p == pkgName] of
    (p : _) -> Just (packageCard p syn)
    [] -> Nothing

packageCard :: PackageEntry -> Text -> Value
packageCard p syn =
    object
        ( [ "package" .= peName p
          , "version" .= peVersion p
          , "status" .= ("hidden-package" :: Text)
          , "cabal" .= ("-- cabal: build-depends: " <> peName p)
          , "modules" .= take packageModuleCap (peModules p)
          ]
            <> ["synopsis" .= syn | not (T.null syn)]
            <> [ "moreModules" .= (length (peModules p) - packageModuleCap)
               | length (peModules p) > packageModuleCap
               ]
        )

packageModuleCap :: Int
packageModuleCap = 20
