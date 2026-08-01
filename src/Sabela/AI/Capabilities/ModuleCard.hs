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
    browseFailed,
    moduleCardValue,
    matchesOutcomeWithDocs,
    importLineFor,
    docSynopsis,
    hitJSON,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))

import Sabela.AI.Capabilities.ModuleCard.Browse (
    browseFailed,
    browseHidden,
 )
import Sabela.AI.Capabilities.ModuleCard.Card (
    moduleCardValue,
    packageCard,
 )
import Sabela.AI.Capabilities.ModuleCard.Hit (
    docSynopsis,
    hitJSON,
    importLineFor,
    matchesOutcomeWithDocs,
 )
import Sabela.AI.Capabilities.ModuleCard.Resolve (
    candidateModules,
    resolveInstalledModule,
    resolveInstalledModules,
    resolveModule,
    storeModuleNames,
 )
import Sabela.AI.PackageIndex (
    PackageEntry (..),
    installedPackages,
    newestNamed,
    storePackageDb,
 )
import System.Process (readProcessWithExitCode)

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
                    Just
                        <$> cardFor
                            mQuery
                            db
                            pkgs
                            pkg
                            resolved
                            (if resolved == modName then Nothing else Just modName)

cardFor ::
    Maybe Text ->
    FilePath ->
    [PackageEntry] ->
    PackageEntry ->
    Text ->
    Maybe Text ->
    IO Value
cardFor mQuery db pkgs pkg modName asked = do
    browsed <- browseHidden db (peName pkg) modName
    syn <- packageSynopsis db (peName pkg)
    pure (moduleCardValue mQuery pkgs pkg modName asked syn browsed)

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
        case [ T.strip v
             | l <- T.lines t
             , Just v <- [T.stripPrefix "synopsis:" l]
             ] of
            (s : _) -> s
            [] -> ""

hiddenPackageCard :: Text -> IO (Maybe Value)
hiddenPackageCard pkgName
    | T.null pkgName = pure Nothing
    | otherwise = do
        mDb <- storePackageDb
        case mDb of
            Nothing -> pure Nothing
            Just db -> do
                pkgs <- installedPackages db
                case newestNamed pkgName pkgs of
                    Nothing -> pure Nothing
                    Just p -> do
                        syn <- packageSynopsis db (peName p)
                        pure (Just (packageCard p syn))

packageCardOf :: [PackageEntry] -> Text -> Text -> Maybe Value
packageCardOf pkgs pkgName syn =
    (`packageCard` syn) <$> newestNamed pkgName pkgs
