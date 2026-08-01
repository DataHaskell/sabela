{-# LANGUAGE OverloadedStrings #-}

{- | Resolving a module name the caller supplied to a module that is actually
installed, and to the package that exposes it.
-}
module Sabela.AI.Capabilities.ModuleCard.Resolve (
    resolveModule,
    resolveInstalledModule,
    resolveInstalledModules,
    candidateModules,
    storeModuleNames,
) where

import Data.List (isInfixOf, isSuffixOf, nub, partition, sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ModuleResolve (closestModules, isNoiseModule)
import Sabela.AI.PackageIndex (
    PackageEntry (..),
    installedPackages,
    newestExposing,
    packagesExposingModule,
    storePackageDb,
 )

resolveModule :: [PackageEntry] -> Text -> Maybe (Text, PackageEntry)
resolveModule pkgs modName = case newestExposing pkgs modName of
    Just p -> Just (modName, p)
    Nothing -> case candidateModules 1 modName allModules of
        (near : _) -> (,) near <$> newestExposing pkgs near
        [] -> Nothing
  where
    allModules = concatMap peModules pkgs

squashModule :: Text -> Text
squashModule = T.toLower . T.filter (/= '.')

candidateModules :: Int -> Text -> [Text] -> [Text]
candidateModules k modName mods =
    take k (nub (squashEq <> named <> squashNear <> trigram))
  where
    pool = nub mods
    q = squashModule modName
    squashEq = rankPublic [m | m <- pool, squashModule m == q]
    named = rankPublic (componentMatches modName pool)
    squashNear
        | T.length q >= 4 =
            rankPublic
                [ m
                | m <- pool
                , let s = squashModule m
                , q `T.isSuffixOf` s || s `T.isSuffixOf` q
                ]
        | otherwise = []
    trigram = closestModules k moduleNearness modName pool

componentMatches :: Text -> [Text] -> [Text]
componentMatches q mods
    | T.null q = []
    | otherwise = endsWith <> [m | m <- carries, m `notElem` endsWith]
  where
    qc = T.splitOn "." q
    endsWith = [m | m <- nub mods, qc `isSuffixOf` T.splitOn "." m]
    carries = [m | m <- nub mods, qc `isInfixOf` T.splitOn "." m]

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
            case newestExposing pkgs modName of
                Just p -> pure [(modName, p)]
                Nothing ->
                    pure
                        [ (near, p)
                        | near <- rankNear pkgs
                        , Just p <- [newestExposing pkgs near]
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
