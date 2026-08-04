{-# LANGUAGE ScopedTypeVariables #-}

module Siza.Agent.Discover.Hackage (
    hackageNamesPath,
    loadHackageNames,
    hackageInfoFor,
    hackageMatching,
    hackageFactsPath,
    hackageFactsFor,
    hackageModuleOwners,
    withModuleOwners,
    withFactsFor,
) where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getModificationTime)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Siza.Agent.Discover.CabalFacts (PkgFacts (..), parseFactsRow)
import Siza.Agent.Discover.Types (HackageInfo (..))

hackageNamesPath :: IO FilePath
hackageNamesPath =
    fromMaybe ("data" </> "hackage-packages.txt")
        <$> lookupEnv "SABELA_HACKAGE_NAMES"

loadHackageNames :: IO (Maybe (S.Set Text))
loadHackageNames = do
    path <- hackageNamesPath
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            r <- try (TIO.readFile path)
            pure $ case r of
                Left (_ :: SomeException) -> Nothing
                Right t ->
                    Just
                        ( S.fromList
                            (filter (not . T.null) (map T.strip (T.lines t)))
                        )

hackageInfoFor :: [Text] -> IO HackageInfo
hackageInfoFor candidates = do
    mNames <- loadHackageNames
    case mNames of
        Nothing -> pure (HackageInfo False [] [])
        Just names -> do
            let known = concatMap (canonical names) candidates
            HackageInfo True known <$> hackageFactsFor known
  where
    canonical names c = case [n | n <- S.toAscList names, eqIgnoreCase n c] of
        (n : _) -> [n]
        [] -> []
    eqIgnoreCase a b = T.toLower a == T.toLower b

-- --- package facts ---------------------------------------------------------

hackageFactsPath :: IO FilePath
hackageFactsPath =
    fromMaybe ("data" </> "hackage-facts.tsv")
        <$> lookupEnv "SABELA_HACKAGE_FACTS"

{- | The facts cache, read once per revision of the file. It is several
megabytes, so re-reading it per query would cost more than the query.
-}
factsCache :: IORef (Maybe (FilePath, UTCTime, M.Map Text PkgFacts))
factsCache = unsafePerformIO (newIORef Nothing)
{-# NOINLINE factsCache #-}

loadHackageFacts :: IO (M.Map Text PkgFacts)
loadHackageFacts = do
    path <- hackageFactsPath
    exists <- doesFileExist path
    if not exists
        then pure M.empty
        else do
            stamp <- try (getModificationTime path)
            case stamp of
                Left (_ :: SomeException) -> pure M.empty
                Right t -> cachedOrRead path t

cachedOrRead :: FilePath -> UTCTime -> IO (M.Map Text PkgFacts)
cachedOrRead path stamp = do
    cached <- readIORef factsCache
    case cached of
        Just (p, t, m) | p == path && t == stamp -> pure m
        _ -> do
            r <- try (TIO.readFile path)
            case r of
                Left (_ :: SomeException) -> pure M.empty
                Right txt -> do
                    let m = M.fromList (mapMaybe parseFactsRow (T.lines txt))
                    atomicModifyIORef'
                        factsCache
                        (const (Just (path, stamp, m), ()))
                    pure m

-- | What the index states about each named package it holds.
hackageFactsFor :: [Text] -> IO [(Text, PkgFacts)]
hackageFactsFor names = do
    facts <- loadHackageFacts
    pure [(n, f) | n <- names, Just f <- [M.lookup n facts]]

{- | Fold more of the index into what a request holds: the packages that expose
a module, or the facts behind names another source produced.
-}
withHackageFacts :: [Text] -> [(Text, PkgFacts)] -> HackageInfo -> HackageInfo
withHackageFacts names extra hk =
    hk
        { hiKnown = hiKnown hk ++ [n | n <- names, n `notElem` hiKnown hk]
        , hiFacts = hiFacts hk ++ [f | f <- extra, fst f `notElem` held]
        }
  where
    held = map fst (hiFacts hk)

-- | What the index states about the packages exposing a module.
withModuleOwners :: Text -> HackageInfo -> IO HackageInfo
withModuleOwners m hk = do
    owners <- hackageModuleOwners m
    pure (withHackageFacts (map fst owners) owners hk)

-- | What the index states about packages a lexical name match produced.
withFactsFor :: [Text] -> HackageInfo -> IO HackageInfo
withFactsFor names hk = do
    facts <- hackageFactsFor names
    pure (withHackageFacts [] facts hk)

{- | The packages that expose a module, for a name the session cannot resolve
because nothing exposing it is installed.
-}
hackageModuleOwners :: Text -> IO [(Text, PkgFacts)]
hackageModuleOwners m
    | T.null (T.strip m) = pure []
    | otherwise = do
        facts <- loadHackageFacts
        pure [(n, f) | (n, f) <- M.toAscList facts, T.strip m `elem` pfModules f]

hackageMatching :: Int -> [Text] -> IO [Text]
hackageMatching cap tokens = do
    mNames <- loadHackageNames
    pure $ case mNames of
        Nothing -> []
        Just names ->
            take
                cap
                [n | n <- S.toAscList names, any (`T.isInfixOf` T.toLower n) usable]
  where
    usable = [T.toLower t | t <- tokens, T.length t >= 3]
