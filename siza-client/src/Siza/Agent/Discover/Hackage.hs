{-# LANGUAGE ScopedTypeVariables #-}

module Siza.Agent.Discover.Hackage (
    loadHackageNames,
    hackageInfoFor,
    hackageMatching,
    hackageFactsFor,
    hackageModuleOwners,
    withModuleOwners,
    withFactsFor,
) where

import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sabela.AI.DataFiles (resolveDataFile)
import Sabela.AI.FactsCache (exactModuleOwners, loadHackageFacts)
import Sabela.AI.ModuleResolve (namesFragment)
import Siza.Agent.Discover.CabalFacts (PkgFacts (..))
import Siza.Agent.Discover.Types (HackageInfo (..))

{- | The names cache, or the paths that were checked for it, so an
unavailability note can state where it looked.
-}
loadHackageNames :: IO (Either [FilePath] (S.Set Text))
loadHackageNames = do
    resolved <- resolveDataFile "SABELA_HACKAGE_NAMES" "hackage-packages.txt"
    case resolved of
        Left checked -> pure (Left checked)
        Right path -> do
            r <- try (TIO.readFile path)
            pure $ case r of
                Left (_ :: SomeException) -> Left [path]
                Right t ->
                    Right
                        ( S.fromList
                            (filter (not . T.null) (map T.strip (T.lines t)))
                        )

hackageInfoFor :: [Text] -> IO HackageInfo
hackageInfoFor candidates = do
    mNames <- loadHackageNames
    case mNames of
        Left checked -> pure (HackageInfo False [] [] (map T.pack checked))
        Right names -> do
            let known = concatMap (canonical names) candidates
            facts <- hackageFactsFor known
            pure (HackageInfo True known facts [])
  where
    canonical names c = case [n | n <- S.toAscList names, eqIgnoreCase n c] of
        (n : _) -> [n]
        [] -> []
    eqIgnoreCase a b = T.toLower a == T.toLower b

-- --- package facts ---------------------------------------------------------

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
because nothing exposing it is installed. Exactly, or — where nothing matches
exactly and the fragment is distinctive enough to mean one thing — the packages
whose module names carry it as a namespace component.
-}
hackageModuleOwners :: Text -> IO [(Text, PkgFacts)]
hackageModuleOwners m
    | T.null asked = pure []
    | otherwise = do
        facts <- loadHackageFacts
        let exact = exactModuleOwners asked facts
        pure (if null exact then componentOwners asked facts else exact)
  where
    asked = T.strip m

{- | The packages a module fragment names, where it names few enough to be an
answer. A common fragment owns thousands of modules, and listing a prefix of
them states nothing about the one the caller meant.
-}
componentOwners :: Text -> M.Map Text PkgFacts -> [(Text, PkgFacts)]
componentOwners asked facts
    | T.length asked < minFragment = []
    | length owners > maxFragmentOwners = []
    | otherwise = owners
  where
    owners =
        take
            (maxFragmentOwners + 1)
            [(n, f) | (n, f) <- M.toAscList facts, any (namesFragment asked) (pfModules f)]

{- | Below this a fragment is too common to distinguish anything, matching the
bound 'hackageMatching' already holds itself to.
-}
minFragment :: Int
minFragment = 3

{- | Above this the fragment named a namespace, not a module. The count is the
answer then, and it is stated rather than a prefix of the matches shown.
-}
maxFragmentOwners :: Int
maxFragmentOwners = 3

hackageMatching :: Int -> [Text] -> IO [Text]
hackageMatching cap tokens = do
    mNames <- loadHackageNames
    pure $ case mNames of
        Left _ -> []
        Right names ->
            take
                cap
                [n | n <- S.toAscList names, any (`T.isInfixOf` T.toLower n) usable]
  where
    usable = [T.toLower t | t <- tokens, T.length t >= 3]
