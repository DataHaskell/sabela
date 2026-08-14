{-# LANGUAGE ScopedTypeVariables #-}

{- | The one reader for the Hackage facts file, shared by the server's index
and the discover client, so the mtime-keyed cache and the exact-ownership
lookup each have exactly one definition.
-}
module Sabela.AI.FactsCache (
    exactModuleOwners,
    loadHackageFacts,
) where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import System.Directory (getModificationTime)
import System.IO.Unsafe (unsafePerformIO)

import Sabela.AI.DataFiles (resolveDataFile)
import Sabela.AI.FactsRow (PkgFacts (..), parseFactsRow)

{- | The facts cache, read once per revision of the file. It is several
megabytes, so re-reading it per query would cost more than the query.
-}
factsCache :: IORef (Maybe (FilePath, UTCTime, M.Map Text PkgFacts))
factsCache = unsafePerformIO (newIORef Nothing)
{-# NOINLINE factsCache #-}

loadHackageFacts :: IO (M.Map Text PkgFacts)
loadHackageFacts = do
    resolved <- resolveDataFile "SABELA_HACKAGE_FACTS" "hackage-facts.tsv"
    case resolved of
        Left _ -> pure M.empty
        Right path -> do
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

-- | The packages whose public library exposes exactly the named module.
exactModuleOwners :: Text -> M.Map Text PkgFacts -> [(Text, PkgFacts)]
exactModuleOwners m facts =
    [(n, f) | (n, f) <- M.toAscList facts, asked `elem` pfModules f]
  where
    asked = T.strip m
