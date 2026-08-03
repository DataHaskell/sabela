{-# LANGUAGE OverloadedStrings #-}

{- | Retrieval quality over the queries models actually issued.

The corpus is mined from @docs\/discover\/live@ and the chat transcripts; the
fixture records every Hoogle sub-query the probe plan raised while answering it.
Replaying the fixture keeps this offline and fast enough to gate a commit, which
is the point: the ladder this replaced could be changed in a way that silently
broke every prose query without a single test noticing.

The fixture is a recording of the real Hoogle DB, keyed by sub-query rather
than by user query, so a new probe shape shows up as a fixture miss instead of
being silently rewarded with no results. Re-record it whenever the probe plan
changes; a recorder executable is still owed (see the pending note below).
-}
module Test.DiscoveryBenchSpec (spec) where

import Control.Monad (forM_, unless)
import Data.Aeson (
    FromJSON (..),
    eitherDecodeFileStrict,
    withObject,
    (.:),
    (.:?),
 )
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.Search.Gather (searchNeed)
import Sabela.AI.Search.Need (parseNeed)
import Sabela.AI.Search.Row (grounded)

data Task = Task
    { taskQuery :: Text
    , taskGoldPackage :: Maybe Text
    , taskGoldSymbol :: Maybe Text
    , taskKnownGap :: Maybe Text
    }

instance FromJSON Task where
    parseJSON = withObject "task" $ \o ->
        Task
            <$> o .: "query"
            <*> o .:? "goldPackage"
            <*> o .:? "goldSymbol"
            <*> o .:? "knownGap"

newtype Row = Row {unRow :: HoogleHit}

instance FromJSON Row where
    parseJSON = withObject "row" $ \o ->
        fmap Row $
            HoogleHit
                <$> o .: "n"
                <*> o .: "p"
                <*> o .: "m"
                <*> o .: "t"
                <*> o .: "d"

corpusPath, fixturePath :: FilePath
corpusPath = "data/eval/discovery-corpus.json"
fixturePath = "data/eval/hoogle-fixture.json"

spec :: Spec
spec = describe "discovery benchmark (replayed)" $ do
    loaded <- runIO (eitherDecodeFileStrict corpusPath)
    recorded <- runIO (eitherDecodeFileStrict fixturePath)
    case (loaded, recorded) of
        (Right tasks, Right fixture) -> benchSpec tasks (Map.map (map unRow) fixture)
        (l, f) ->
            it "corpus and fixture load" $
                expectationFailure (either id (const "") l ++ either id (const "") f)

benchSpec :: [Task] -> Map Text [HoogleHit] -> Spec
benchSpec tasks fixture = do
    -- A fixture miss must fail rather than read as "nothing exists"; otherwise
    -- the recording silently reproduces the very bug it exists to catch.
    it "the fixture covers every sub-query the plan raises" $ do
        missed <- newIORef []
        forM_ tasks (runTask missed fixture)
        leftovers <- readIORef missed
        unless (null leftovers) $
            expectationFailure
                ("not in fixture (re-record): " ++ show (take 8 leftovers))

    -- The guardrail. One assertion covering every query, present and future: an
    -- answer that shares no term with the question is not an answer.
    it "every returned hit is grounded in its query" $
        forM_ tasks $ \t -> do
            missed <- newIORef []
            hits <- runTask missed fixture t
            let need = parseNeed Set.empty (taskQuery t)
                ungrounded = filter (not . grounded need) hits
            unless (null ungrounded) $
                expectationFailure
                    ( T.unpack (taskQuery t)
                        ++ " -> ungrounded: "
                        ++ show (map hhName ungrounded)
                    )

    describe "rank-1 regressions" $
        forM_ (filter hasGold tasks) $ \t ->
            it (T.unpack (taskQuery t)) $ do
                missed <- newIORef []
                hits <- runTask missed fixture t
                case (taskKnownGap t, hits) of
                    (Just why, _) -> pendingWith (T.unpack why)
                    (Nothing, []) -> expectationFailure "no hits"
                    (Nothing, h : _) -> do
                        expectPrefix "package" (taskGoldPackage t) (hhPackage h)
                        expectPrefix "symbol" (taskGoldSymbol t) (hhName h)
  where
    hasGold t = isJust (taskGoldSymbol t) || isJust (taskGoldPackage t)

expectPrefix :: String -> Maybe Text -> Text -> Expectation
expectPrefix what mgold actual = forM_ mgold $ \gold ->
    unless (T.null gold || T.toLower gold `T.isPrefixOf` T.toLower actual) $
        expectationFailure
            (what ++ ": expected " ++ show gold ++ ", got " ++ show actual)

runTask :: IORef [Text] -> Map Text [HoogleHit] -> Task -> IO [HoogleHit]
runTask missed fixture t = searchNeed run 5 (parseNeed Set.empty (taskQuery t))
  where
    run _ q = case Map.lookup q fixture of
        Just hs -> pure hs
        Nothing -> modifyIORef' missed (q :) >> pure []
