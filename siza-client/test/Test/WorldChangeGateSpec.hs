{-# LANGUAGE OverloadedStrings #-}

{- | R10-T4 (R1.4): the world-change banner is legal ONLY when a dep-install or
kernel-restart landed AFTER a prior recorded discover answer in the same
session. The first search of a session never banners — a change before any
query has nothing prior to stale (the revenueTotal first-of-session false
banner). Proven over enumerated D/R event sequences, not one fixture.
-}
module Test.WorldChangeGateSpec (worldChangeGateSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (IORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.History (SearchLedger)
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Test.DiscoverFixtures (installNamesFile, textField)

-- | Two session events: a discover (distinct query per step) and a restart.
data Ev = D | R deriving (Eq, Show)

-- | A found envelope — recorded by the ledger, so it bumps the call counter.
foundEnv :: Value
foundEnv =
    object
        [ "state" .= ("found" :: Text)
        , "hits" .= [object ["name" .= ("gust" :: Text), "type" .= ("Int" :: Text)]]
        , "consulted"
            .= [object ["source" .= ("hoogle" :: Text), "status" .= ("ok" :: Text)]]
        ]

-- | Inner dispatch: discover answers found; a restart is a world event.
inner :: ToolCall -> IO (Either Text ToolOutcome)
inner tc = pure . Right . ToolOk $ case tcName tc of
    "discover" -> foundEnv
    _ -> object []

discoverCall :: Int -> ToolCall
discoverCall i =
    ToolCall "discover" (object ["query" .= ("gust" <> T.pack (show i))])

restartCall :: ToolCall
restartCall = ToolCall "kernel_restart" (object [])

-- | Run a sequence, returning each discover's worldChange banner text.
runSeq :: IORef SearchLedger -> [Ev] -> IO [Text]
runSeq ref = go 0
  where
    disp = guardDiscover ref inner
    go _ [] = pure []
    go i (D : es) = do
        r <- disp (discoverCall i)
        let banner = case r of
                Right (ToolOk v) -> textField "worldChange" v
                _ -> ""
        (banner :) <$> go (i + 1) es
    go i (R : es) = disp restartCall >> go i es

-- | Every event sequence up to a given length over {D, R}.
seqsUpTo :: Int -> [[Ev]]
seqsUpTo n = concat [replicateEvs k | k <- [1 .. n]]
  where
    replicateEvs 0 = [[]]
    replicateEvs k = [e : es | e <- [D, R], es <- replicateEvs (k - 1)]

worldChangeGateSpec :: Spec
worldChangeGateSpec =
    describe "R10-T4 world-change banner legality (R1.4)" $ do
        it "the first discover of a session never banners" $ do
            installNamesFile
            forM_ (seqsUpTo 4) $ \evs -> do
                ref <- newSearchLedger
                banners <- runSeq ref evs
                case banners of
                    (first : _) -> (evs, first) `shouldBe` (evs, "")
                    [] -> pure ()
        it "a restart before any query leaves the first search unbannered" $ do
            installNamesFile
            ref <- newSearchLedger
            banners <- runSeq ref [R, R, D]
            head banners `shouldBe` ""
        it "a restart AFTER a recorded answer banners the next search" $ do
            installNamesFile
            ref <- newSearchLedger
            banners <- runSeq ref [D, R, D]
            banners !! 1 `shouldSatisfy` (not . T.null)
        it "back-to-back searches with no world event never banner" $ do
            installNamesFile
            ref <- newSearchLedger
            banners <- runSeq ref [D, D, D]
            banners `shouldSatisfy` all T.null
