module Test.ProvenanceSpec (
    provenanceSpec,
    chainSpec,
    retroSpec,
) where

import Control.Monad (forM_, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import Sabela.AI.Capabilities.ToolName (
    ToolName (ExecuteCell, ReplaceCellSource),
 )
import Sabela.AI.Types (ToolOutcome (ToolOk))
import Siza.Provenance (
    Actor (Agent, Human, InBrowserChat),
    SessionEvent (..),
    appendEvent,
    chainEvents,
    eventHash,
    recordEvent,
    sessionLogPath,
    verifyChain,
 )
import Siza.Retro (
    RetroMetrics (..),
    computeMetrics,
    decodeSession,
 )
import System.Directory (
    doesFileExist,
    getTemporaryDirectory,
    removeFile,
 )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Fixtures (retroSession, sampleEvent)
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Provenance: the append-only JSONL SessionEvent log
-- ---------------------------------------------------------------------------

provenanceSpec :: Spec
provenanceSpec = describe "provenance (client-seam SessionEvent log)" $ do
    it "round-trips a SessionEvent through JSON, preserving the typed fields" $
        case A.decode (A.encode sampleEvent) of
            Nothing -> expectationFailure "SessionEvent failed to decode"
            Just ev -> do
                seSession ev `shouldBe` seSession sampleEvent
                seGen ev `shouldBe` seGen sampleEvent
                seCall ev `shouldBe` seCall sampleEvent
                seActor ev `shouldBe` seActor sampleEvent
                sePreflight ev `shouldBe` sePreflight sampleEvent
                seOutcome ev `shouldBe` seOutcome sampleEvent
                seKernelBefore ev `shouldBe` seKernelBefore sampleEvent
                ev `shouldBe` sampleEvent

    it "tags each actor distinctly on the wire" $
        forM_ [Agent, Human, InBrowserChat] $ \actor -> do
            let ev = sampleEvent{seActor = actor}
            (seActor <$> A.decode (A.encode ev)) `shouldBe` Just actor

    it "carries the (session, gen) correlation key into the JSON" $ do
        let Object o = A.toJSON sampleEvent
        KM.lookup "session" o `shouldBe` Just (String "siza-host-42")
        KM.lookup "gen" o `shouldBe` Just (A.Number 7)

    it "resolves the log path under XDG_STATE_HOME/sabela/sessions" $ do
        tmp <- getTemporaryDirectory
        setEnv "XDG_STATE_HOME" tmp
        p <- sessionLogPath "demo" "siza-host-42"
        unsetEnv "XDG_STATE_HOME"
        p `shouldBe` tmp </> "sabela" </> "sessions" </> "demo" </> "siza-host-42.jsonl"

    it "appends a well-formed JSONL line that decodes back to the event" $ do
        tmp <- getTemporaryDirectory
        let path = tmp </> "siza-provenance-test.jsonl"
        exists0 <- doesFileExist path
        when exists0 (removeFile path)
        appendEvent path sampleEvent
        appendEvent
            path
            sampleEvent{seCall = ExecuteCell, seOutcome = ToolOk (object [])}
        raw <- LBS8.readFile path
        let lns = filter (not . LBS8.null) (LBS8.lines raw)
        length lns `shouldBe` 2
        (A.decode (head lns) :: Maybe SessionEvent) `shouldBe` Just sampleEvent
        fmap seCall (A.decode (lns !! 1)) `shouldBe` Just ExecuteCell
        removeFile path

    it "recordEvent never throws even when the path is unwritable" $ do
        setEnv "XDG_STATE_HOME" "/dev/null/not-a-dir"
        recordEvent sampleEvent
        unsetEnv "XDG_STATE_HOME"

-- ---------------------------------------------------------------------------
-- Hash chain (opt-in tamper-evidence)
-- ---------------------------------------------------------------------------

chainSpec :: Spec
chainSpec = describe "provenance hash chain (opt-in)" $ do
    let evs =
            [ sampleEvent
            , sampleEvent{seCall = ExecuteCell, seGen = 8}
            , sampleEvent{seCall = ExecuteCell, seGen = 9}
            ]
        chained = chainEvents evs

    it "links each record to its predecessor's hash; the first is Nothing" $ do
        map sePrev chained
            `shouldBe` [ Nothing
                       , Just (eventHash (head chained))
                       , Just (eventHash (chained !! 1))
                       ]

    it "a chained log verifies" $
        verifyChain chained `shouldBe` True

    it "the default plain (unchained) log is not a valid 1+ chain" $
        -- two unchained events both carry sePrev=Nothing, so the second
        -- record's link does not match its predecessor's hash
        verifyChain evs `shouldBe` False

    it "editing any record after chaining breaks verification" $ do
        let tampered =
                take 1 chained
                    ++ [(chained !! 1){seGen = 999}]
                    ++ drop 2 chained
        verifyChain tampered `shouldBe` False

    it "an empty or single-event chain trivially verifies" $ do
        verifyChain [] `shouldBe` True
        verifyChain (chainEvents [sampleEvent]) `shouldBe` True

-- ---------------------------------------------------------------------------
-- Retro: session metrics from the JSONL log
-- ---------------------------------------------------------------------------

retroSpec :: Spec
retroSpec = describe "retro (session metrics from the log)" $ do
    let m = computeMetrics retroSession

    it "counts the total calls" $
        rmTotal m `shouldBe` 4

    it "counts calls per tool" $ do
        lookup ReplaceCellSource (toAssoc m) `shouldBe` Just 2
        lookup ExecuteCell (toAssoc m) `shouldBe` Just 2

    it "counts the error outcomes" $
        rmErrors m `shouldBe` 2

    it "counts the security-scan hits (preflight with findings)" $
        rmScanHits m `shouldBe` 1

    it "counts the pre-flight blocks (an unvetted mutation)" $
        rmBlocks m `shouldBe` 1

    it "decodes a JSONL blob, skipping blank lines, into events" $ do
        let blob =
                LBS8.intercalate
                    "\n"
                    (map A.encode retroSession ++ ["", "   "])
        length (decodeSession blob) `shouldBe` 4

    it "metrics over the decoded JSONL match metrics over the events" $ do
        let blob = LBS8.unlines (map A.encode retroSession)
        computeMetrics (decodeSession blob) `shouldBe` m
  where
    toAssoc = rmPerTool
