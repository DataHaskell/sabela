{-# LANGUAGE OverloadedStrings #-}

{- | Models re-confirm the literal same check many times per episode, each
costing a scratch cell and a gate build. The memo answers a repeat under a
seal proving nothing changed, and only a pass is ever memoised.
-}
module Test.VerifyMemoSpec (verifyMemoSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.GrammarCards (GrammarMode (..))
import Siza.Agent.Stack (Surface (..), newSessionFor)
import Siza.Agent.Stack.Call (crOutcome, runToolCall)
import Siza.Agent.VerifyMemo (
    currentSeal,
    memoHit,
    memoRecord,
    newVerifyMemo,
    verifyCheckOf,
 )

cellsPayload :: Text -> Value
cellsPayload h =
    object ["cells" .= [object ["id" .= (3 :: Int), "hash" .= h]]]

statusPayload :: Int -> Value
statusPayload g = object ["ksGen" .= g, "ebGeneration" .= (99 :: Int)]

passPayload :: Value
passPayload =
    object ["verdict" .= ("pass" :: Text), "check" .= ("x == 1" :: Text)]

-- | A dispatch serving canned notebook state, counting verify dispatches.
fakeDispatch ::
    Text -> Int -> IO (ToolCall -> IO (Either Text ToolOutcome), IO Int)
fakeDispatch h g = do
    verifies <- newIORef (0 :: Int)
    let disp call = case tcName call of
            "list_cells" -> pure (Right (ToolOk (cellsPayload h)))
            "kernel_status" -> pure (Right (ToolOk (statusPayload g)))
            "verify" -> do
                modifyIORef' verifies (+ 1)
                pure (Right (ToolOk passPayload))
            _ -> pure (Left "unexpected tool")
    pure (disp, readIORef verifies)

verifyCall :: ToolCall
verifyCall = ToolCall "verify" (object ["check" .= ("x == 1" :: Text)])

unchangedOf :: Either Text ToolOutcome -> Maybe Value
unchangedOf (Right (ToolOk (Object o))) = KM.lookup (K.fromText "unchanged") o
unchangedOf _ = Nothing

verifyMemoSpec :: Spec
verifyMemoSpec = describe "the repeated-verify memo" $ do
    it "reads the check off a verify call and nothing else" $ do
        verifyCheckOf verifyCall `shouldBe` Just "x == 1"
        verifyCheckOf (ToolCall "list_cells" (object [])) `shouldBe` Nothing
        verifyCheckOf (ToolCall "verify" (object [])) `shouldBe` Nothing

    it "answers a repeat under an identical seal, marked as unchanged" $ do
        (disp, _) <- fakeDispatch "h1" 5
        memo <- newVerifyMemo
        Just seal <- currentSeal disp
        memoRecord memo "x == 1" seal (Right (ToolOk passPayload))
        hit <- memoHit memo "x == 1" seal
        hit `shouldSatisfy` isJust

    it "misses when a cell hash changed" $ do
        (disp1, _) <- fakeDispatch "h1" 5
        (disp2, _) <- fakeDispatch "h2" 5
        memo <- newVerifyMemo
        Just s1 <- currentSeal disp1
        Just s2 <- currentSeal disp2
        memoRecord memo "x == 1" s1 (Right (ToolOk passPayload))
        memoHit memo "x == 1" s2 `shouldReturn` Nothing

    it "misses after a kernel restart" $ do
        (disp1, _) <- fakeDispatch "h1" 5
        (disp2, _) <- fakeDispatch "h1" 6
        memo <- newVerifyMemo
        Just s1 <- currentSeal disp1
        Just s2 <- currentSeal disp2
        memoRecord memo "x == 1" s1 (Right (ToolOk passPayload))
        memoHit memo "x == 1" s2 `shouldReturn` Nothing

    it "never memoises anything but a pass" $ do
        (disp, _) <- fakeDispatch "h1" 5
        memo <- newVerifyMemo
        Just seal <- currentSeal disp
        let failPayload = object ["verdict" .= ("fail" :: Text)]
        memoRecord memo "x == 1" seal (Right (ToolErr failPayload))
        memoRecord memo "x == 1" seal (Right (ToolOk failPayload))
        memoHit memo "x == 1" seal `shouldReturn` Nothing

    it "the stack runs the scratch cycle once for two identical verifies" $ do
        ss <- newSessionFor McpSurface GrammarOff ""
        (disp, verifies) <- fakeDispatch "h1" 5
        r1 <- runToolCall ss disp verifyCall
        r2 <- runToolCall ss disp verifyCall
        verifies `shouldReturn` 1
        unchangedOf (crOutcome r1) `shouldBe` Nothing
        unchangedOf (crOutcome r2) `shouldSatisfy` isJust
