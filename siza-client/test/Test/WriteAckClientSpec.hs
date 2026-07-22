{-# LANGUAGE OverloadedStrings #-}

{- | Client-side write-ack reconciliation (R6.1): an @executing@ insert ack is
settled through bounded @await_idle@ follow-ups and merged so the health gate
('ownedCellOutcome') reads the settled execution, not a false red; settled
and error outcomes pass through untouched.
-}
module Test.WriteAckClientSpec (writeAckClientSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Ack (maxAwaitRounds, reconcileWrite)
import Siza.Agent.Owned (ownedCellOutcome)

executingAck :: Value
executingAck =
    object
        [ "cellId" .= (0 :: Int)
        , "status" .= ("executing" :: Text)
        , "note" .= ("landed; await_idle" :: Text)
        ]

settledReply :: Value
settledReply =
    object
        [ "waited" .= ("idle" :: Text)
        , "writes"
            .= [ object
                    [ "cellId" .= (0 :: Int)
                    , "status" .= ("completed" :: Text)
                    , "execution" .= object ["ok" .= True]
                    ]
               ]
        ]

emptyReply :: Value
emptyReply = object ["waited" .= ("timedOut" :: Text)]

{- | A fake tool caller that answers @await_idle@ from a scripted list and
counts every call it receives.
-}
scriptedCaller ::
    [Value] -> IO (ToolName -> Value -> IO (Either Text ToolOutcome), IO Int)
scriptedCaller replies = do
    ref <- newIORef (0 :: Int)
    let call AwaitIdle _ = do
            n <- readIORef ref
            modifyIORef' ref (+ 1)
            pure (Right (ToolOk (replies !! min n (length replies - 1))))
        call _ _ = pure (Left "unexpected tool")
    pure (call, readIORef ref)

writeAckClientSpec :: Spec
writeAckClientSpec = describe "client write-ack reconciliation (R6.1)" $ do
    it "settles an executing ack via await_idle and the health gate reads it" $ do
        (call, calls) <- scriptedCaller [emptyReply, settledReply]
        out <- reconcileWrite call (Right (ToolOk executingAck))
        calls `shouldReturn` 2
        let tc = ToolCall "insert_cell" (object ["source" .= ("x = 1" :: Text)])
        ownedCellOutcome tc out `shouldBe` Just (0, True)

    it "keeps the honest executing ack when the write never settles" $ do
        (call, calls) <- scriptedCaller [emptyReply]
        out <- reconcileWrite call (Right (ToolOk executingAck))
        n <- calls
        n `shouldBe` maxAwaitRounds
        out `shouldBe` Right (ToolOk executingAck)

    it "passes settled and error outcomes through untouched" $ do
        (call, calls) <- scriptedCaller [settledReply]
        let done =
                Right
                    ( ToolOk
                        ( object
                            [ "cellId" .= (1 :: Int)
                            , "status" .= ("completed" :: Text)
                            , "execution" .= object ["ok" .= True]
                            ]
                        )
                    )
        out <- reconcileWrite call done
        out `shouldBe` done
        errOut <- reconcileWrite call (Left "transport error")
        errOut `shouldBe` Left "transport error"
        calls `shouldReturn` 0
