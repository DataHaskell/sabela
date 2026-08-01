{-# LANGUAGE OverloadedStrings #-}

module Test.WriteAckSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, threadDelay)
import Control.Concurrent.MVar (MVar)
import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import System.Timeout (timeout)

import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.State (App)
import Test.Hspec
import Test.WriteAckFixture (
    callTool,
    cellCount,
    fastRn,
    field,
    insertSrc,
    mkFixture,
    slowRn,
    textField,
    withAckEnv,
 )

ackExecuting :: IO (App, AIStore.AIStore, ReactiveNotebook, MVar (), Value)
ackExecuting = do
    (app, store) <- mkFixture
    barrier <- newEmptyMVar
    let rn = slowRn app barrier
    mv <- timeout 30000000 (insertSrc app store rn "x = 1")
    case mv of
        Nothing -> do
            expectationFailure "insert did not ack within 30s"
            error "unreachable"
        Just v -> do
            case textField "status" v of
                Just "executing" -> pure ()
                _ ->
                    expectationFailure $
                        "fixture precondition: the barrier is still held, so the\
                        \ ack must be 'executing'; got "
                            <> show v
            pure (app, store, rn, barrier, v)

spec :: Spec
spec = around_ withAckEnv $ describe "write-ack (R6.1/R6.2/R6.4)" $ do
    it "a long-running insert acks {cellId, status: executing} under the deadline" $ do
        t0 <- getMonotonicTimeNSec
        (_, _, _, barrier, v) <- ackExecuting
        t1 <- getMonotonicTimeNSec
        (t1 - t0) < 25000000000 `shouldBe` True
        textField "status" v `shouldBe` Just "executing"
        field "cellId" v `shouldBe` Just (Number 0)
        putMVar barrier ()

    it "the executing ack states the write landed and how to reconcile" $ do
        (_, _, _, barrier, v) <- ackExecuting
        let note = fromMaybe "" (textField "note" v)
        note `shouldSatisfy` T.isInfixOf "landed"
        note `shouldSatisfy` T.isInfixOf "await_idle"
        putMVar barrier ()

    it "retrying the identical write while executing does not duplicate (R6.2)" $ do
        (app, store, rn, barrier, v) <- ackExecuting
        retry <- insertSrc app store rn "x = 1"
        field "duplicate" retry `shouldBe` Just (Bool True)
        field "cellId" retry `shouldBe` field "cellId" v
        fromMaybe "" (textField "note" retry)
            `shouldSatisfy` T.isInfixOf "landed"
        cellCount app `shouldReturn` 1
        putMVar barrier ()

    it
        "a kernel call during one's own write is bounced naming cell + elapsed (R6.4)"
        $ do
            (app, store, rn, barrier, _) <- ackExecuting
            out <-
                callTool app store rn "execute_cell" (object ["cell_id" .= (0 :: Int)])
            toolOutcomeIsError out `shouldBe` True
            let v = toolOutcomeValue out
            field "busy" v `shouldBe` Just (Bool True)
            textField "cause" v `shouldBe` Just "own-write"
            field "cellId" v `shouldBe` Just (Number 0)
            isJust (field "elapsedMs" v) `shouldBe` True
            fromMaybe "" (textField "hint" v)
                `shouldSatisfy` T.isInfixOf "await_idle"
            putMVar barrier ()

    it "await_idle reconciles the settled outcome exactly once (R6.1)" $ do
        (app, store, rn, barrier, _) <- ackExecuting
        putMVar barrier ()
        threadDelay 300000
        v <- toolOutcomeValue <$> callTool app store rn "await_idle" (object [])
        case field "writes" v of
            Just (Array _) -> pure ()
            other -> expectationFailure ("no writes reconciled: " <> show other)
        let cellIds = case field "writes" v of
                Just ws -> field "cellId" <$> arrayItems ws
                Nothing -> []
        cellIds `shouldBe` [Just (Number 0)]
        v2 <- toolOutcomeValue <$> callTool app store rn "await_idle" (object [])
        field "writes" v2 `shouldBe` Nothing

    it "retrying after settle says the original landed, with its outcome" $ do
        (app, store, rn, barrier, _) <- ackExecuting
        putMVar barrier ()
        threadDelay 300000
        retry <- insertSrc app store rn "x = 1"
        field "duplicate" retry `shouldBe` Just (Bool True)
        textField "status" retry `shouldBe` Just "completed"
        isJust (field "execution" retry) `shouldBe` True
        cellCount app `shouldReturn` 1

    it "a fast insert still completes inline with its execution summary" $ do
        (app, store) <- mkFixture
        let rn = fastRn app
        v <- insertSrc app store rn "x = 1"
        textField "status" v `shouldBe` Just "completed"
        isJust (field "execution" v) `shouldBe` True
        cellCount app `shouldReturn` 1

    it "a different source is never deduped" $ do
        (app, store) <- mkFixture
        let rn = fastRn app
        _ <- insertSrc app store rn "x = 1"
        v <- insertSrc app store rn "y = 2"
        field "duplicate" v `shouldBe` Nothing
        field "cellId" v `shouldBe` Just (Number 1)
        cellCount app `shouldReturn` 2

    it "a prose insert completes at once and its retry is deduped" $ do
        (app, store) <- mkFixture
        let rn = fastRn app
        let input =
                object
                    [ "source" .= ("hello world" :: Text)
                    , "cell_type" .= ("ProseCell" :: Text)
                    ]
        v <- toolOutcomeValue <$> callTool app store rn "insert_cell" input
        textField "status" v `shouldBe` Just "completed"
        retry <- toolOutcomeValue <$> callTool app store rn "insert_cell" input
        field "duplicate" retry `shouldBe` Just (Bool True)
        cellCount app `shouldReturn` 1

arrayItems :: Value -> [Value]
arrayItems (Array xs) = foldr (:) [] xs
arrayItems _ = []
