{-# LANGUAGE OverloadedStrings #-}

{- | The R6.2 general invariant, over GENERATED lost-response retry schedules:
for every schedule of byte-identical retries of ONE write — some while the
write is still executing, some after it settles — every prefix of the
schedule leaves exactly one cell in the notebook, and every retry response
states the original landed. Nothing here reproduces a bench task; the
schedule space is enumerated mechanically.
-}
module Test.WriteAckRetrySpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, threadDelay)
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Test.Hspec

import Test.WriteAckFixture (
    cellCount,
    field,
    insertSrc,
    mkFixture,
    slowRn,
    textField,
    withAckEnv,
 )

-- | The generated schedule space: retries before x after the write settles.
schedules :: [(Int, Int)]
schedules = [(before, after) | before <- [0 .. 2], after <- [0 .. 2]]

spec :: Spec
spec = around_ withAckEnv $
    describe "write-ack retry schedules (R6.2 at-most-once)" $
        it "every prefix of every generated schedule yields exactly one cell" $
            forM_ schedules $ \(nBefore, nAfter) -> do
                (app, store) <- mkFixture
                barrier <- newEmptyMVar
                let rn = slowRn app barrier
                v0 <- insertSrc app store rn "x = 1"
                let cid0 = field "cellId" v0
                cid0 `shouldBe` Just (Number 0)
                cellCount app `shouldReturn` 1
                forM_ [1 .. nBefore] $ \_ -> do
                    r <- insertSrc app store rn "x = 1"
                    field "duplicate" r `shouldBe` Just (Bool True)
                    field "cellId" r `shouldBe` cid0
                    fromMaybe "" (textField "note" r)
                        `shouldSatisfy` T.isInfixOf "landed"
                    cellCount app `shouldReturn` 1
                putMVar barrier ()
                threadDelay 300000
                forM_ [1 .. nAfter] $ \_ -> do
                    r <- insertSrc app store rn "x = 1"
                    field "duplicate" r `shouldBe` Just (Bool True)
                    field "cellId" r `shouldBe` cid0
                    textField "status" r `shouldBe` Just "completed"
                    fromMaybe "" (textField "note" r)
                        `shouldSatisfy` T.isInfixOf "landed"
                    cellCount app `shouldReturn` 1
                cellCount app `shouldReturn` 1
