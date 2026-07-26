{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | G8: lock ownership is real and nameable. A busy verdict may cite a cell
id only for a cell the notebook actually contains; every other holder — a
dependency install, a write whose cell never committed — is named as the
operation it is. Companion to 'Test.KernelStateIntegritySpec' (C6): the same
family of bug, interpreter state diverging from what a tool claims.
-}
module Test.KernelState.HolderSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Kernel (interruptOutcome, restartOutcome)
import Sabela.AI.Capabilities.KernelHealth (runningHolder)
import Sabela.AI.KernelVocab (
    Holding (..),
    LockOwner (..),
    busyDenyJson,
    ownerLabel,
 )
import Sabela.AI.Store (AIStore (..))
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.AI.WriteRegistry (registerWrite)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (modifyNotebook)

import Test.WriteAckFixture (mkFixture)

-- | A committed code cell, as the notebook would hold it.
cell :: Int -> Cell
cell cid = Cell cid CodeCell ST.Haskell "x = 1" [] Nothing False

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

-- | Register a running write under @cid@ and ask who holds the lock.
holderFor :: [Cell] -> Int -> IO (Maybe Holding)
holderFor cells cid = do
    (app, store) <- mkFixture
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = cells})
    _ <- registerWrite (aiWriteReg store) "k" cid
    runningHolder app store

spec :: Spec
spec = describe "lock ownership (G8)" $ do
    describe "phantom-cell-zero: a cell id the notebook does not contain" $ do
        it "is never claimed as a cell holder" $ do
            h <- holderFor [] 0
            fmap hdOwner h `shouldSatisfy` \case
                Just (OwnedByCell _) -> False
                Just (OwnedByOp _) -> True
                Nothing -> False

        it "emits no cellId in the busy verdict, over every absent id" $
            forM_ [0, 1, 7, 99] $ \cid -> do
                h <- holderFor [cell 3] cid
                let v = busyDenyJson h
                field "cellId" v `shouldSatisfy` isNothing
                field "operation" v `shouldNotSatisfy` isNothing

        it "still names a holder and an elapsed clock" $ do
            h <- holderFor [] 0
            let v = busyDenyJson h
            field "elapsedMs" v `shouldNotSatisfy` isNothing
            case field "error" v of
                Just (String m) -> T.isInfixOf "cell 0" m `shouldBe` False
                _ -> expectationFailure "expected an error message"

    describe "a cell the notebook does contain" $ do
        it "is claimed as a cell holder, keeping the cellId wire shape" $ do
            h <- holderFor [cell 4] 4
            fmap hdOwner h `shouldBe` Just (OwnedByCell 4)
            let v = busyDenyJson h
            field "cellId" v `shouldBe` Just (Number 4)
            field "elapsedMs" v `shouldNotSatisfy` isNothing

    describe "ownerLabel" $ do
        it "names a cell by id and an operation by its own words" $ do
            ownerLabel (OwnedByCell 12) `shouldBe` "cell 12"
            ownerLabel (OwnedByOp "installing dependencies")
                `shouldBe` "installing dependencies"

    describe "no holder" $ do
        it "denies without inventing one" $ do
            let v = busyDenyJson Nothing
            field "cellId" v `shouldSatisfy` isNothing
            field "operation" v `shouldSatisfy` isNothing
            field "busy" v `shouldBe` Just (Bool True)

    describe "control operations are effective or honest (G8.4)" $ do
        it "false-interrupt: an uninterruptible holder is not a success" $ do
            let install = Holding (OwnedByOp "installing dependencies") 86515
                v = toolOutcomeValue (interruptOutcome (Just install))
            field "interrupted" v `shouldBe` Just (Bool False)
            field "holder" v `shouldBe` Just (String "installing dependencies")
            field "elapsedMs" v `shouldBe` Just (Number 86515)

        it "reports a real release as a success" $ do
            let v = toolOutcomeValue (interruptOutcome Nothing)
            field "interrupted" v `shouldBe` Just (Bool True)

        it "restart-into-death: a restart leaving the kernel cold fails" $ do
            let v = toolOutcomeValue (restartOutcome False)
            field "restarted" v `shouldBe` Just (Bool False)
            field "detail" v `shouldSatisfy` isJust
            -- live_test10 read `restartInitiated: true` as a working kernel.
            -- Initiation alone may never be the whole verdict again.
            case field "detail" v of
                Just (String d) -> T.isInfixOf "cold" d `shouldBe` True
                _ -> expectationFailure "expected a failure detail"

        it "a restart that comes back reports no failure detail" $ do
            let v = toolOutcomeValue (restartOutcome True)
            field "restarted" v `shouldBe` Just (Bool True)
            field "detail" v `shouldSatisfy` isNothing
