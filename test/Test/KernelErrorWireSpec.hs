{-# LANGUAGE OverloadedStrings #-}

module Test.KernelErrorWireSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec

import Sabela.Model (
    KernelPhase (..),
    NotebookEvent (..),
    SessionStatus (..),
 )

objOf :: Value -> KM.KeyMap Value
objOf (Object o) = o
objOf other = error ("expected object, got " <> show other)

look :: String -> Value -> Maybe Value
look k v = KM.lookup (Key.fromString k) (objOf v)

spec :: Spec
spec = do
    describe "sessionStatus wire shape (a tag, not a rendered Show)" $ do
        it "carries a machine-readable state beside the human message" $ do
            let v = toJSON (EvSessionStatus SReady)
            look "type" v `shouldBe` Just (String "sessionStatus")
            look "state" v `shouldBe` Just (String "ready")
            look "message" v `shouldBe` Just (String "ready")

        it
            "gives every status a distinct state tag, so a client never has to\
            \ match on prose"
            $ do
                let tagOf s = look "state" (toJSON (EvSessionStatus s))
                tagOf SReset `shouldBe` Just (String "reset")
                tagOf SCrashed `shouldBe` Just (String "crashed")
                tagOf SStarting `shouldBe` Just (String "starting")
                tagOf SDepsUpToDate `shouldBe` Just (String "depsUpToDate")
                tagOf (SUpdateDeps ["text"]) `shouldBe` Just (String "installing")

        it
            "names the packages as data: the client used to recover them by\
            \ stripping an 'installing: ' prefix off the message"
            $ do
                let v = toJSON (EvSessionStatus (SUpdateDeps ["text", "vector"]))
                look "deps" v `shouldBe` Just (toJSON ["text" :: String, "vector"])
                look "message" v `shouldBe` Just (String "installing: text, vector")

        it "keeps deps present but empty on statuses that install nothing" $
            look "deps" (toJSON (EvSessionStatus SReady))
                `shouldBe` Just (toJSON ([] :: [String]))

    describe "kernelError wire shape (its own channel)" $ do
        let ev = EvKernelError KpBuildTimeout "gave up after 1800s" [2, 5]
            v = toJSON ev

        it "is a separate event type, not a sessionStatus" $
            look "type" v `shouldBe` Just (String "kernelError")

        it "carries the failing phase, the message and the cells to blame" $ do
            look "phase" v `shouldBe` Just (String "buildTimeout")
            look "message" v `shouldBe` Just (String "gave up after 1800s")
            look "cellIds" v `shouldBe` Just (toJSON [2 :: Int, 5])

        it
            "distinguishes a build that ran out of time from one that failed:\
            \ only the first is worth retrying at a longer budget"
            $ do
                let phaseOf p = look "phase" (toJSON (EvKernelError p "m" []))
                phaseOf KpBuildTimeout `shouldBe` Just (String "buildTimeout")
                phaseOf KpBuildFailed `shouldBe` Just (String "buildFailed")
                phaseOf KpPreludeFailed `shouldBe` Just (String "preludeFailed")
                phaseOf KpCrashed `shouldBe` Just (String "crashed")

        it "emits an empty cellIds array when nothing can be blamed" $
            look "cellIds" (toJSON (EvKernelError KpCrashed "died" []))
                `shouldBe` Just (toJSON ([] :: [Int]))
