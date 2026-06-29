{-# LANGUAGE OverloadedStrings #-}

{- | The Capability fold's literals: it carries the six outcome-only tasks, and
the redesigned image task is a value-based PNG-decode (no codec-specific type in
its check), so it tests discovery rather than type coercion.
-}
module Test.CapabilityCorpusSpec (spec) where

import Data.List (find, nub)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.Corpus.Capability (capabilityTasks)
import Eval.Task (Task (..), taskTest)

findTaskIn :: Text -> Maybe Task
findTaskIn tid = find ((== tid) . taskId) capabilityTasks

spec :: Spec
spec = describe "Eval.Corpus.Capability" $ do
    it "has six tasks with unique ids" $ do
        length capabilityTasks `shouldBe` 6
        let allIds = map taskId capabilityTasks
        nub allIds `shouldBe` allIds

    describe "imageInfo (redesigned, value-based PNG decode)" $ do
        let task = findTaskIn "imageInfo"

        it "replaces the old hand-rollable shrinkInfo task" $ do
            isNothing (findTaskIn "shrinkInfo") `shouldBe` True
            (taskId <$> task) `shouldBe` Just "imageInfo"

        it "checks a plain tuple value, not a codec pixel type" $ do
            let check = task >>= taskTest
            check `shouldBe` Just "imageInfo == ((3, 3), (100, 100, 80))"
            (("PixelRGB8" `T.isInfixOf`) <$> check) `shouldBe` Just False

        it "supplies a base64-encoded picture in the prompt and names no library" $ do
            let prompt = taskPrompt <$> task
            (("base64" `T.isInfixOf`) <$> prompt) `shouldBe` Just True
            -- the embedded PNG signature, base64-encoded, starts with iVBOR
            (("iVBOR" `T.isInfixOf`) <$> prompt) `shouldBe` Just True
            (("JuicyPixels" `T.isInfixOf`) <$> prompt) `shouldBe` Just False
            (("PNG" `T.isInfixOf`) <$> prompt) `shouldBe` Just False
