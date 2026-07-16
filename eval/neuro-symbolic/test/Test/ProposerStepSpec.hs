{-# LANGUAGE OverloadedStrings #-}

{- | The deterministic pieces of per-step rejection sampling: the K gate read from
@SIZA_SAMPLE_K@ (default 1 = off, so the default episode is unchanged) and the
cell-source extraction a write candidate is judged on. The sampling loop itself
(re-ask → verify → keep first that compiles) is exercised end-to-end by the eval
run and by 'Test.SampleVerifySpec' for 'sampleVerifyOne'.
-}
module Test.ProposerStepSpec (spec) where

import Data.Aeson (object, (.=))
import System.Environment (setEnv, unsetEnv)
import Test.Hspec

import Eval.Agent (qualifiedBaseNames, sampleK, writeSource)
import Eval.Ollama (ToolCall (..))

withK :: String -> IO a -> IO a
withK v act = setEnv "SIZA_SAMPLE_K" v *> act <* unsetEnv "SIZA_SAMPLE_K"

spec :: Spec
spec = describe "per-step rejection sampling (deterministic pieces)" $ do
    describe "sampleK (SIZA_SAMPLE_K gate)" $ do
        it "defaults to 1 (off) when unset" $ do
            unsetEnv "SIZA_SAMPLE_K"
            sampleK `shouldReturn` 1
        it "reads a set fan-out" $
            withK "5" (sampleK `shouldReturn` 5)
        it "clamps a non-positive or garbage value to 1" $ do
            withK "0" (sampleK `shouldReturn` 1)
            withK "nope" (sampleK `shouldReturn` 1)

    describe "writeSource (candidate the verifier judges)" $ do
        it "reads insert_cell source" $
            writeSource (ToolCall "insert_cell" (object ["source" .= ("x = 1" :: String)]))
                `shouldBe` Just "x = 1"
        it "reads replace_cell_source new_source" $
            writeSource
                ( ToolCall
                    "replace_cell_source"
                    (object ["cell_id" .= (1 :: Int), "new_source" .= ("y = 2" :: String)])
                )
                `shouldBe` Just "y = 2"
        it "is Nothing for a blank source (nothing to sample)" $
            writeSource (ToolCall "insert_cell" (object ["source" .= ("  " :: String)]))
                `shouldBe` Nothing
        it "is Nothing for a non-writing call" $
            writeSource (ToolCall "list_cells" (object [])) `shouldBe` Nothing

    describe "qualifiedBaseNames (grounding query from used calls)" $ do
        it "pulls the base name of a qualified call" $
            qualifiedBaseNames "x = D.col \"r\" df" `shouldBe` ["col"]
        it "ignores module-like uppercase-headed qualifiers" $
            qualifiedBaseNames "import qualified Data.DataFrame as D" `shouldBe` []
        it "collects the value-level calls the cell used" $
            qualifiedBaseNames "sum (D.columnAsList v df) + M.foo x"
                `shouldBe` ["columnAsList", "foo"]
