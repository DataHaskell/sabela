{-# LANGUAGE OverloadedStrings #-}

{- | Phase-1 proof: the neutral kernel represents an Ollama turn. Pins
'turnToCompletion' — content/tool-call mapping, id synthesis, and the derived
stop condition.
-}
module Test.ProviderMapSpec (spec) where

import Data.Aeson (object, (.=))
import Test.Hspec

import Eval.Ollama (Turn (..))
import qualified Eval.Ollama as O
import Eval.Provider (turnToCompletion)
import Sabela.Ids (ToolCallId (..))
import Sabela.LLM.Completion (Completion (..), StopCondition (..))
import Sabela.LLM.Message (ContentPart (..), ToolCall (..))

spec :: Spec
spec = describe "turnToCompletion (Ollama Turn -> neutral Completion)" $ do
    it "maps plain content with no tool calls to one text part, Done" $ do
        let c = turnToCompletion (Turn (object []) "hello" [])
        compStop c `shouldBe` Done
        compParts c `shouldBe` [TextPart "hello"]

    it "maps a tool call to a ToolCallPart, WantsTools, synthesising an id" $ do
        let input = object ["source" .= ("x = 1" :: String)]
            c = turnToCompletion (Turn (object []) "" [O.ToolCall "insert_cell" input])
        compStop c `shouldBe` WantsTools
        compParts c
            `shouldBe` [ ToolCallPart
                            ToolCall
                                { tcId = ToolCallId "ollama-0"
                                , tcName = "insert_cell"
                                , tcInput = input
                                }
                       ]

    it "drops empty content but keeps the calls" $ do
        let c = turnToCompletion (Turn (object []) "" [O.ToolCall "list_cells" (object [])])
        length (compParts c) `shouldBe` 1

    it "numbers multiple calls positionally" $ do
        let c =
                turnToCompletion
                    ( Turn
                        (object [])
                        ""
                        [O.ToolCall "list_cells" (object []), O.ToolCall "read_cell" (object [])]
                    )
            ids = [tcId tc | ToolCallPart tc <- compParts c]
        ids `shouldBe` [ToolCallId "ollama-0", ToolCallId "ollama-1"]

    it "contributes no token usage" $
        compUsage (turnToCompletion (Turn (object []) "hi" [])) `shouldBe` mempty
