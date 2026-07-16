{-# LANGUAGE OverloadedStrings #-}

{- | The product system prompt after unification: it draws its working rules
from the shared core, generates its tool-surface block from the real catalogue
(so no phantom tool names), and no longer ships the static API card.
-}
module Test.PromptUnifySpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (toolWireName)
import Sabela.AI.Capabilities.Tools (chatToolSpecs)
import Sabela.AI.Orchestrator.Prompt (systemPrompt)
import Sabela.AI.PromptCore (sharedPromptCore, toolSurfaceBlock)
import Sabela.AI.ReferenceCard (apiReferenceCard)
import Sabela.LLM.Tool (ToolSpec (..))

spec :: Spec
spec = describe "product systemPrompt (unified)" $ do
    it "embeds the shared prompt core" $
        (sharedPromptCore `T.isInfixOf` systemPrompt) `shouldBe` True

    it "no longer ships the static API reference card" $
        (apiReferenceCard `T.isInfixOf` systemPrompt) `shouldBe` False

    it "names the Sabela builtins and steers detail lookup to search, not prose" $ do
        let has s = (s `T.isInfixOf` systemPrompt) `shouldBe` True
        mapM_
            has
            [ "displayHtml"
            , "slider"
            , "import Sabela.Notebook"
            , "describe_function"
            ]

    it "names no phantom ghci_query tool" $
        ("ghci_query" `T.isInfixOf` systemPrompt) `shouldBe` False

    it "generates the tool-surface block from the real catalogue" $ do
        ("## Tools available" `T.isInfixOf` systemPrompt) `shouldBe` True
        let names = map (toolWireName . toolName) chatToolSpecs
        [n | n <- names, not (n `T.isInfixOf` systemPrompt)] `shouldBe` []

    describe "toolSurfaceBlock" $
        it "lists exactly the catalogue's wire names, in order" $
            surfaceNames (toolSurfaceBlock chatToolSpecs)
                `shouldBe` map (toolWireName . toolName) chatToolSpecs

-- | The tool names a generated surface block lists, one per @- name:@ line.
surfaceNames :: Text -> [Text]
surfaceNames block =
    [ T.strip (T.takeWhile (/= ':') (T.drop 2 l))
    | l <- T.lines block
    , "- " `T.isPrefixOf` l
    ]
