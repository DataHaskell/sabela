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

    {- The prompt POINTS at Sabela's library and the search tools; it does
    not enumerate the API. Naming the entry points taught the answer to the
    probes that grade this surface — see 'Test.PromptBuiltinsSpec'. -}
    it "points at the library and the search tools, naming no entry point" $ do
        let has s = (s `T.isInfixOf` systemPrompt) `shouldBe` True
            lacks s = (s `T.isInfixOf` systemPrompt) `shouldBe` False
        mapM_ has ["Sabela.Notebook", "describe_function"]
        -- The DRAWING entry points, which the sine/animate probes grade.
        -- (`displayHtml` still appears in this surface's dataframe cookbook,
        -- which is the orchestrator's own choice, not the shared core's.)
        mapM_ lacks ["displayPicture", "lineChart", "animateWith"]

    it "names no phantom ghci_query tool" $
        ("ghci_query" `T.isInfixOf` systemPrompt) `shouldBe` False

    it "advertises one try interface and no legacy evaluation modes" $ do
        ("try" `T.isInfixOf` systemPrompt) `shouldBe` True
        ("scratchpad" `T.isInfixOf` systemPrompt) `shouldBe` False
        ("eval_live" `T.isInfixOf` systemPrompt) `shouldBe` False

    it "distinguishes durable cell effects from the narrower try grammar" $ do
        ("same rules in cells AND try" `T.isInfixOf` systemPrompt) `shouldBe` False
        systemPrompt `shouldSatisfy` T.isInfixOf "at most one final expression"
        systemPrompt `shouldSatisfy` T.isInfixOf "GHCi meta-commands"
        systemPrompt
            `shouldSatisfy` T.isInfixOf "compile-time escapes (including TH and FFI)"
        systemPrompt `shouldSatisfy` T.isInfixOf "unrestricted IO"
        systemPrompt `shouldSatisfy` T.isInfixOf "owned effects in a notebook cell"

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
