{-# LANGUAGE OverloadedStrings #-}

module Test.SystemPromptSpec (systemPromptSpec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Loop (systemPrompt)
import Siza.Agent.Stack (Surface (..))
import Siza.Agent.Tools (catalogueFor, toolGroups, toolSurfacePrompt)
import Siza.Agent.Tools.Catalogue (baseCatalogue)

systemPromptSpec :: Spec
systemPromptSpec = describe "systemPrompt" $ do
    it "explains that a write only commits code that compiles" $
        systemPrompt `shouldSatisfy` T.isInfixOf "only commit"

    it "says the compiler's diagnostic comes back on rejection, not a bare failure" $
        systemPrompt `shouldSatisfy` T.isInfixOf "diagnostic"

    it "never names a specific dataset, package, or task from a benchmark scenario" $
        mapM_
            (\leak -> systemPrompt `shouldNotSatisfy` T.isInfixOf leak)
            ["wine", "csv", "summarize", "optics", "vectors"]

    toolCopySpec

{- | C1-21d: one request carried two copies of the catalogue, the prose one
truncated to a first sentence, and the sentences it cut were the load-bearing
ones. The prompt now names tools and describes none.
-}
toolCopySpec :: Spec
toolCopySpec = describe "the prompt's copy of the tool surface (C1-21d)" $ do
    it "carries no partial copy of any tool description" $
        [ n
        | (n, d) <- catalogueEntries
        , distinctivePrefix d `T.isInfixOf` systemPrompt
        , not (d `T.isInfixOf` systemPrompt)
        ]
            `shouldBe` []

    it "names every tool both surfaces offer, and no tool neither offers" $
        promptToolNames `shouldMatchList` sharedToolNames

    it "names nothing this surface will not answer" $
        [n | n <- promptToolNames, n `notElem` chatToolNames] `shouldBe` []

    it "gives every group of tools at least one worked example" $
        [ label
        | (label, names) <- toolGroups
        , not (any (`T.isInfixOf` examplesSection) names)
        ]
            `shouldBe` []

{- | Enough of a description to identify it, and more than any honest index
entry would repeat.
-}
distinctivePrefix :: Text -> Text
distinctivePrefix = T.take 40

catalogueEntries :: [(Text, Text)]
catalogueEntries =
    [ (n, d)
    | Object o <- baseCatalogue
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    , Just (String d) <- [KM.lookup "description" f]
    ]

-- | The tools named in the prompt's own text, read back out of it.
promptToolNames :: [Text]
promptToolNames =
    [ T.strip n
    | l <- T.lines toolSurfacePrompt
    , Just rest <- [T.stripPrefix "* " l]
    , (_, listed) <- [T.breakOn ": " rest]
    , n <- T.splitOn ", " (T.drop 2 listed)
    , not (T.null (T.strip n))
    ]

sharedToolNames :: [Text]
sharedToolNames = [n | n <- offeredOn McpSurface, n `elem` chatToolNames]

chatToolNames :: [Text]
chatToolNames = offeredOn ChatSurface

offeredOn :: Surface -> [Text]
offeredOn s =
    [ n
    | Object o <- catalogueFor s
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    ]

examplesSection :: Text
examplesSection = snd (T.breakOn "Examples:" systemPrompt)
