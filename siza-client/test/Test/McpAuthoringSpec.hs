{-# LANGUAGE OverloadedStrings #-}

{- | What an agent driving through MCP can author. The surface could not create
a prose cell, place a cell anywhere but the end, propose an edit for a human to
accept, or guard a write against a concurrent one, so half of a notebook that
is half prose was unreachable and every edit raced the browser.

Tools are added to the MCP surface alone: the chat surface serves weak models,
where tool availability drives routing, and it does not need them.
-}
module Test.McpAuthoringSpec (mcpAuthoringSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Stack (Surface (..))
import Siza.Agent.ToolRoute (Route (..), routeCallWith)
import Siza.Agent.Tools (catalogueFor, offeredArgKeys)

names :: Surface -> [Text]
names surface =
    [ n
    | Object o <- catalogueFor surface
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    ]

-- | The property names a tool advertises.
argsOf :: Surface -> Text -> [Text]
argsOf surface tool =
    case lookup tool (offeredArgKeysFor surface) of
        Just (props, _) -> props
        Nothing -> []
  where
    offeredArgKeysFor _ = offeredArgKeys

-- | Whether a call with these arguments is accepted rather than bounced.
accepts :: Text -> Value -> Bool
accepts tool args = case routeCallWith offeredArgKeys (ToolCall tool args) of
    RouteBadArgs _ -> False
    RouteUnknown _ -> False
    _ -> True

mcpAuthoringSpec :: Spec
mcpAuthoringSpec = describe "authoring a notebook through the MCP surface" $ do
    describe "a cell can be a prose cell, and can go somewhere" $ do
        it "insert_cell offers the cell type" $
            argsOf McpSurface "insert_cell" `shouldSatisfy` elem "cell_type"

        it "insert_cell offers the language" $
            argsOf McpSurface "insert_cell" `shouldSatisfy` elem "language"

        it "insert_cell offers a position" $
            argsOf McpSurface "insert_cell" `shouldSatisfy` elem "after_cell_id"

        it "accepts a positioned prose insert" $
            accepts
                "insert_cell"
                ( object
                    [ "source" .= ("## A heading" :: Text)
                    , "cell_type" .= ("ProseCell" :: Text)
                    , "after_cell_id" .= (12 :: Int)
                    ]
                )
                `shouldBe` True

    describe "a write can be guarded against a concurrent one" $ do
        it "replace_cell_source offers the expected hash" $
            argsOf McpSurface "replace_cell_source" `shouldSatisfy` elem "expected_hash"

        it "accepts a guarded replace" $
            accepts
                "replace_cell_source"
                ( object
                    [ "cell_id" .= (3 :: Int)
                    , "new_source" .= ("x = 1" :: Text)
                    , "expected_hash" .= ("p123" :: Text)
                    ]
                )
                `shouldBe` True

    describe "the editing tools the surface was missing" $ do
        it "offers propose_edit, for a cell whose owner should accept it" $
            names McpSurface `shouldSatisfy` elem "propose_edit"

        it "offers export_notebook, so a notebook is read in one call" $
            names McpSurface `shouldSatisfy` elem "export_notebook"

        it "routes both rather than bouncing them" $ do
            accepts
                "propose_edit"
                (object ["cell_id" .= (2 :: Int), "new_source" .= ("y = 2" :: Text)])
                `shouldBe` True
            accepts "export_notebook" (object []) `shouldBe` True

    describe "the chat surface is left alone" $ do
        it "does not serve the editing tools to weak models" $ do
            names ChatSurface `shouldNotSatisfy` elem "propose_edit"
            names ChatSurface `shouldNotSatisfy` elem "export_notebook"

        it "still serves the tools it always did" $
            mapM_
                (\n -> names ChatSurface `shouldSatisfy` elem n)
                ["insert_cell", "replace_cell_source", "read_cell", "list_cells"]
