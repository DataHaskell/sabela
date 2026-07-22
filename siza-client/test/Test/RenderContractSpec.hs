{-# LANGUAGE OverloadedStrings #-}

module Test.RenderContractSpec (renderContractSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.RenderContract (displayCandidate, repairDisplayContract)

renderContractSpec :: Spec
renderContractSpec = describe "R10-T3 inferred-type display contract" $ do
    describe "pure applicability" $ do
        it "wraps strict Text as SVG for a chart deliverable" $ do
            let got = displayCandidate "show the chart" "Text" "bars xs plot"
            got `shouldSatisfy` maybe False (T.isInfixOf "import qualified Data.Text as T")
            got
                `shouldSatisfy` maybe False (T.isInfixOf "displaySvg (T.unpack (bars xs plot))")
        it "accepts a check_type reply carrying the expression before ::" $
            displayCandidate "show the chart" "bars xs plot :: Text" "bars xs plot"
                `shouldSatisfy` maybe False (T.isInfixOf "displaySvg")
        it "wraps a multiline bare expression as one expression" $
            displayCandidate "show the chart" "Text" "bars\n  xs\n  defPlot"
                `shouldSatisfy` maybe False (T.isInfixOf "displaySvg (T.unpack (bars xs defPlot))")
        it "wraps lazy Text with the matching unpacker" $
            displayCandidate "display the plot" "Data.Text.Lazy.Text" "renderZephyr x"
                `shouldSatisfy` maybe False (T.isInfixOf "TL.unpack")
        it "appends a display of a top-level textual binding" $
            displayCandidate "show this" "T.Text" "plotCumulus = renderCumulus input"
                `shouldSatisfy` maybe False (T.isInfixOf "displaySvg (T.unpack (plotCumulus))")
        it "wraps markdown-content Text with displayMarkdown" $
            displayCandidate
                "write a summary report"
                "Text"
                "summary = \"# Results\\n\\nAll checks passed.\""
                `shouldSatisfy` maybe False (T.isInfixOf "displayMarkdown (T.unpack (summary))")
        it "sniffs SVG and HTML content before the textual default" $ do
            displayCandidate "show the result" "Text" "\"<svg><path/></svg>\""
                `shouldSatisfy` maybe False (T.isInfixOf "displaySvg")
            displayCandidate "show the result" "String" "\"<table><tr></tr></table>\""
                `shouldSatisfy` maybe False (T.isInfixOf "displayHtml")
        it "wraps a DataFrame value with its display bridge" $
            displayCandidate "show me the table" "DataFrame" "housing"
                `shouldSatisfy` maybe
                    False
                    (T.isInfixOf "DFDisplay.display DFDisplay.defaultDisplayOptions (housing)")
        it "never fires for a non-markup inferred type" $
            displayCandidate "show the chart" "Int" "answer = 42" `shouldBe` Nothing
        it "never fires when the deliverable does not request display" $
            displayCandidate "compute the value" "Text" "answer" `shouldBe` Nothing
        it "never double-wraps an already displayed source" $
            displayCandidate "show it" "Text" "displaySvg (T.unpack chart)"
                `shouldBe` Nothing

    it "keeps the wrapper only when replacement compiles and produces output" $ do
        calls <- newIORef ([] :: [ToolCall])
        let original = ToolCall "insert_cell" (object ["source" .= ("plotZephyr xs" :: Text)])
            originalOut =
                Right . ToolOk $
                    object
                        [ "cellId" .= (3 :: Int)
                        , "execution" .= object ["ok" .= True, "outputs" .= ([] :: [Value])]
                        ]
            dispatch tc = do
                modifyIORef' calls (++ [tc])
                pure . Right $ case tcName tc of
                    "check_type" -> ToolOk (object ["result" .= ("Text" :: Text)])
                    "list_cells" -> ToolOk (cellsSnapshot False)
                    "replace_cell_source" ->
                        ToolOk
                            ( object
                                [ "cellId" .= (3 :: Int)
                                , "execution"
                                    .= object
                                        [ "ok" .= True
                                        , "outputs"
                                            .= [ object ["oiMime" .= ("image/svg+xml" :: Text), "oiOutput" .= ("<svg/>" :: Text)]
                                               ]
                                        ]
                                ]
                            )
                    _ -> ToolOk (object [])
        repaired <- repairDisplayContract "show the chart" dispatch original originalOut
        repaired `shouldSatisfy` maybe False (T.isInfixOf "displaySvg" . sourceOf . fst)
        seen <- readIORef calls
        map tcName seen
            `shouldBe` ["check_type", "list_cells", "replace_cell_source", "list_cells"]

    it "rejects a rendering candidate when acceptRepair sees a sibling regression" $ do
        calls <- newIORef ([] :: [ToolCall])
        snapshots <- newIORef (0 :: Int)
        let original = ToolCall "insert_cell" (object ["source" .= ("renderNimbus xs" :: Text)])
            originalOut = cleanOutcome 3 []
            dispatch tc = do
                modifyIORef' calls (++ [tc])
                case tcName tc of
                    "check_type" -> pure (Right (ToolOk (object ["result" .= ("Text" :: Text)])))
                    "list_cells" -> do
                        n <- readIORef snapshots
                        modifyIORef' snapshots (+ 1)
                        pure . Right . ToolOk $ cellsSnapshot (n > 0)
                    "read_cell" -> pure (Right (ToolOk (object ["error" .= ("new sibling failure" :: Text)])))
                    "replace_cell_source"
                        | "displaySvg" `T.isInfixOf` sourceOf tc -> pure (cleanOutcome 3 [svgOutput])
                        | otherwise -> pure originalOut
                    _ -> pure (Right (ToolOk (object [])))
        repaired <-
            repairDisplayContract "render the visual" dispatch original originalOut
        repaired `shouldBe` Nothing
        seen <- readIORef calls
        map tcName seen
            `shouldBe` [ "check_type"
                       , "list_cells"
                       , "replace_cell_source"
                       , "list_cells"
                       , "read_cell"
                       , "replace_cell_source"
                       ]

cleanOutcome :: Int -> [Value] -> Either Text ToolOutcome
cleanOutcome cid outputs =
    Right . ToolOk $
        object
            [ "cellId" .= cid
            , "execution" .= object ["ok" .= True, "outputs" .= outputs]
            ]

svgOutput :: Value
svgOutput =
    object ["oiMime" .= ("image/svg+xml" :: Text), "oiOutput" .= ("<svg/>" :: Text)]

cellsSnapshot :: Bool -> Value
cellsSnapshot siblingRed =
    object
        [ "cells"
            .= [ object ["id" .= (3 :: Int), "hasError" .= False, "defines" .= ([] :: [Text])]
               , object
                    ["id" .= (4 :: Int), "hasError" .= siblingRed, "defines" .= ([] :: [Text])]
               ]
        ]

sourceOf :: ToolCall -> Text
sourceOf tc = case tcArgs tc of
    Object o -> case KM.lookup (K.fromText "new_source") o of
        Just (String s) -> s
        _ -> ""
    _ -> ""
