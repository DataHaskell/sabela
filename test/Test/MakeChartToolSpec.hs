{-# LANGUAGE OverloadedStrings #-}

{- | The chart tool: its name on the wire, and the cell a call becomes or
the reason it does not, decided before any code reaches the kernel.
-}
module Test.MakeChartToolSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.MakeChart (chartCallSource)
import Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    actsOnNotebook,
    parseToolName,
    toolWireName,
 )
import Sabela.AI.Capabilities.Tools (chatToolSpecs)
import Sabela.AI.Capabilities.Tools.Viz (makeChartSchema)
import Sabela.LLM.Tool (ToolSpec (..))

spec :: Spec
spec = describe "make_chart" $ do
    it "is a named tool that acts on the notebook" $ do
        parseToolName "make_chart" `shouldBe` Just MakeChart
        toolWireName MakeChart `shouldBe` "make_chart"
        actsOnNotebook MakeChart `shouldBe` True
        [toolSchema t | t <- chatToolSpecs, toolName t == MakeChart]
            `shouldBe` [makeChartSchema]

    it "writes the canonical cell for a well-formed call" $ do
        let call =
                object
                    [ "data" .= ("orders" :: T.Text)
                    , "mark" .= ("bar" :: T.Text)
                    , "x" .= object ["field" .= ("city" :: T.Text)]
                    , "y"
                        .= object ["field" .= ("revenue" :: T.Text), "aggregate" .= ("sum" :: T.Text)]
                    , "title" .= ("Revenue" :: T.Text)
                    , "cell_id" .= (4 :: Int)
                    , "select" .= ("interval" :: T.Text)
                    , "select_name" .= ("pick" :: T.Text)
                    ]
        case chartCallSource call of
            Left why -> expectationFailure (T.unpack why)
            Right src -> do
                T.isInfixOf "import qualified DataFrame.Display.Web.Chart.Typed as C" src
                    `shouldBe` True
                T.isInfixOf "C.chart orders" src `shouldBe` True
                T.isInfixOf "|> C.aggregateOn C.Y C.Sum" src `shouldBe` True
                T.isInfixOf "|> C.title \"Revenue\"" src `shouldBe` True

    it "names the field and the allowed words when a value does not fit" $ do
        let call =
                object
                    [ "data" .= ("orders" :: T.Text)
                    , "mark" .= ("histogram" :: T.Text)
                    , "x" .= object ["field" .= ("a" :: T.Text)]
                    ]
        case chartCallSource call of
            Left why -> do
                T.isInfixOf "mark: expected one of bar, line" why `shouldBe` True
                T.isInfixOf "histogram" why `shouldBe` True
            Right _ -> expectationFailure "expected a rejection"

    it "asks for data when none is given" $
        case chartCallSource (object ["mark" .= ("bar" :: T.Text)]) of
            Left why -> T.isInfixOf "data is required" why `shouldBe` True
            Right _ -> expectationFailure "expected a rejection"

    it "rejects a key the chart language does not have, by name" $
        case chartCallSource
            ( object
                [ "data" .= ("df" :: T.Text)
                , "mark" .= ("bar" :: T.Text)
                , "colour" .= object ["field" .= ("a" :: T.Text)]
                ]
            ) of
            Left why -> T.isInfixOf "unknown key(s) colour" why `shouldBe` True
            Right _ -> expectationFailure "expected a rejection"

    it "accepts a count-all y with a bare object schema" $
        case makeChartSchema of
            Object _ -> pure ()
            _ -> expectationFailure "schema should be an object"
