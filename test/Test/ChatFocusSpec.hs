{-# LANGUAGE OverloadedStrings #-}

-- | What the model is told about the cell a question was asked from.
module Test.ChatFocusSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    OutputItem (..),
    textToMime,
 )
import Sabela.Server.Ai.Focus (focusPreamble)
import qualified Sabela.SessionTypes as ST

cell :: Int -> T.Text -> [OutputItem] -> Maybe T.Text -> Cell
cell cid src outs err = Cell cid CodeCell ST.Haskell src outs err False

spec :: Spec
spec = describe "chat focus" $ do
    it "names the cell, its kind, its source and its error" $ do
        let nb =
                Notebook
                    "t.md"
                    [cell 4 "x = 1\nx + \"a\"" [] (Just "No instance for Num String")]
            Just p = focusPreamble nb 4
        T.isInfixOf "cell 4 (code)" p `shouldBe` True
        T.isInfixOf "x + \"a\"" p `shouldBe` True
        T.isInfixOf "No instance for Num String" p `shouldBe` True

    it "reads a table's columns and a chart's spec from the outputs" $ do
        let grid =
                "<div class=\"sabela-grid\"><script type=\"application/json\" class=\"sabela-grid-data\">{\"cols\":[\"city\",\"n\"],\"types\":[\"Text\",\"Int\"],\"rows\":[]}</script></div>"
            chart =
                "<div class=\"sabela-viz\"><script type=\"application/json\" class=\"sabela-viz-spec\">{\"v\":2,\"vegaLite\":{\"mark\":{\"type\":\"bar\"},\"encoding\":{\"x\":{\"field\":\"city\",\"type\":\"nominal\"}},\"data\":{\"values\":[]},\"width\":600,\"height\":400}}</script></div>"
            nb =
                Notebook
                    "t.md"
                    [ cell 1 "df" [OutputItem (textToMime "text/html") grid] Nothing
                    , cell
                        2
                        "C.chart df |> C.mark C.Bar"
                        [OutputItem (textToMime "text/html") chart]
                        Nothing
                    ]
            Just t = focusPreamble nb 1
            Just c = focusPreamble nb 2
        T.isInfixOf "(table)" t `shouldBe` True
        T.isInfixOf "city : Text, n : Int" t `shouldBe` True
        T.isInfixOf "(chart)" c `shouldBe` True
        T.isInfixOf "\"mark\":\"bar\"" c `shouldBe` True
        T.isInfixOf "Data expression: df" c `shouldBe` True

    it "says nothing for a cell that is not there" $
        focusPreamble (Notebook "t.md" []) 9 `shouldBe` Nothing
