{-# LANGUAGE OverloadedStrings #-}

{- | Pins the pure 'peekData' analysis (delimiter inference, header
detection, per-column type guessing, first-N rows) and the @peek_data@
JSON shape, plus a 'ToolName' round-trip for the new constructor.
-}
module Test.PeekDataSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    parseToolName,
    toolWireName,
 )
import Sabela.AI.PeekData (
    ColType (..),
    PeekResult (..),
    peekData,
    peekResultJSON,
 )
import Test.Hspec

csv :: Text
csv =
    "name,age,score,active\n\
    \alice,30,9.5,true\n\
    \bob,25,8.0,false\n\
    \carol,41,7.25,true\n"

spec :: Spec
spec = do
    describe "peekData delimiter + header + types" $ do
        let r = peekData 2 csv

        it "infers the comma delimiter" $
            peekDelimiter r `shouldBe` ","

        it "detects the header row" $ do
            peekHasHeader r `shouldBe` True
            peekHeader r `shouldBe` ["name", "age", "score", "active"]

        it "returns at most N data rows (not the header)" $ do
            length (peekRows r) `shouldBe` 2
            peekRows r
                `shouldBe` [ ["alice", "30", "9.5", "true"]
                           , ["bob", "25", "8.0", "false"]
                           ]

        it "guesses per-column types" $
            peekColTypes r `shouldBe` [ColText, ColInt, ColDouble, ColBool]

        it "infers a tab delimiter" $
            peekDelimiter (peekData 5 "a\tb\n1\t2\n") `shouldBe` "\t"

        it "infers a semicolon delimiter" $
            peekDelimiter (peekData 5 "a;b\n1;2\n") `shouldBe` ";"

        it "treats an all-numeric first row as headerless" $ do
            let h = peekData 5 "1,2,3\n4,5,6\n"
            peekHasHeader h `shouldBe` False
            peekColTypes h `shouldBe` [ColInt, ColInt, ColInt]

    describe "peek_data JSON shape" $ do
        let v = peekResultJSON (peekData 2 csv)
            getKey k = case v of
                Object o -> KM.lookup (Key.fromText k) o
                _ -> Nothing

        it "exposes delimiter, header, columns, and rows" $ do
            getKey "delimiter" `shouldBe` Just (String ",")
            getKey "hasHeader" `shouldBe` Just (Bool True)

        it "tags each column with name and type" $
            getKey "columns"
                `shouldBe` Just
                    ( toJSON
                        [ object ["name" .= ("name" :: Text), "type" .= ("Text" :: Text)]
                        , object ["name" .= ("age" :: Text), "type" .= ("Int" :: Text)]
                        , object ["name" .= ("score" :: Text), "type" .= ("Double" :: Text)]
                        , object ["name" .= ("active" :: Text), "type" .= ("Bool" :: Text)]
                        ]
                    )

    describe "ToolName round-trip for peek_data" $ do
        it "parses the wire name" $
            parseToolName "peek_data" `shouldBe` Just PeekData

        it "round-trips parse . wire for every constructor" $
            mapM_
                (\t -> parseToolName (toolWireName t) `shouldBe` Just t)
                allToolNames

allToolNames :: [ToolName]
allToolNames =
    [ ListCells
    , ReadCell
    , ReadCellOutput
    , FindCellsByContent
    , ProposeEdit
    , ReplaceCellSource
    , InsertCell
    , DeleteCell
    , ExecuteCell
    , Scratchpad
    , ListBindings
    , CheckType
    , FindByType
    , DescribeFunction
    , ApiReference
    , ExploreResult
    , KernelStatus
    , Interrupt
    , KernelRestart
    , AwaitIdle
    , ExportNotebook
    , PeekData
    , FindExampleCell
    , FindFunction
    ]
