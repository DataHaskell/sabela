{-# LANGUAGE OverloadedStrings #-}

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
    DelimitedView (..),
    PeekColumn (..),
    PeekResult (..),
    PeekVerdict (..),
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

-- | The delimited reading, or a failure naming the verdict that came back.
viewOf :: PeekResult -> DelimitedView
viewOf r = case peekVerdict r of
    Delimited v -> v
    NotDelimited reason -> error ("expected a delimited verdict, got: " <> show reason)

spec :: Spec
spec = do
    describe "peekData delimiter + header + types" $ do
        let r = viewOf (peekData 2 csv)

        it "infers the comma delimiter" $
            dvDelimiter r `shouldBe` ","

        it "detects the header row" $ do
            dvHasHeader r `shouldBe` True
            map pcName (dvColumns r)
                `shouldBe` map Just ["name", "age", "score", "active"]

        it "returns at most N data rows (not the header)" $ do
            length (dvRows r) `shouldBe` 2
            dvRows r
                `shouldBe` [ ["alice", "30", "9.5", "true"]
                           , ["bob", "25", "8.0", "false"]
                           ]

        it "guesses per-column types" $
            map pcType (dvColumns r) `shouldBe` [ColText, ColInt, ColDouble, ColBool]

        it "infers a tab delimiter" $
            dvDelimiter (viewOf (peekData 5 "a\tb\n1\t2\n")) `shouldBe` "\t"

        it "infers a semicolon delimiter" $
            dvDelimiter (viewOf (peekData 5 "a;b\n1;2\n")) `shouldBe` ";"

        it "treats an all-numeric first row as headerless and names no column" $ do
            let h = viewOf (peekData 5 "1,2,3\n4,5,6\n")
            dvHasHeader h `shouldBe` False
            map pcName (dvColumns h) `shouldBe` [Nothing, Nothing, Nothing]
            map pcType (dvColumns h) `shouldBe` [ColInt, ColInt, ColInt]

        it "refuses to read prose as a table" $
            case peekVerdict (peekData 5 "hello there\nthis is prose\n") of
                NotDelimited _ -> pure ()
                Delimited v -> expectationFailure ("read prose as " <> show v)

    describe "peek_data JSON shape" $ do
        let v = peekResultJSON (peekData 2 csv)
            getKey k = case v of
                Object o -> KM.lookup (Key.fromText k) o
                _ -> Nothing

        it "exposes the verdict, delimiter, header, columns, and rows" $ do
            getKey "verdict" `shouldBe` Just (String "delimited")
            getKey "delimiter" `shouldBe` Just (String ",")
            getKey "hasHeader" `shouldBe` Just (Bool True)

        it "tags each column with its index, its name and its type" $
            getKey "columns"
                `shouldBe` Just
                    ( toJSON
                        [ column 0 "name" "Text"
                        , column 1 "age" "Int"
                        , column 2 "score" "Double"
                        , column 3 "active" "Bool"
                        ]
                    )

        it "leaves the name null when the file has no header" $
            case peekResultJSON (peekData 2 "1,2\n3,4\n") of
                Object o ->
                    KM.lookup "columns" o
                        `shouldBe` Just
                            ( toJSON
                                [ object ["index" .= (0 :: Int), "name" .= Null, "type" .= ("Int" :: Text)]
                                , object ["index" .= (1 :: Int), "name" .= Null, "type" .= ("Int" :: Text)]
                                ]
                            )
                v -> expectationFailure ("not an object: " <> show v)

    describe "ToolName round-trip for peek_data" $ do
        it "parses the wire name" $
            parseToolName "peek_data" `shouldBe` Just PeekData

        it "round-trips parse . wire for every constructor" $
            mapM_
                (\t -> parseToolName (toolWireName t) `shouldBe` Just t)
                allToolNames

column :: Int -> Text -> Text -> Value
column i n ty = object ["index" .= i, "name" .= n, "type" .= ty]

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
    , Try
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
    , SearchCapability
    , EvalLive
    ]
