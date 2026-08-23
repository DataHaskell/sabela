{-# LANGUAGE OverloadedStrings #-}

{- | Reading and writing a notebook without drowning in it. Asking for a
notebook's prose meant pulling every code cell and every rendered chart with
it, and thirty prose edits meant thirty round trips.
-}
module Test.McpBulkSpec (mcpBulkSpec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Bulk (applyCellFilters, keptOutputs, replaceCellsPlan)

cell :: Int -> Text -> Text -> Value
cell cid ty firstLine =
    object
        [ "id" .= cid
        , "type" .= ty
        , "source" .= firstLine
        , "position" .= cid
        ]

notebook :: Value
notebook =
    object
        [ "title" .= ("n" :: Text)
        , "cells"
            .= [ cell 1 "CodeCell" "import Torch"
               , cell 2 "ProseCell" "## A heading"
               , cell 3 "CodeCell" "x = 1"
               , cell 4 "ProseCell" "Some prose."
               ]
        ]

idsOf :: Value -> [Int]
idsOf (Object o) = case KM.lookup "cells" o of
    Just (Array cs) ->
        [ n
        | Object c <- foldr (:) [] cs
        , Just (Number v) <- [KM.lookup "id" c]
        , let n = round v
        ]
    _ -> []
idsOf _ = []

mcpBulkSpec :: Spec
mcpBulkSpec = describe "reading and writing a notebook in bulk" $ do
    describe "asking for one kind of cell" $ do
        it "keeps only prose when asked for prose" $
            idsOf (applyCellFilters (object ["cell_type" .= ("ProseCell" :: Text)]) notebook)
                `shouldBe` [2, 4]

        it "keeps only code when asked for code" $
            idsOf (applyCellFilters (object ["cell_type" .= ("CodeCell" :: Text)]) notebook)
                `shouldBe` [1, 3]

        it "leaves the notebook alone when no filter is given" $
            idsOf (applyCellFilters (object []) notebook) `shouldBe` [1, 2, 3, 4]

        it "leaves it alone rather than emptying it on an unknown type" $
            idsOf (applyCellFilters (object ["cell_type" .= ("Nonsense" :: Text)]) notebook)
                `shouldBe` [1, 2, 3, 4]

    describe "asking for one kind of output" $ do
        let outputs =
                [ object ["oiMime" .= ("text/markdown" :: Text), "oiOutput" .= ("| a |" :: Text)]
                , object ["oiMime" .= ("image/svg+xml" :: Text), "oiOutput" .= ("<svg/>" :: Text)]
                ]
        it "keeps the mime that was asked for" $
            keptOutputs (Just "text/markdown") outputs `shouldBe` take 1 outputs

        it "keeps everything when nothing is asked for" $
            keptOutputs Nothing outputs `shouldBe` outputs

        it "keeps nothing rather than guessing when the mime is absent" $
            keptOutputs (Just "text/plain") outputs `shouldBe` []

    describe "many edits in one call" $ do
        it "plans one replace per entry, in the order given" $
            replaceCellsPlan
                ( object
                    [ "edits"
                        .= [ object ["cell_id" .= (3 :: Int), "new_source" .= ("a" :: Text)]
                           , object ["cell_id" .= (1 :: Int), "new_source" .= ("b" :: Text)]
                           ]
                    ]
                )
                `shouldBe` Right
                    [ (3, "a", Nothing)
                    , (1, "b", Nothing)
                    ]

        it "carries an expected hash through when one is given" $
            replaceCellsPlan
                ( object
                    [ "edits"
                        .= [ object
                                [ "cell_id" .= (2 :: Int)
                                , "new_source" .= ("c" :: Text)
                                , "expected_hash" .= ("p9" :: Text)
                                ]
                           ]
                    ]
                )
                `shouldBe` Right [(2, "c", Just "p9")]

        it "refuses an entry missing its source rather than writing part of it" $
            replaceCellsPlan (object ["edits" .= [object ["cell_id" .= (2 :: Int)]]])
                `shouldSatisfy` either (const True) (const False)

        it "refuses an empty batch" $
            replaceCellsPlan (object ["edits" .= ([] :: [Value])])
                `shouldSatisfy` either (const True) (const False)
