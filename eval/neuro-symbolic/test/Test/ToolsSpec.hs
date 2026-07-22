{-# LANGUAGE OverloadedStrings #-}

module Test.ToolsSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.Tools (unknownToolMsg, withInsertDefaults)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = do
    describe "withInsertDefaults (insert_cell fields hidden from the model)" $ do
        it "injects a Haskell CodeCell kind when absent" $ do
            let v = withInsertDefaults (object ["source" .= ("x = 1" :: Text)])
            field "cell_type" v `shouldBe` Just (String "CodeCell")
            field "language" v `shouldBe` Just (String "Haskell")
        it "injects no placement anchor — the server appends every new cell" $
            field
                "after_cell_id"
                (withInsertDefaults (object ["source" .= ("x" :: Text)]))
                `shouldBe` Nothing
        it "keeps any field the model did supply" $ do
            let v =
                    withInsertDefaults
                        ( object
                            [ "source" .= ("x" :: Text)
                            , "cell_type" .= ("ProseCell" :: Text)
                            ]
                        )
            field "cell_type" v `shouldBe` Just (String "ProseCell")

    describe "unknownToolMsg (recovery hint for an invented tool)" $ do
        it "names the invented tool" $
            ("browse" `T.isInfixOf` unknownToolMsg "browse") `shouldBe` True
        it "redirects a browse to discover" $
            ("discover" `T.isInfixOf` unknownToolMsg "browse") `shouldBe` True
        it "lists delete_cell among the valid tools" $
            ("delete_cell" `T.isInfixOf` unknownToolMsg "bogus") `shouldBe` True
