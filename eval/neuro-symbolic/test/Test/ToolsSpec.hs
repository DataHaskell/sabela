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
        it "injects a Haskell CodeCell kind and the anchor when absent" $ do
            let v = withInsertDefaults 3 (object ["source" .= ("x = 1" :: Text)])
            field "cell_type" v `shouldBe` Just (String "CodeCell")
            field "language" v `shouldBe` Just (String "Haskell")
            field "after_cell_id" v `shouldBe` Just (Number 3)
        it "uses -1 (the beginning) as the anchor for an empty notebook" $
            field
                "after_cell_id"
                (withInsertDefaults (-1) (object ["source" .= ("x" :: Text)]))
                `shouldBe` Just (Number (-1))
        it "keeps any field the model did supply" $ do
            let v =
                    withInsertDefaults
                        9
                        ( object
                            [ "source" .= ("x" :: Text)
                            , "cell_type" .= ("ProseCell" :: Text)
                            , "after_cell_id" .= (2 :: Int)
                            ]
                        )
            field "cell_type" v `shouldBe` Just (String "ProseCell")
            field "after_cell_id" v `shouldBe` Just (Number 2)

    describe "unknownToolMsg (recovery hint for an invented tool)" $ do
        it "names the invented tool" $
            ("browse" `T.isInfixOf` unknownToolMsg "browse") `shouldBe` True
        it "redirects a browse to ghci_query" $
            ("ghci_query" `T.isInfixOf` unknownToolMsg "browse") `shouldBe` True
