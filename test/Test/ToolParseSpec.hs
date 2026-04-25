{-# LANGUAGE OverloadedStrings #-}

module Test.ToolParseSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

parseErrorMarker :: Text
parseErrorMarker = "_parseError"

lookupParseError :: Value -> Maybe Text
lookupParseError (Object o) = case KM.lookup (Key.fromText parseErrorMarker) o of
    Just (String s) -> Just s
    _ -> Nothing
lookupParseError _ = Nothing

spec :: Spec
spec = do
    describe "tool_use parse-failure propagation" $ do
        it "a clean tool input has no parse error marker" $ do
            let v = object ["cell_id" .= (1 :: Int), "new_source" .= ("x = 1" :: Text)]
            lookupParseError v `shouldBe` Nothing

        it "the orchestrator's truncation marker surfaces as a hint" $ do
            let v =
                    object
                        [ Key.fromText parseErrorMarker
                            .= ("Tool input JSON failed to parse" :: Text)
                        , "raw" .= ("{\"cell_id\": 1, \"new_source\": \"partial" :: Text)
                        ]
            lookupParseError v
                `shouldSatisfy` maybe False ("failed to parse" `T.isInfixOf`)

        it "non-object values are treated as having no parse error" $ do
            lookupParseError (String "some scalar") `shouldBe` Nothing
            lookupParseError Null `shouldBe` Nothing
