{-# LANGUAGE OverloadedStrings #-}

module Test.RecordFieldDiscoverySpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ModuleCard (hitJSON)
import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    Match (..),
    parseCapabilities,
 )

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

hit :: Capability -> Hit
hit c = Hit{hitCap = c, hitScore = 100, hitVia = ByName}

readOptionsBrowse :: Text
readOptionsBrowse =
    "data DataFrame.ReadOptions = DataFrame.ReadOptions {DataFrame.columnSeparator :: Char}\n\
    \DataFrame.summarize :: DataFrame.DataFrame -> DataFrame.DataFrame"

caps :: [Capability]
caps = parseCapabilities "DataFrame" readOptionsBrowse

capNamed :: Text -> Capability
capNamed n = case [c | c <- caps, capName c == n] of
    (c : _) -> c
    [] -> error ("no capability named " <> T.unpack n)

spec :: Spec
spec = describe "a record field is told apart from a plain function" $ do
    describe "parseCapabilities" $ do
        it "names the record a field belongs to" $
            capField (capNamed "columnSeparator") `shouldBe` Just "ReadOptions"

        it "leaves an ordinary function's field empty" $
            capField (capNamed "summarize") `shouldBe` Nothing

    describe "hitJSON" $ do
        it "carries the exact usable record-update syntax, not a description" $
            textField "field" (hitJSON "" (hit (capNamed "columnSeparator")))
                `shouldBe` Just "ReadOptions { columnSeparator = ... }"

        it "omits the key entirely for an ordinary function" $
            field "field" (hitJSON "" (hit (capNamed "summarize"))) `shouldBe` Nothing
