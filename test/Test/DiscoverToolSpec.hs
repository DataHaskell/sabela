{-# LANGUAGE OverloadedStrings #-}

{- | The wire shape of the two discovery tool handlers, pinned so the JSON the
model (and the MCP client) sees does not drift.
-}
module Test.DiscoverToolSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.AI.Capabilities.Discover (findExamplesOutcome)
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

enc :: ToolOutcome -> Text
enc (ToolOk v) = encT v
enc (ToolErr v) = encT v

encT :: Value -> Text
encT = TE.decodeUtf8 . LBS.toStrict . encode

query :: Text -> Value
query q = object ["query" .= q]

spec :: Spec
spec = describe "discovery tool handlers" $ do
    it "find_example_cell returns a runnable example cell for a covered query" $ do
        let t = enc (findExamplesOutcome (query "read csv"))
        t `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: dataframe"
        t `shouldSatisfy` T.isInfixOf "readCsv"

    it "find_example_cell returns no example for a retired query (no near-miss)" $ do
        let t = enc (findExamplesOutcome (query "plotting"))
        t `shouldSatisfy` T.isInfixOf "\"examples\":[]"
