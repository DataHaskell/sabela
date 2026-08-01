{-# LANGUAGE OverloadedStrings #-}

module Test.QueryGuidanceSpec (spec) where

import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.AI.Capabilities.Query (guidedOutcome)
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

hiddenResult :: Text
hiddenResult =
    "Could not load module \8216DataFrame\8217.\n\
    \It is a member of the hidden package \8216dataframe-2.3.0.0\8217.\n\
    \You can run \8216:set -package dataframe\8217 to expose it.\n\
    \It is a member of the hidden package \8216dataframe-2.2.0.0\8217."

-- | What check_type was asked about: the expression, and nothing else.
browsed :: Text
browsed = "readCsv"

outcomeText :: ToolOutcome -> Text
outcomeText (ToolOk v) = enc v
outcomeText (ToolErr v) = enc v

enc :: Value -> Text
enc = TE.decodeUtf8 . LBS.toStrict . encode

spec :: Spec
spec = describe "Sabela.AI.Capabilities.Query.guidedOutcome" $ do
    it "turns a hidden-package browse wall into a -- cabal: dependency hint" $ do
        let t = outcomeText (guidedOutcome browsed [] hiddenResult)
        t `shouldSatisfy` T.isInfixOf "build-depends: dataframe"
        t `shouldSatisfy` T.isInfixOf "-- cabal:"

    it "adds no guidance to a clean result" $ do
        let t = outcomeText (guidedOutcome browsed [] "sum :: Num a => [a] -> a")
        t `shouldSatisfy` not . T.isInfixOf "guidance"
