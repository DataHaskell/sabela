{-# LANGUAGE OverloadedStrings #-}

{- | `consulted[].status` is what the model reads to decide whether an answer is
complete. A source consulted twice, once successfully and once not, is partial:
reporting it as `ok` beside a note saying the session was unavailable is the
harness contradicting itself in one object.
-}
module Test.ConsultedStatusSpec (consultedStatusSpec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Discover.Render (consultedJson, dedupSources)
import Siza.Agent.Discover.Types (
    SourceAnswer (..),
    okAnswer,
    unavailableAnswer,
 )

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

statusOf :: SourceAnswer -> Maybe Value
statusOf = field "status" . consultedJson

sessionDown :: SourceAnswer
sessionDown =
    unavailableAnswer
        "session"
        "session unavailable (no live kernel or transport error)"

consultedStatusSpec :: Spec
consultedStatusSpec = describe "a consulted source reports how complete its answer is" $ do
    it "is ok when every consultation succeeded" $
        statusOf (okAnswer "session" []) `shouldBe` Just (String "ok")

    it "is unavailable when every consultation failed" $
        statusOf sessionDown `shouldBe` Just (String "unavailable")

    it "is partial when one consultation succeeded and another did not" $ do
        let merged = dedupSources [okAnswer "session" [], sessionDown]
        map statusOf merged `shouldBe` [Just (String "partial")]

    it "is partial regardless of which consultation came first" $ do
        let merged = dedupSources [sessionDown, okAnswer "session" []]
        map statusOf merged `shouldBe` [Just (String "partial")]

    it "never reports ok beside a note recording a failure" $ do
        let merged = dedupSources [okAnswer "session" [], sessionDown]
        case merged of
            [a] -> do
                statusOf a `shouldNotBe` Just (String "ok")
                saNote a `shouldBe` "session unavailable (no live kernel or transport error)"
            _ -> expectationFailure "expected one merged session answer"

    it "keeps distinct sources apart" $ do
        let merged = dedupSources [okAnswer "session" [], sessionDown, okAnswer "hoogle" []]
        map saSource merged `shouldBe` ["session", "hoogle"]
        map statusOf merged
            `shouldBe` [Just (String "partial"), Just (String "ok")]
