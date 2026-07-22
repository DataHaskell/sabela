{-# LANGUAGE OverloadedStrings #-}

{- | R7-T3 x R8.4: verifier-surface leak lint — the run-181440 check_type
shape fails, the distilled shape passes; non-verifier channels and
model-authored text are exempt.
-}
module Test.VerifierLeakLintSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.TranscriptLint (LintIssue (..), lintMessages)

asst :: Text -> Value
asst c = object ["role" .= ("assistant" :: Text), "content" .= c]

toolRes :: Text -> Text -> Value
toolRes n c = object ["role" .= ("tool" :: Text), "tool_name" .= n, "content" .= c]

rules :: [Value] -> [Text]
rules = map liRule . lintMessages

verifierRules :: [Value] -> [Text]
verifierRules = filter ("verifier-" `T.isPrefixOf`) . rules

-- | The run-181440 revenueTotal check_type content: sig + double-encoded blob.
blobCheckType :: Text
blobCheckType =
    "{\"expr\":\"D.sum\",\"result\":\"D.sum :: (D.Columnable a, \
    \Num a) => D.Expr a -> D.DataFrame -> a\\n\\n{\\\"version\\\"\
    \:\\\"1.1\\\",\\\"severity\\\":\\\"Error\\\",\\\"message\\\":\
    \[\\\"Not in scope\\\"]}\",\"via\":\"type\"}"

distilledCheckType :: Text
distilledCheckType =
    "{\"expr\":\"D.sum\",\"result\":\"D.sum :: (D.Columnable a, \
    \Num a) => D.Expr a -> D.DataFrame -> a\",\"verdict\":\"ok\"\
    \,\"via\":\"type\"}"

hashBindings :: Text
hashBindings =
    "{\"result\":\"stale: df :: D.DataFrame = \
    \dtfrm-cr-2.0.0.0-c1e52ef7:DataFrame.Internal.DataFrame\
    \.DataFrame\",\"verdict\":\"ok\"}"

spec :: Spec
spec = describe "verifier surfaces are envelope citizens (R7-T3, R8.4)" $ do
    it "flags the run-181440 check_type double-encoded blob" $
        rules [toolRes "check_type" blobCheckType]
            `shouldSatisfy` elem "verifier-serialisation-in-string"
    it "passes the distilled check_type shape" $
        verifierRules [toolRes "check_type" distilledCheckType] `shouldBe` []
    it "flags a unit-hash atom on the list_bindings surface" $
        rules [toolRes "list_bindings" hashBindings]
            `shouldSatisfy` elem "verifier-package-hash"
    it "a non-verifier channel is exempt from the verifier rule" $
        verifierRules [toolRes "read_cell" blobCheckType] `shouldBe` []
    it "model-authored text is exempt from the verifier rule" $
        verifierRules [asst blobCheckType] `shouldBe` []
