{-# LANGUAGE OverloadedStrings #-}

{- | A candidate that typechecks is never reported as a compile error. The
value echo needs a `Show` instance the candidate's author never asked for, so
failing it is the harness's outcome, not the candidate's diagnostic.
-}
module Test.UnshowableValueSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Try.Payload (
    pureOutcomeText,
    pureVerdictClass,
    unshowablePayload,
 )
import Sabela.AI.Verdict (VerdictClass (..))
import qualified Sabela.SessionTypes as ST

result :: Text -> ST.PureEvalResult
result ty =
    ST.PureEvalResult
        { ST.pureEvalVerdict = ST.PureEvalUnshowable
        , ST.pureEvalGeneration = 1
        , ST.pureEvalInferredType = ty
        , ST.pureEvalOutput = ""
        , ST.pureEvalError = ""
        , ST.pureEvalBindingsUnchanged = True
        , ST.pureEvalItUnchanged = True
        , ST.pureEvalRecovery = ST.PureEvalNoRecovery
        }

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = describe "a value with no Show instance is not a compile error" $ do
    it "is an ok verdict, because the candidate typechecked" $ do
        pureVerdictClass ST.PureEvalUnshowable `shouldBe` VerdictOk
        pureOutcomeText ST.PureEvalUnshowable `shouldBe` "ok"

    it "keeps the type the admission probe already inferred" $
        field "type" (unshowablePayload (result "Int -> Int"))
            `shouldBe` Just (String "Int -> Int")

    it "says the value was not shown, rather than implying there is none" $
        field "valueShown" (unshowablePayload (result "Int -> Int"))
            `shouldBe` Just (Bool False)

    it "names the missing instance and the type it is missing for" $
        case field "reason" (unshowablePayload (result "Int -> Int")) of
            Just (String r) -> do
                r `shouldSatisfy` T.isInfixOf "Show"
                r `shouldSatisfy` T.isInfixOf "Int -> Int"
            _ -> expectationFailure "expected a reason naming the instance"

    it "carries no diagnostic, because the candidate has no defect" $ do
        let v = unshowablePayload (result "IO ()")
        field "stderr" v `shouldBe` Nothing
        field "diagnostic" v `shouldBe` Nothing

    it "never leaks the harness's own wrapper into the reason" $
        case field "reason" (unshowablePayload (result "Picture -> IO ()")) of
            Just (String r) -> do
                r `shouldNotSatisfy` T.isInfixOf "_sabelaCandidate"
                r `shouldNotSatisfy` T.isInfixOf "take 4001"
                r `shouldNotSatisfy` T.isInfixOf "rendered"
            _ -> expectationFailure "expected a reason"

    it "still reports the live-state invariants the caller relies on" $ do
        let v = unshowablePayload (result "Int -> Int")
        field "bindingsUnchanged" v `shouldBe` Just (Bool True)
        field "itUnchanged" v `shouldBe` Just (Bool True)
