{-# LANGUAGE OverloadedStrings #-}

module Test.StderrFailureSpec (spec) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Session.Materialize.Run (textualStderrFailure)

diag :: Text -> Text -> Text
diag severity msg =
    "{\"version\":\"1.0\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":\
    \{\"file\":\"<interactive>\",\"start\":{\"line\":514,\"column\":30},\
    \\"end\":{\"line\":514,\"column\":34}},\"severity\":\""
        <> severity
        <> "\",\"code\":63394,\"message\":[\""
        <> msg
        <> "\"],\"hints\":[]}"

spec :: Spec
spec = describe "Sabela.Session.Materialize.Run.textualStderrFailure" $ do
    it "a JSON warning is not a failure" $
        textualStderrFailure (diag "Warning" "In the use of head") `shouldBe` Nothing

    it "a JSON error is a failure, and reports the error's own message" $
        textualStderrFailure (diag "Error" "Variable not in scope: foo")
            `shouldSatisfy` maybe False (T.isInfixOf "Variable not in scope")

    it "warnings alongside an error never mask it" $
        textualStderrFailure
            ( diag "Warning" "In the use of head"
                <> "\n"
                <> diag "Error" "Couldn't match type"
            )
            `shouldSatisfy` isJust

    it "a runtime exception is still a failure" $
        textualStderrFailure "*** Exception: divide by zero"
            `shouldSatisfy` isJust

    it "a textual (non-JSON) error is still a failure" $
        textualStderrFailure
            "<interactive>:3:1: error: [GHC-88464] Variable not in scope: nope"
            `shouldSatisfy` isJust

    it "clean stderr is no failure" $
        textualStderrFailure "" `shouldBe` Nothing

    it "linker noise is no failure" $
        textualStderrFailure "ld: warning: ignoring duplicate libraries"
            `shouldBe` Nothing
