{-# LANGUAGE OverloadedStrings #-}

{- | The admission probe asks @Data.Typeable.typeOf@ for the candidate's type,
which needs a concrete one. A defaultable candidate such as @21 * 2@ carries
@Num a@ alongside @Typeable a@, and @Typeable@ is not a standard class, so
standard defaulting does not fire and GHC reports an ambiguous type variable.
-}
module Test.PureEvalProtocolSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.Session.Query.PureEval.Protocol (admissionCommand)

spec :: Spec
spec = describe "the pure admission probe" $ do
    let cmd = admissionCommand "21 * 2"

    it "enables extended defaulting, without which a numeric literal is ambiguous" $
        cmd `shouldSatisfy` T.isInfixOf ":set -XExtendedDefaultRules"

    it "restores the setting, so a candidate cannot change how cells default" $
        cmd `shouldSatisfy` T.isInfixOf ":set -XNoExtendedDefaultRules"

    it "sets before the probe and restores after it" $ do
        let ix t = T.length (fst (T.breakOn t cmd))
        ix ":set -XExtendedDefaultRules" `shouldSatisfy` (< ix ":cmd")
        ix ":cmd" `shouldSatisfy` (< ix ":set -XNoExtendedDefaultRules")

    it "still carries the candidate expression" $
        cmd `shouldSatisfy` T.isInfixOf "21 * 2"
