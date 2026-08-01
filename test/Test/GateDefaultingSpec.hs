{-# LANGUAGE OverloadedStrings #-}

{- | A binding whose type only exists because GHC defaulted an ambiguous type
variable to @()@ is a green cell that means nothing. The rule is about GHC
defaulting, not about any library: it fires the same for @mempty@, @minBound@
or any other ambiguous expression (LEDGER C2-8d).
-}
module Test.GateDefaultingSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.CompileGate (defaultedToUnit)
import Test.HarnessGen (genDefaultingWarning, genDiagnostic)

spec :: Spec
spec = describe "defaulting to unit is not a result" $ do
    it
        "prop_unitDefaultsAreReported: a type variable defaulted to () or Any \
        \is reported, for any variable and any warning position"
        $ property
        $ forAll (genDefaultingWarning (elements ["()", "GHC.Types.Any", "Any"]))
        $ \w -> not (null (defaultedToUnit w))

    it
        "prop_ordinaryDefaultsAreNotReported: defaulting to a type that carries \
        \information is normal Haskell and is never reported"
        $ property
        $ forAll
            ( genDefaultingWarning
                (elements ["Integer", "Double", "[Char]", "Data.Text.Text", "Int"])
            )
        $ \w -> null (defaultedToUnit w)

    it "prop_errorsAreNotDefaultingWarnings: a plain diagnostic reports nothing" $
        property $
            forAll genDiagnostic $
                \d -> null (defaultedToUnit d)

    it "reports the variable GHC named, so the message can quote it" $ do
        let w =
                "<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]\n\
                \    \8226 Defaulting the type variable \8216a0\8217 to type \8216()\8217 \
                \in the following constraints"
        defaultedToUnit w `shouldSatisfy` elem "a0"

    it "survives several warnings in one block" $ do
        let w =
                "<interactive>:1:1: warning: [-Wtype-defaults]\n\
                \    \8226 Defaulting the type variable \8216a0\8217 to type \8216Integer\8217\n\
                \\n\
                \<interactive>:2:1: warning: [-Wtype-defaults]\n\
                \    \8226 Defaulting the type variable \8216b1\8217 to type \8216()\8217"
        defaultedToUnit w `shouldBe` ["b1"]
        T.length (T.pack (show (defaultedToUnit w))) `shouldSatisfy` (> 0)
