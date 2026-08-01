{-# LANGUAGE OverloadedStrings #-}

{- | The session runs with -fdiagnostics-as-json, so GHC's hole fits arrive as
one JSON line with escaped newlines. Splitting that on real newlines yields a
single element and the parser walks off the end, which is why every fit query
came back empty.
-}
module Test.HoleFitsJsonSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)

{- | Verbatim shape of the stderr `try` returns for a hole, trimmed to two fits.
The `\\n` sequences are literal backslash-n in the payload, not newlines.
-}
jsonBlob :: Text
jsonBlob =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"severity\":\"Error\",\
    \\"code\":88464,\"message\":[\"Found hole: _ :: Picture -> Picture -> Picture\",\
    \\"Valid hole fits include\\n  mappend :: forall a. Monoid a => a -> a -> a\\n\
    \    with mappend @Picture\\n      (imported from `Prelude')\\n\
    \  (<>) :: forall a. Semigroup a => a -> a -> a\\n    with (<>) @Picture\\n\
    \      (imported from `Prelude')\"],\"hints\":[]}"

plainBlob :: Text
plainBlob =
    "Found hole: _ :: Picture -> Picture -> Picture\n\
    \Valid hole fits include\n\
    \  mappend :: forall a. Monoid a => a -> a -> a\n\
    \    with mappend @Picture\n\
    \      (imported from `Prelude')\n\
    \  (<>) :: forall a. Semigroup a => a -> a -> a\n\
    \    with (<>) @Picture\n\
    \      (imported from `Prelude')"

spec :: Spec
spec = describe "hole fits parse from the rendering GHC actually emits" $ do
    it "reads the fits out of a JSON diagnostic line" $
        map hfWrite (parseHoleFits jsonBlob) `shouldBe` ["mappend", "(<>)"]

    it "reads the same fits from the plain rendering" $
        map hfWrite (parseHoleFits plainBlob) `shouldBe` ["mappend", "(<>)"]

    it "agrees on the types across both renderings" $
        map hfType (parseHoleFits jsonBlob)
            `shouldBe` map hfType (parseHoleFits plainBlob)

    it "attributes the module in the JSON rendering too" $
        map hfModule (parseHoleFits jsonBlob)
            `shouldBe` [Just "Prelude", Just "Prelude"]

    it "finds nothing when the diagnostic carries no fits" $
        parseHoleFits "{\"message\":[\"Variable not in scope: combine\"],\"hints\":[]}"
            `shouldBe` []
