{-# LANGUAGE OverloadedStrings #-}

{- | The compact module card a browse-route discover answer renders to.
Measured live: GHC's PERFECT answers (did-you-mean with package flags,
hidden-package notes) reached the model as raw double-encoded JSON blobs with
the same paragraph repeated 11-15x — the one load-bearing line buried twice.
The card decodes the diagnostic (via "Sabela.Errors.Json") and ships ONLY the
distilled fields.
-}
module Test.BrowseCardSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.BrowseCard (
    browseCard,
    packageOfUnit,
 )

-- | Field accessor for card assertions.
field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

-- Verbatim WIRE fixture (gate transcript): hidden-package, note repeated.
hiddenBlob :: Text
hiddenBlob =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":null,\
    \\"severity\":\"Error\",\"code\":87110,\"message\":[\"Could not load module \
    \`Network.HTTP.Client'.\\nIt is a member of the hidden package \
    \`http-client-0.7.19'.\\nYou can run `:set -package http-client' to expose \
    \it.\\n(Note: this unloads all the modules in the current scope.)\\nIt is a \
    \member of the hidden package `http-client-0.7.19'.\\nYou can run `:set \
    \-package http-client' to expose it.\\n(Note: this unloads all the modules \
    \in the current scope.)\\nIt is a member of the hidden package \
    \`http-client-0.7.19'.\\nYou can run `:set -package http-client' to expose \
    \it.\\n(Note: this unloads all the modules in the current scope.)\"],\
    \\"hints\":[]}"

-- Verbatim WIRE fixture (bench transcript): could-not-find + did-you-mean.
didYouMeanBlob :: Text
didYouMeanBlob =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":null,\
    \\"severity\":\"Error\",\"code\":61948,\"message\":[\"Could not find module \
    \`Data.Frame'.\\nPerhaps you meant\\n  DataFrame (needs flag -package-id \
    \dataframe-0.7.0.0)\\n  DataFrame (needs flag -package-id \
    \dataframe-0.3.3.7)\\n  DataFrame (needs flag -package-id \
    \dataframe-1.0.0.0)\"],\"hints\":[]}"

-- Same wire shapes, DIFFERENT subject (the laws must generalise past the
-- eval corpus's packages).
hiddenBlobOther :: Text
hiddenBlobOther =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":null,\
    \\"severity\":\"Error\",\"code\":87110,\"message\":[\"Could not load module \
    \`Text.Pretty.Simple'.\\nIt is a member of the hidden package \
    \`pretty-simple-4.1.2.3'.\\nYou can run `:set -package pretty-simple' to \
    \expose it.\\n(Note: this unloads all the modules in the current \
    \scope.)\"],\"hints\":[]}"

spec :: Spec
spec = describe "Sabela.AI.Capabilities.BrowseCard" $ do
    describe "hidden-package diagnostics distil to one actionable card" $ do
        it "the verbatim 11x-repeated wire fixture becomes one package + cabal line" $ do
            let card = browseCard "Network.HTTP.Client" hiddenBlob
            field "status" card `shouldBe` Just (String "hidden-package")
            field "package" card `shouldBe` Just (String "http-client")
            field "cabal" card
                `shouldBe` Just (String "-- cabal: build-depends: http-client")
            -- The raw blob must NOT ride along in any field.
            T.isInfixOf "ghcVersion" (T.pack (show card)) `shouldBe` False
        it "generalises to any package (same shape, different subject)" $ do
            let card = browseCard "Text.Pretty.Simple" hiddenBlobOther
            field "package" card `shouldBe` Just (String "pretty-simple")
            field "cabal" card
                `shouldBe` Just (String "-- cabal: build-depends: pretty-simple")

    describe "did-you-mean diagnostics distil to suggestions" $ do
        it "the verbatim Data.Frame fixture names module + package once" $ do
            let card = browseCard "Data.Frame" didYouMeanBlob
            field "status" card `shouldBe` Just (String "not-found")
            field "suggestions" card
                `shouldBe` Just (Array (pure (String "DataFrame")))
            field "package" card `shouldBe` Just (String "dataframe")
            field "cabal" card
                `shouldBe` Just (String "-- cabal: build-depends: dataframe")

    describe "a successful browse is a ranked, small-capped listing (R3.4)" $ do
        it "exports ride as a small capped array with shown/more/total disclosed" $ do
            let raw = T.unlines ["export" <> T.pack (show i) <> " :: Int" | i <- [1 .. 70 :: Int]]
                card = browseCard "Some.Module" raw
            field "status" card `shouldBe` Just (String "ok")
            case field "exports" card of
                Just (Array a) -> length a `shouldBe` 24
                other -> expectationFailure ("no exports array: " <> show other)
            field "more" card `shouldBe` Just (Number 46)
            field "total" card `shouldBe` Just (Number 70)
        it "a short listing has no overflow note but still a total" $ do
            let card = browseCard "Tiny.Module" "one :: Int\ntwo :: Int"
            field "more" card `shouldBe` Nothing
            field "total" card `shouldBe` Just (Number 2)
        it "ranks value signatures above declarations, internals last" $ do
            let raw =
                    T.unlines
                        [ "_hidden :: Int"
                        , "data Foo"
                        , "bar :: Int -> Int"
                        ]
                card = browseCard "Ranked.Module" raw
            case field "exports" card of
                Just (Array a) ->
                    [s | String s <- foldr (:) [] a]
                        `shouldBe` ["bar :: Int -> Int", "data Foo", "_hidden :: Int"]
                other -> expectationFailure ("no exports array: " <> show other)

    describe "unhelpful errors mark the card unusable (caller falls back)" $
        it "an unrelated error is status=error with ONE deduped line" $ do
            let card =
                    browseCard
                        "Whatever"
                        "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\
                        \\"span\":null,\"severity\":\"Error\",\"code\":1,\
                        \\"message\":[\"parse error on input\\nparse error on input\"],\
                        \\"hints\":[]}"
            field "status" card `shouldBe` Just (String "error")
            field "message" card `shouldBe` Just (String "parse error on input")

    describe "packageOfUnit — versioned unit ids reduce to package names" $ do
        it "strips a numeric version tail" $ do
            packageOfUnit "http-client-0.7.19" `shouldBe` "http-client"
            packageOfUnit "dataframe-0.7.0.0" `shouldBe` "dataframe"
            packageOfUnit "pretty-simple-4.1.2.3" `shouldBe` "pretty-simple"
        it "leaves an unversioned name alone" $
            packageOfUnit "containers" `shouldBe` "containers"
