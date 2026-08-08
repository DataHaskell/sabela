{-# LANGUAGE OverloadedStrings #-}

module Test.ExemplarsSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Exemplars (
    Exemplar (..),
    exemplarMessage,
    retrieveExemplars,
 )

content :: Value -> Maybe T.Text
content (Object o) = case KM.lookup (K.fromText "content") o of
    Just (String s) -> Just s
    _ -> Nothing
content _ = Nothing

spec :: Spec
spec = describe "Siza.Agent.Exemplars (learning loop memory)" $ do
    describe "retrieveExemplars" $ do
        let store =
                [ Exemplar "total the revenue column of a dataframe csv" "A"
                , Exemplar "render a bar chart of monthly revenue" "B"
                ]
        it "ranks the most task-similar exemplar first" $
            map exSource (retrieveExemplars 1 "sum the revenue column from the csv" store)
                `shouldBe` ["A"]
        it "returns nothing when no exemplar clears the similarity floor" $
            retrieveExemplars 2 "quantum chromodynamics lattice simulation" store
                `shouldBe` []

    describe "exemplarMessage" $ do
        it "renders a fenced in-context example under a similar-task heading" $ do
            let body = case exemplarMessage [Exemplar "t" "x = 1"] of
                    (msg : _) -> content msg
                    [] -> Nothing
            (T.isInfixOf "verified working solution" <$> body) `shouldBe` Just True
            (T.isInfixOf "```haskell\nx = 1\n```" <$> body) `shouldBe` Just True
        it "is empty for no exemplars" $
            exemplarMessage [] `shouldBe` []
        it "carries no invent-ban boilerplate (R5.8: search misses can be wrong)" $ do
            let body = case exemplarMessage [Exemplar "t" "x = 1"] of
                    (msg : _) -> content msg
                    [] -> Nothing
            (T.isInfixOf "do not invent" . T.toLower <$> body) `shouldBe` Just False
            (T.isInfixOf "invent" . T.toLower <$> body) `shouldBe` Just False
