{-# LANGUAGE OverloadedStrings #-}

module Test.TypeDirectedResolveSpec (spec) where

import Sabela.AI.Capabilities.Edit.Repair (goalOfName, notInScopeNames)
import Sabela.AI.HoleRepair (goalFromError)
import Sabela.AI.HoogleResolve (HoogleHit (..), rankResolveTopK)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (bareCellError)
import Test.Hspec

hit :: HoogleHit
hit =
    HoogleHit
        "takeWhile1"
        "attoparsec"
        "Data.Attoparsec.Text"
        "(Char -> Bool) -> Parser Text"
        ""

spec :: Spec
spec = describe "type-directed resolution (intention)" $ do
    describe "the goal type is available in the not-in-scope error" $ do
        it "extracts name + goal type from a single-line error" $
            goalFromError
                "Variable not in scope: takeWhile1 :: (Char -> Bool) -> Parser Text"
                `shouldBe` Just ("takeWhile1", "(Char -> Bool) -> Parser Text")

        it "extracts the FULL multi-line goal type, not just the first line" $ do
            let err =
                    "Variable not in scope:\n\
                    \  takeWhile1\n\
                    \    :: (Char -> Bool)\n\
                    \       -> ParsecT Void String Identity String"
            (snd <$> goalFromError err)
                `shouldBe` Just "(Char -> Bool) -> ParsecT Void String Identity String"

        it "does NOT absorb GHC's trailing did-you-mean hint into the goal type" $ do
            let err =
                    "Variable not in scope:\n\
                    \  takeWhile1\n\
                    \    :: (Char -> Bool)\n\
                    \       -> ParsecT Void String Identity String\n\
                    \    • Perhaps use `takeWhile' (imported from Prelude)"
            (snd <$> goalFromError err)
                `shouldBe` Just "(Char -> Bool) -> ParsecT Void String Identity String"

        it "does not fabricate a goal from a later :: in context prose" $
            goalFromError
                "Variable not in scope: foo\n    • In the expression: foo 3 :: Int"
                `shouldBe` Nothing

    describe "notInScopeNames — the resolver tier's trigger harvest" $ do
        it "harvests the multi-line form" $ do
            let er =
                    ExecutionResult
                        []
                        Nothing
                        [ bareCellError
                            Nothing
                            Nothing
                            "Variable not in scope:\n  chainl1\n    :: Parser Double\n       -> Parser (Double -> Double -> Double) -> Parser Double"
                        ]
                        []
            notInScopeNames "" (Right er) `shouldBe` ["chainl1"]
        it "excludes names the cell itself defines (knock-on casualties)" $ do
            let er =
                    ExecutionResult
                        []
                        Nothing
                        [ bareCellError Nothing Nothing "Variable not in scope: parseTerm"
                        , bareCellError
                            Nothing
                            Nothing
                            "Variable not in scope:\n  chainl1\n    :: Parser Double -> Parser Double"
                        ]
                        []
            notInScopeNames "parseTerm = chainl1 f g" (Right er)
                `shouldBe` ["chainl1"]
        it "goalOfName finds a LATER diagnostic's goal, not just the first" $ do
            let er =
                    ExecutionResult
                        []
                        Nothing
                        [ bareCellError
                            Nothing
                            Nothing
                            "Variable not in scope: takeWhile1 :: (Char -> Bool) -> Parser String"
                        , bareCellError
                            Nothing
                            Nothing
                            "Variable not in scope:\n  chainl1\n    :: Parser Double -> Parser Double"
                        ]
                        []
            goalOfName (Right er) "chainl1"
                `shouldBe` Just "Parser Double -> Parser Double"
        it "still harvests the single-line form" $ do
            let er =
                    ExecutionResult
                        []
                        Nothing
                        [bareCellError Nothing Nothing "Variable not in scope: divvy"]
                        []
            notInScopeNames "" (Right er) `shouldBe` ["divvy"]

    describe "resolution should respect more than the name" $ do
        -- With no popularity table the modules tie on length, so the shortlist
        -- falls to module order; the prior itself is pinned in HoogleRankSpec.
        it "orders equally measured hits by module path, never by package" $ do
            let hits =
                    [ HoogleHit "decode" "obscure-thing-9000" "Some.Niche" "..." ""
                    , HoogleHit "decode" "aeson" "Data.Aeson" "..." ""
                    ]
            map fst (rankResolveTopK 3 "decode" Nothing hits)
                `shouldBe` ["aeson", "obscure-thing-9000"]

        it "demotes a type-incompatible hit below a type-matching one" $ do
            let goal = "(Char -> Bool) -> ParsecT Void String Identity String"
                matching =
                    HoogleHit
                        "takeWhile1"
                        "parser-x"
                        "Parser.X"
                        "(Char -> Bool) -> ParsecT Void String Identity String"
                        ""
            map fst (rankResolveTopK 3 "takeWhile1" (Just goal) [hit, matching])
                `shouldBe` ["parser-x", "attoparsec"]

        it "KEEPS a hit whose concrete result head matches the goal's" $ do
            let goal = "(Char -> Bool) -> Parser Text"
            rankResolveTopK 3 "takeWhile1" (Just goal) [hit]
                `shouldBe` [("attoparsec", "Data.Attoparsec.Text")]

        it "never gates on a polymorphic goal head" $ do
            let goal = "(Char -> Bool) -> f a"
            rankResolveTopK 3 "takeWhile1" (Just goal) [hit]
                `shouldBe` [("attoparsec", "Data.Attoparsec.Text")]
