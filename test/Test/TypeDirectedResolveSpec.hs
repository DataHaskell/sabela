{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for TYPE-DIRECTED resolution.

The gemma4 evalExpr transcripts showed keyword resolution pull attoparsec's
@takeWhile1 :: (Char -> Bool) -> Parser Text@ into a megaparsec cell — because
it matched the NAME. But the not-in-scope error already carries the goal type
(@… -> ParsecT Void String … String@) and the cell already imports megaparsec.
A type-directed resolver should use the goal type + the cell's ecosystem to find
a COMPATIBLE function (or decline), not cross-import an incompatible package by
name.

These pin what the pieces already do and mark, as @pendingWith@, the behaviour
the type-directed work must deliver — the executable statement of intent.
-}
module Test.TypeDirectedResolveSpec (spec) where

import Sabela.AI.Capabilities.Edit.Repair (goalOfName, notInScopeNames)
import Sabela.AI.HoleRepair (goalFromError)
import Sabela.AI.HoogleResolve (HoogleHit (..), rankResolveTopK)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (bareCellError)
import Test.Hspec

-- | A hoogle exact-name hit: name, package, module, type, docs.
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
            -- GHC prints the real form across lines; type-directed search needs
            -- the WHOLE arrow type (the ParsecT tail is what excludes attoparsec),
            -- not "(Char -> Bool)" alone.
            let err =
                    "Variable not in scope:\n\
                    \  takeWhile1\n\
                    \    :: (Char -> Bool)\n\
                    \       -> ParsecT Void String Identity String"
            (snd <$> goalFromError err)
                `shouldBe` Just "(Char -> Bool) -> ParsecT Void String Identity String"

        it "does NOT absorb GHC's trailing did-you-mean hint into the goal type" $ do
            -- A polluted goal string zero-hits every downstream type search,
            -- silently making the whole tier inert.
            let err =
                    "Variable not in scope:\n\
                    \  takeWhile1\n\
                    \    :: (Char -> Bool)\n\
                    \       -> ParsecT Void String Identity String\n\
                    \    • Perhaps use `takeWhile' (imported from Prelude)"
            (snd <$> goalFromError err)
                `shouldBe` Just "(Char -> Bool) -> ParsecT Void String Identity String"

        it "does not fabricate a goal from a later :: in context prose" $
            -- The name slot must be an identifier next to the error, not a
            -- token scavenged out of an \"In the expression: foo 3 :: Int\" line.
            goalFromError
                "Variable not in scope: foo\n    • In the expression: foo 3 :: Int"
                `shouldBe` Nothing

    describe "notInScopeNames — the resolver tier's trigger harvest" $ do
        -- GHC's MULTI-LINE not-in-scope form is the common one; a per-line
        -- harvest misses it and silently no-ops the whole resolver tier.
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
            -- Hunting an import for a knock-on committed rzk's parseTerm for a
            -- cell's OWN parseTerm; a cell-defined name is never a target.
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
            -- Matching only the first error left later names goal-less, and a
            -- goal-less candidate bypassed the scratch vet (ReadP's chainl1
            -- was committed unvetted this way).
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

    describe "resolution should respect the cell's ecosystem, not just the name" $ do
        it "prefers an ecosystem package over a niche one for the same name" $ do
            -- Foundation that already holds (the ranker's ecosystemScore): given
            -- the same name in two packages, the well-known one wins.
            let hits =
                    [ HoogleHit "decode" "obscure-thing-9000" "Some.Niche" "..." ""
                    , HoogleHit "decode" "aeson" "Data.Aeson" "..." ""
                    ]
            map fst (rankResolveTopK 3 "decode" Nothing hits)
                `shouldBe` ["aeson", "obscure-thing-9000"]

        it "demotes a type-incompatible hit below a type-matching one" $ do
            -- attoparsec's takeWhile1 (Parser Text) mismatches a megaparsec
            -- ParsecT goal, so it sorts below any matching hit. The DECLINE
            -- itself is the scratch vet's job — the compiler is the oracle
            -- there, not this text heuristic (pinned live, not here).
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
            -- The accept quadrant: a gate that dropped everything under Just goal
            -- would pass the decline test above and kill all name repair silently.
            let goal = "(Char -> Bool) -> Parser Text"
            rankResolveTopK 3 "takeWhile1" (Just goal) [hit]
                `shouldBe` [("attoparsec", "Data.Attoparsec.Text")]

        it "never gates on a polymorphic goal head" $ do
            let goal = "(Char -> Bool) -> f a"
            rankResolveTopK 3 "takeWhile1" (Just goal) [hit]
                `shouldBe` [("attoparsec", "Data.Attoparsec.Text")]
