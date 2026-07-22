{-# LANGUAGE OverloadedStrings #-}

{- | The diagnostic-set health model that decides whether a repair candidate is
kept. Pins the rule that comparing sets, not error counts, rejects a candidate
that trades one error for another.
-}
module Test.HealthSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Health (
    Health (..),
    healthOfResult,
    healthOfTypeQuery,
    improvesHealth,
    improvesHealthFor,
    isClean,
    normalizeMsg,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..), bareCellError)

cellErr :: Text -> CellError
cellErr = bareCellError Nothing Nothing

result :: Maybe Text -> [CellError] -> Either Text ExecutionResult
result err errs = Right (ExecutionResult [] err errs [])

spec :: Spec
spec = describe "Sabela.AI.Health" $ do
    describe "healthOfResult" $ do
        it "a Left is not clean and carries a sentinel diagnostic" $ do
            let h = healthOfResult (Left "Cancelled")
            healthCompileOk h `shouldBe` False
            Set.size (healthDiagnostics h) `shouldBe` 1
        it "a clean Right compiles ok with no diagnostics" $ do
            let h = healthOfResult (result Nothing [])
            isClean h `shouldBe` True
        it "keys on the structured errors, not the volatile joined blob" $ do
            -- The holistic message is the JOIN of the structured errors, so it
            -- shifts whenever any sibling's inferred type shifts — it must not
            -- be its own diagnostic key when structured errors exist.
            let er = ExecutionResult [] (Just "boom") [cellErr "a", cellErr "b"] [cellErr "w"]
                h = healthOfResult (Right er)
            healthCompileOk h `shouldBe` False
            Set.size (healthDiagnostics h) `shouldBe` 2
        it "keeps the holistic error when there are no structured ones" $ do
            let h = healthOfResult (result (Just "boom") [])
            Set.size (healthDiagnostics h) `shouldBe` 1

    describe "improvesHealth" $ do
        it "keeps a candidate that removes an error and adds none" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a", cellErr "b"]))
                (healthOfResult (result Nothing [cellErr "a"]))
                `shouldBe` True
        it "keeps a candidate that compiles clean" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a"]))
                (healthOfResult (result Nothing []))
                `shouldBe` True
        it "REJECTS trading one error for a different one (count would accept)" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a", cellErr "b"]))
                (healthOfResult (result Nothing [cellErr "a", cellErr "c"]))
                `shouldBe` False
        it "rejects an unchanged non-clean result" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a"]))
                (healthOfResult (result Nothing [cellErr "a"]))
                `shouldBe` False
        it "rejects a regression to an abort (Left)" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a"]))
                (healthOfResult (Left "Cancelled"))
                `shouldBe` False

    describe "inference-volatile diagnostics key on the stable part" $ do
        -- Fixing one name rewrites the TYPES GHC prints inside a SIBLING
        -- not-in-scope error (measured: chainl1's goal referenced Ghci20.Parser
        -- and shifted per run). The sibling must read as the SAME fact, or
        -- every partial heal is rejected as "a new diagnostic appeared".
        let chainl1V1 =
                cellErr
                    "Variable not in scope:\n  chainl1\n    :: (Text -> Either Text Term)\n       -> Ghci20.Parser (Double -> Double -> Double) -> Parser Double"
            chainl1V2 =
                cellErr
                    "Variable not in scope:\n  chainl1\n    :: t0 -> Parser (Double -> Double -> Double) -> Parser Double"
            takeW =
                cellErr
                    "Variable not in scope:\n  takeWhile1\n    :: (Char -> Bool) -> Parser String\nPerhaps use `takeWhileP' (imported from Text.Megaparsec)"
        it "keeps a heal that removes one name while a sibling's type text shifts" $
            improvesHealth
                (healthOfResult (result Nothing [takeW, chainl1V1]))
                (healthOfResult (result Nothing [chainl1V2]))
                `shouldBe` True
        it "still rejects a genuinely new error class" $
            improvesHealth
                (healthOfResult (result Nothing [chainl1V1]))
                ( healthOfResult
                    ( result
                        Nothing
                        [chainl1V2, cellErr "Couldn't match type: Int with: Text"]
                    )
                )
                `shouldBe` False
        it "did-you-mean hints are not part of the key either" $
            improvesHealth
                (healthOfResult (result Nothing [takeW, chainl1V1]))
                (healthOfResult (result Nothing [takeW]))
                `shouldBe` True

    describe "phase-ordered acceptance — a reveal is not a regression" $ do
        -- Healing a scope error lets its declaration groups reach the TYPE
        -- CHECKER for the first time, surfacing latent errors that were always
        -- in the code. Advancing the scope frontier is progress even when it
        -- reveals type errors; an unchanged frontier falls back to the strict
        -- subset rule.
        let nsA = cellErr "Variable not in scope: takeWhile1 :: X"
            nsB = cellErr "Variable not in scope:\n  chainl1\n    :: Y"
            latent1 = cellErr "No instance for `Fractional Integer' arising from a use of `/'"
            latent2 = cellErr "No instance for `Monoid Integer' arising from a use of `mempty'"
        it "accepts a scope heal that reveals latent type errors" $
            improvesHealth
                (healthOfResult (result Nothing [nsA, nsB]))
                (healthOfResult (result Nothing [nsB, latent1, latent2]))
                `shouldBe` True
        it "rejects a candidate that introduces a NEW scope error" $
            improvesHealth
                (healthOfResult (result Nothing [nsA]))
                (healthOfResult (result Nothing [nsB]))
                `shouldBe` False
        it "unchanged scope frontier still requires a type-error subset" $ do
            improvesHealth
                (healthOfResult (result Nothing [nsB, latent1, latent2]))
                (healthOfResult (result Nothing [nsB, latent1]))
                `shouldBe` True
            improvesHealth
                (healthOfResult (result Nothing [nsB, latent1]))
                (healthOfResult (result Nothing [nsB, latent2]))
                `shouldBe` False

    describe "knock-on scope noise — cell-defined names are excluded" $ do
        -- Not-in-scope errors for names the CELL ITSELF defines flap with
        -- which declaration groups happened to compile; the failing group's
        -- own diagnostic is always separately present, so these are noise.
        let defined =
                Set.fromList
                    [ "parseTerm"
                    , "parseFactor"
                    , "parseExpr"
                    , "operatorAddSub"
                    , "operatorMulDiv"
                    , "parens"
                    ]
            nsD = cellErr . ("Variable not in scope: " <>)
            nsChainl1 = cellErr "Variable not in scope:\n  chainl1\n    :: t0 -> Parser Double"
            latent1 = cellErr "No instance for `Fractional Integer' arising from a use of `/'"
            latent2 = cellErr "No instance for `Monoid Integer' arising from a use of `mempty'"
        it "accepts an external scope heal despite cell-defined knock-ons flapping" $
            -- Pinned from the gemma evalExpr verify dump: healing chainl1 made
            -- knock-ons for operatorAddSub/operatorMulDiv/parens REAPPEAR.
            improvesHealthFor
                defined
                ( healthOfResult
                    ( result
                        Nothing
                        [ latent1
                        , latent2
                        , nsChainl1
                        , nsD "parseTerm"
                        , nsD "parseFactor"
                        , nsD "parseExpr"
                        ]
                    )
                )
                ( healthOfResult
                    ( result
                        Nothing
                        [ latent1
                        , latent2
                        , nsD "parseTerm"
                        , nsD "operatorAddSub"
                        , nsD "parseFactor"
                        , nsD "operatorMulDiv"
                        , nsD "parens"
                        , nsD "parseExpr"
                        ]
                    )
                )
                `shouldBe` True
        it "still rejects a NEW scope error for a name the cell does not define" $
            improvesHealthFor
                defined
                (healthOfResult (result Nothing [nsChainl1]))
                (healthOfResult (result Nothing [nsD "attoparsecOnly"]))
                `shouldBe` False
        it "rejects when nothing external changes and only knock-ons flap" $
            improvesHealthFor
                defined
                (healthOfResult (result Nothing [nsChainl1, nsD "parseTerm"]))
                (healthOfResult (result Nothing [nsChainl1, nsD "parens"]))
                `shouldBe` False
        it "knock-ons do not block a type-error subset improvement either" $
            improvesHealthFor
                defined
                (healthOfResult (result Nothing [latent1, latent2, nsD "parseTerm"]))
                (healthOfResult (result Nothing [latent1, nsD "parens"]))
                `shouldBe` True
        it "with no defined names it is exactly improvesHealth" $
            improvesHealthFor
                Set.empty
                (healthOfResult (result Nothing [nsChainl1, nsD "parseTerm"]))
                (healthOfResult (result Nothing [nsD "parseTerm"]))
                `shouldBe` True

    describe "healthOfTypeQuery" $ do
        it "treats a returned type signature as clean" $
            isClean (healthOfTypeQuery "foldr (+) 0 :: (Foldable t, Num b) => t b -> b")
                `shouldBe` True
        it "treats a structured json diagnostic as red" $
            isClean
                ( healthOfTypeQuery
                    "{\"severity\":\"Error\",\"message\":[\"No instance for IsString Int\"]}"
                )
                `shouldBe` False
        it "treats a textual error: as red" $
            isClean (healthOfTypeQuery "<interactive>:1:1: error: not in scope: foo")
                `shouldBe` False

    describe "normalizeMsg" $
        it "collapses whitespace so reflowed messages compare equal" $
            normalizeMsg "not   in\nscope" `shouldBe` normalizeMsg "not in scope"
