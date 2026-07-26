{-# LANGUAGE OverloadedStrings #-}

module Test.CandidateRankSpec (candidateRankSpec) where

import Control.Monad (forM_)
import Data.List (minimumBy, nub)
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Candidate (
    candidateCell,
    candidateCellFrom,
    candidateClauseFrom,
 )
import Siza.Agent.Discover.Goal (argTypesOf, genuineGaps, literalFill)

consumerFact :: Text -> Text -> Text -> Text -> Text
consumerFact name sig m pkg =
    "`" <> name <> "` :: " <> sig <> " — found in " <> m <> " (" <> pkg <> ")"

seedName :: Text -> Maybe Text
seedName src = case reverse (T.lines src) of
    (appLine : _) -> case T.words appLine of
        (h : _) -> Just h
        [] -> Nothing
    [] -> Nothing

type C = (Text, Text)

grid :: [[C]]
grid =
    [ [("act1", "A -> Text"), ("act2", "B -> C -> Text")]
    , [("wide", "P -> Q -> R -> Text"), ("thin", "Int -> Text")]
    , [("mid", "S -> Int -> Text"), ("wider", "U -> V -> W -> Text")]
    ,
        [ ("bars", "[(Text, Double)] -> Plot -> Text")
        , ("chart", "D -> E -> F -> G -> H -> I -> J -> K -> Text")
        ]
    , [("z", "M -> Text"), ("y", "N -> Text"), ("x", "L -> Text")]
    ]

factsOf :: [C] -> [Text]
factsOf cs =
    [consumerFact n sig "Mod" "pkg" | (n, sig) <- cs]
        ++ map probedFact (gapTypes cs)

gapTypes :: [C] -> [Text]
gapTypes cs =
    nub [t | (_, sig) <- cs, t <- argTypesOf sig, isNothing (literalFill t)]

probedFact :: Text -> Text
probedFact t = "`" <> t <> "` is produced by: `mk" <> t <> "` (via: hole-probe)"

hasGap :: C -> Bool
hasGap (_, sig) = any (isNothing . literalFill) (argTypesOf sig)

expectedSeed :: [C] -> Text
expectedSeed cs = fst (minimumBy (comparing gaps) cs)
  where
    heldSigs = [sig | (_, sig) <- cs]
    gaps (_, sig) = length (genuineGaps heldSigs sig)

candidateRankSpec :: Spec
candidateRankSpec = describe "candidate-seed re-ranking (R9-T3)" $ do
    describe "fewest-genuine-gap seed over the generated grid" $
        it "the seed is the minimising consumer, ties broken by held order" $
            forM_ grid $ \cs -> do
                let facts = factsOf cs
                case candidateCell facts >>= seedName of
                    Nothing -> expectationFailure ("no candidate: " <> show cs)
                    Just got -> (cs, got) `shouldBe` (cs, expectedSeed cs)

    describe "an unprobed gap proposes nothing at all (G3)" $
        it "a ledger whose every consumer has a gap yields no candidate" $
            forM_ [cs | cs <- grid, all hasGap cs] $ \cs ->
                candidateCell [consumerFact n sig "Mod" "pkg" | (n, sig) <- cs]
                    `shouldBe` Nothing

    describe "the 1-gap bars vs 8-gap record-stub fixture" $
        it "bars (1 genuine gap) wins over the wide record stub (8)" $ do
            let facts =
                    factsOf
                        [ ("chart", "D -> E -> F -> G -> H -> I -> J -> K -> Text")
                        , ("bars", "[(Text, Double)] -> Plot -> Text")
                        ]
            (candidateCell facts >>= seedName) `shouldBe` Just "bars"

    describe "a held model draft seeds the candidate verbatim" $ do
        it "candidateCellFrom prefers a non-blank draft over the ledger seed" $ do
            let draft = "import Granite.Svg\nbars myData (barPlot 400 300)\n"
                facts = factsOf [("bars", "[(Text, Double)] -> Plot -> Text")]
            candidateCellFrom (Just draft) facts
                `shouldBe` Just (T.stripEnd draft)
        it "a blank draft falls back to the ranked ledger seed" $ do
            let facts = factsOf [("bars", "[(Text, Double)] -> Plot -> Text")]
            (candidateCellFrom (Just "   \n") facts >>= seedName)
                `shouldBe` Just "bars"
        it "the clause carries the drafted source, and never a hole" $ do
            let draft = "bars myData (barPlot 400 300)"
                clause = candidateClauseFrom (Just draft) []
            clause `shouldSatisfy` T.isInfixOf draft
            clause `shouldSatisfy` (not . T.isInfixOf "(_ :: ")
        it "a draft the model holed itself is not a usable seed (G3)" $
            candidateClauseFrom (Just "bars myData (_ :: Plot)") [] `shouldBe` ""
