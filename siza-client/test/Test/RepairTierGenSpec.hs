{-# LANGUAGE OverloadedStrings #-}

{- | R4-T3(b): the two formerly-dead repair tiers become real pure generators
(search-api.md section 7.1, RepairDispatch.tiersFor already advertises them):
the type-directed tier proposes catalogue bindings of the goal type, the arity
tier permutes/re-groups applied arguments. Both are safe by construction under
'acceptRepair' (R7.5), discover-findable (R7.6), and report within budget for
adversarial K (R7.7). Grow generators, not control flow.
-}
module Test.RepairTierGenSpec (repairTierGenSpec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.RepairDispatch (
    DiagClass (..),
    RepairTier (..),
    classifyDiag,
    tiersFor,
 )
import Siza.Agent.RepairTiers (
    Candidate (..),
    TierInput (..),
    candidatesFor,
    tierCandidates,
 )
import Test.DiscoverFixtures (
    installNamesFileWith,
    runCat,
    stateOf,
    synHackageNames,
 )

repairTierGenSpec :: Spec
repairTierGenSpec = describe "type-directed and arity tier generators (R4-T3b)" $ do
    typeDirectedSpec
    aritySpec
    findabilitySpec
    livenessSpec

-- A hole-fit blob GHC would print for a goal-type search over the catalogue.
plotBlob :: Text
plotBlob =
    T.unlines
        [ "Valid hole fits include"
        , "  defaultPlot :: Plot"
        , "  emptyStyle :: Style"
        ]

typeInput :: Text -> Text -> TierInput
typeInput diag src =
    TierInput
        { tiDiag = diag
        , tiSource = src
        , tiHoleFits = plotBlob
        , tiLocate = const []
        , tiModules = const []
        }

-- The type-directed tier ----------------------------------------------------

typeDirectedSpec :: Spec
typeDirectedSpec = describe "TierTypeDirected proposes a goal-typed binding" $ do
    let diag =
            "Variable not in scope: thePlot :: Plot\n"
                <> "  Perhaps you meant something typed Plot"
        src = "chart = bars vals thePlot"
    it "substitutes a producer of the goal type for the wrong name" $ do
        let cands = tierCandidates TierTypeDirected (typeInput diag src)
        cands `shouldSatisfy` (not . null)
        map cdSource cands `shouldSatisfy` any (T.isInfixOf "defaultPlot")
        concatMap cdProposes cands `shouldContain` ["defaultPlot"]
    it "never proposes a fit of the WRONG type (only goal-typed producers)" $ do
        let cands = tierCandidates TierTypeDirected (typeInput diag src)
        map cdSource cands
            `shouldSatisfy` (not . any (T.isInfixOf "emptyStyle"))
    it "yields nothing when the diagnostic carries no goal type" $ do
        let cands =
                tierCandidates
                    TierTypeDirected
                    (typeInput "Variable not in scope: foo" src)
        cands `shouldBe` []
    it "is a rewrite (every candidate differs from the source)" $ do
        let cands = tierCandidates TierTypeDirected (typeInput diag src)
        map cdSource cands `shouldSatisfy` notElem src

-- The arity tier ------------------------------------------------------------

aritySpec :: Spec
aritySpec = describe "TierArity permutes/re-groups applied arguments" $ do
    let diag =
            "• Couldn't match expected type: Plot -> [(Text, Double)] -> Text\n"
                <> "  with actual type: Text"
        src = "chart = bars vals thePlot"
    it "proposes a permutation of the applied arguments" $ do
        let cands = tierCandidates TierArity (typeInput diag src)
        cands `shouldSatisfy` (not . null)
        map cdSource cands `shouldSatisfy` any (T.isInfixOf "bars thePlot vals")
    it "leaves the function head in place, only reordering its arguments" $ do
        let cands = tierCandidates TierArity (typeInput diag src)
        map cdSource cands `shouldSatisfy` all (T.isInfixOf "bars ")
    it "yields nothing for a non-function (nothing to permute)" $ do
        let cands =
                tierCandidates
                    TierArity
                    (typeInput "Couldn't match expected type: Int" src)
        cands `shouldBe` []
    it "every candidate is a genuine rewrite of the source" $ do
        let cands = tierCandidates TierArity (typeInput diag src)
        map cdSource cands `shouldSatisfy` notElem src
    it "the arity class dispatches to exactly the arity tier" $
        tiersFor (classifyDiag diag) `shouldBe` [TierArity]

-- R7.6 cross-check: every proposed name is discover-findable -----------------

findabilitySpec :: Spec
findabilitySpec = describe "R7.6: the new tiers propose only findable names" $
    it "every name the type-directed tier proposes is found by discover" $ do
        installNamesFileWith ("defaultPlot" : synHackageNames)
        let diag = "Variable not in scope: p :: Wind"
            input =
                TierInput
                    { tiDiag = diag
                    , tiSource = "x = f p"
                    , tiHoleFits =
                        T.unlines
                            ["Valid hole fits include", "  gust :: Int -> Wind"]
                    , tiLocate = const []
                    , tiModules = const []
                    }
            cands = tierCandidates TierTypeDirected input
        forM_ (concatMap cdProposes cands) $ \n -> do
            v <- runCat n
            (n, stateOf v) `shouldBe` (n, "found")

-- The tiers are no longer dead ----------------------------------------------

livenessSpec :: Spec
livenessSpec = describe "the advertised tiers are live (no []-returning tier)" $ do
    it "the not-in-scope cascade includes a live type-directed tier" $ do
        let diag = "Variable not in scope: thePlot :: Plot"
            input = typeInput diag "chart = bars vals thePlot"
            tiers = tiersFor ClassNotInScope
        tiers `shouldContain` [TierTypeDirected]
        candidatesFor tiers input `shouldSatisfy` (not . null)
    it "the arity cascade produces candidates end to end" $ do
        let diag = "Couldn't match expected type: B -> A -> C"
            input = typeInput diag "y = g a b"
        candidatesFor (tiersFor ClassArity) input
            `shouldSatisfy` (not . null)
