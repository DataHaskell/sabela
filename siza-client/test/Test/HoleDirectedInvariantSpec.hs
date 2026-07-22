{-# LANGUAGE OverloadedStrings #-}

{- | R10-T1 general invariants: the hole-fit / type-directed repair on a
self-authored red cell, decided by the diagnostic CLASS alone (never a library
name). Over a synthetic catalogue of INVENTED types (Zephyr/Cumulus/Nimbus)
every @Found hole: _ :: T@ classifies identically, yields a candidate per named
fit, and — for an EMPTY-fit hole — still yields a type-directed candidate from
the queried producers. The keep/revert gate is 'acceptRepair' (compiler-as-
verifier); a sibling regression is reverted. Grow generators, not control flow.
-}
module Test.HoleDirectedInvariantSpec (holeDirectedInvariantSpec) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Health (DiagnosticKey (..), Health (..))
import Sabela.AI.RepairDispatch (
    DiagClass (..),
    RepairTier (..),
    acceptRepair,
    classifyDiag,
    tiersFor,
 )
import Siza.Agent.RepairTiers (
    Candidate (..),
    TierInput (..),
    candidatesFor,
    tierCandidates,
 )

-- | Invented types, never a real library — applicability is by class, not name.
synTypes :: [Text]
synTypes = ["Zephyr", "Cumulus", "Nimbus"]

-- | A nullary producer name for a synthetic type.
fitName :: Text -> Text
fitName ty = "mk" <> ty

-- | A literal Found-hole diagnostic for a hole of the synthetic type.
holeDiag :: Text -> Text
holeDiag ty =
    "cell 1, line 2: Found hole: _ :: "
        <> ty
        <> "\n  In the first argument of `render', namely `(_ :: "
        <> ty
        <> ")'"

-- | GHC's valid-hole-fits blob for a hole of the synthetic type (two producers).
fitBlob :: Text -> Text
fitBlob ty =
    T.unlines
        [ "Valid hole fits include"
        , "  " <> fitName ty <> " :: " <> ty
        , "  alt" <> ty <> " :: Int -> " <> ty
        ]

inputFor :: Text -> Text -> TierInput
inputFor diag blob =
    TierInput
        { tiDiag = diag
        , tiSource = "cell = render _"
        , tiHoleFits = blob
        , tiLocate = const []
        , tiModules = const []
        }

holeDirectedInvariantSpec :: Spec
holeDirectedInvariantSpec =
    describe "R10-T1 hole-fit / type-directed repair invariants" $ do
        classInvariantSpec
        emptyFitSpec
        acceptGateSpec

-- The trigger is decided by class, identically for every synthetic type -------

classInvariantSpec :: Spec
classInvariantSpec =
    describe "trigger fingerprint identical for every synthetic type" $ do
        it "classifies every synthetic Found-hole as ClassRefinement" $
            forM_ synTypes $ \ty ->
                classifyDiag (holeDiag ty) `shouldBe` ClassRefinement
        it "yields a substituting candidate per named fit, every type" $
            forM_ synTypes $ \ty -> do
                let cands =
                        candidatesFor
                            (tiersFor ClassRefinement)
                            (inputFor (holeDiag ty) (fitBlob ty))
                cands `shouldSatisfy` (not . null)
                map cdSource cands
                    `shouldSatisfy` any (T.isInfixOf (fitName ty))
                concatMap cdProposes cands `shouldContain` [fitName ty]
        it "every candidate is a genuine rewrite of the source" $
            forM_ synTypes $ \ty -> do
                let cands =
                        candidatesFor
                            (tiersFor ClassRefinement)
                            (inputFor (holeDiag ty) (fitBlob ty))
                map cdSource cands `shouldSatisfy` notElem "cell = render _"

-- A diagnostic with no inline fits can use queried producers (R7.5) ----------

emptyFitSpec :: Spec
emptyFitSpec =
    describe "no-inline-fit hole engages TierTypeDirected" $ do
        it "ClassRefinement dispatches to the type-directed tier" $
            tiersFor ClassRefinement `shouldContain` [TierTypeDirected]
        it "a no-inline-fit diagnostic yields candidates from a non-empty producer query" $
            forM_ synTypes $ \ty -> do
                -- The diagnostic carries NO fits; the producers arrive from the
                -- queried find_by_type blob (buildInput's queryHoleFits path).
                let cands =
                        tierCandidates
                            TierTypeDirected
                            (inputFor (holeDiag ty) (fitBlob ty))
                cands `shouldSatisfy` (not . null)
                map cdSource cands
                    `shouldSatisfy` any (T.isInfixOf (fitName ty))
        it "yields nothing when neither diagnostic nor blob names the goal" $
            forM_ synTypes $ \ty ->
                tierCandidates
                    TierTypeDirected
                    (inputFor (holeDiag ty) "")
                    `shouldBe` []

-- The keep/revert gate is the compiler-as-verifier acceptRepair --------------

acceptGateSpec :: Spec
acceptGateSpec =
    describe "acceptRepair gate over synthetic health snapshots" $ do
        let tgt = "1" :: Text
            sib = "2" :: Text
            note ms = [DiagnosticKey Nothing Nothing m | m <- ms]
            red ms = Health False (Set.fromList (note ms))
            clean = Health True Set.empty
            snap t s = [(tgt, t), (sib, s)]
        it "keeps a candidate that heals the hole cell" $
            acceptRepair
                Set.empty
                (snap (red ["Found hole: _ :: Zephyr"]) clean)
                (snap clean clean)
                tgt
                `shouldBe` True
        it "reverts a candidate that regresses a sibling" $
            acceptRepair
                Set.empty
                (snap (red ["h"]) clean)
                (snap clean (red ["boom"]))
                tgt
                `shouldBe` False
        it "reverts a candidate that leaves the hole cell red" $
            acceptRepair
                Set.empty
                (snap (red ["h"]) clean)
                (snap (red ["h"]) clean)
                tgt
                `shouldBe` False
