{-# LANGUAGE OverloadedStrings #-}

{- | R3-T5: ONE repair cascade dispatched by diagnostic class (goal-1
invariant: the tier is a function of the class alone, never a library name),
the notebook-scope acceptance law (R7.5), the heal/search findability
cross-check (R7.6), budget visibility (R7.7/R7.3) and the report leak
invariant (R3.9).
-}
module Test.RepairDispatchSpec (repairDispatchSpec) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Health (DiagnosticKey (..), Health (..))
import Sabela.AI.RepairDispatch (
    DiagClass (..),
    RepairReport (..),
    RepairTier (..),
    acceptRepair,
    classifyDiag,
    renderRepairReport,
    reportCharBudget,
    tierRequiresRestart,
    tiersFor,
 )
import Siza.Agent.RepairTiers (
    Candidate (..),
    TierInput (..),
    candidatesFor,
 )
import Test.DiscoverFixtures (
    installNamesFileWith,
    runCat,
    stateOf,
    synHackageNames,
 )

-- | Diagnostic templates: (class, builder over a library/module name pair).
templates :: [(DiagClass, (Text, Text) -> Text)]
templates =
    [
        ( ClassHiddenPackage
        , \(p, m) ->
            "Could not load module ‘"
                <> m
                <> "’\nIt is a member of the hidden package ‘"
                <> p
                <> "-0.7.19’."
        )
    ,
        ( ClassModuleNotFound
        , \(_, m) -> "Could not find module ‘" <> m <> "’"
        )
    ,
        ( ClassMissingExtension
        , \(_, m) ->
            "Illegal lambda-case (use ‘"
                <> m
                <> "’?)\n  Perhaps you intended to use LambdaCase"
        )
    ,
        ( ClassAmbiguous
        , \(_, m) ->
            "Ambiguous occurrence ‘take’\nIt could refer to either ‘Prelude.take’,"
                <> " imported from ‘Prelude’ or ‘"
                <> m
                <> ".take’"
        )
    ,
        ( ClassNotInScope
        , \(_, m) ->
            "Variable not in scope: gust :: Int -> " <> m <> ".Wind"
        )
    ,
        ( ClassArity
        , \(_, m) ->
            "• Couldn't match expected type: (a -> b) -> ["
                <> m
                <> ".Wind] -> b\n  with actual type: Int"
        )
    ,
        ( ClassRefinement
        , \(_, m) -> "Found hole: _ :: " <> m <> ".Wind -> Int"
        )
    ]

libNames :: [(Text, Text)]
libNames =
    [ ("alpha-lib", "Alpha.Core")
    , ("granite", "Granite.Svg")
    , ("http-client", "Network.HTTP.Client")
    , ("dataframe", "DataFrame.Functions")
    ]

clean :: Health
clean = Health True Set.empty

red :: [Text] -> Health
red ms =
    Health False (Set.fromList [DiagnosticKey Nothing Nothing m | m <- ms])

repairDispatchSpec :: Spec
repairDispatchSpec = describe "repair cascade dispatch (R3-T5)" $ do
    describe "dispatch purity: tier is a function of the class alone" $ do
        it "library-name substitution never changes the class or tiers" $
            forM_ templates $ \(cls, mk) ->
                forM_ libNames $ \names -> do
                    classifyDiag (mk names) `shouldBe` cls
                    tiersFor (classifyDiag (mk names)) `shouldBe` tiersFor cls
        it "every diagnostic class routes to a total tier list" $
            forM_ [minBound .. maxBound :: DiagClass] $ \cls ->
                length (tiersFor cls) `shouldSatisfy` (>= 0)
        it "every class except Other is reachable from a template" $ do
            let covered = map fst templates
            forM_ [minBound .. maxBound :: DiagClass] $ \cls ->
                (cls == ClassOther || cls `elem` covered) `shouldBe` True
        it "only the dep-add tier requires a restart (R7.3)" $
            [t | t <- [minBound .. maxBound], tierRequiresRestart t]
                `shouldBe` [TierDepAdd]

    describe "acceptance law over generated notebooks (R7.5)" $ do
        let target = "1" :: Text
            sib = "2" :: Text
            notebookOf tgt s = [(target, tgt), (sib, s)]
            targetStates =
                [ ("healed", red ["e1", "e2"], clean, True)
                , ("improved", red ["e1", "e2"], red ["e1"], True)
                , ("unchanged", red ["e1"], red ["e1"], False)
                , ("worsened", red ["e1"], red ["e1", "e3"], False)
                ]
            sibStates =
                [ ("stable", clean, clean, True)
                , ("regressed", clean, red ["boom"], False)
                , ("still red", red ["s1"], red ["s1"], True)
                ,
                    ( "knock-on only"
                    , clean
                    , red ["Variable not in scope: gustTotal"]
                    , True
                    )
                ]
        forM_ targetStates $ \(tn, t0, t1, tOk) ->
            forM_ sibStates $ \(sn, s0, s1, sOk) ->
                it ("target " <> tn <> " / sibling " <> sn) $
                    acceptRepair
                        (Set.singleton "gustTotal")
                        (notebookOf t0 s0)
                        (notebookOf t1 s1)
                        target
                        `shouldBe` (tOk && sOk)

    describe "heal and search never disagree (R7.6)" $
        it "every proposed name is findable by discover on the same catalogue" $ do
            installNamesFileWith synHackageNames
            let diag =
                    "Variable not in scope: gustt :: Int -> Wind\n"
                        <> "  Perhaps use `gust' (imported from Zephyr.Core)"
                input =
                    TierInput
                        { tiDiag = diag
                        , tiSource = "total = gustt 3"
                        , tiHoleFits = ""
                        , tiLocate = \n -> [("Zephyr.Core", Nothing) | n == "gustt"]
                        , tiModules = const []
                        }
                cands = candidatesFor (tiersFor ClassNotInScope) input
            cands `shouldSatisfy` (not . null)
            forM_ [n | c <- cands, n <- cdProposes c, nameLike n] $ \n -> do
                v <- runCat n
                stateOf v `shouldBe` "found"

    describe "budget visibility (R7.7) and the leak invariant (R3.9)" $ do
        it "the report states attempts run and why the cascade stopped" $
            forM_ [0 .. 5 :: Int] $ \k -> do
                let rep =
                        RepairReport
                            ClassNotInScope
                            k
                            4
                            "all candidates reverted"
                            Nothing
                            []
                    txt = renderRepairReport rep
                txt `shouldSatisfy` T.isInfixOf (T.pack (show k))
                txt `shouldSatisfy` T.isInfixOf "reverted"
        it "restart-requiring repairs are disclosed as unvalidated (R7.3)" $ do
            let rep =
                    RepairReport
                        ClassHiddenPackage
                        1
                        4
                        "kept"
                        (Just (TierDepAdd, "-- cabal: build-depends: x"))
                        ["-- cabal: build-depends: x"]
            renderRepairReport rep `shouldSatisfy` T.isInfixOf "unvalidated"
        it "the report stays within budget for any K and leaks no candidate" $
            forM_ [1, 10, 100 :: Int] $ \k -> do
                let long = T.replicate 500 "z"
                    rep =
                        RepairReport
                            ClassNotInScope
                            k
                            k
                            "budget exhausted"
                            (Just (TierNameResolve, long))
                            []
                    txt = renderRepairReport rep
                T.length txt `shouldSatisfy` (<= reportCharBudget)
                -- the report type cannot even hold rejected candidates
                txt `shouldSatisfy` (not . T.isInfixOf (T.replicate 300 "z"))
  where
    nameLike = T.all (\c -> c `elem` ("abcdefghijklmnopqrstuvwxyz" :: String))
