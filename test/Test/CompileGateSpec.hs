{-# LANGUAGE OverloadedStrings #-}

module Test.CompileGateSpec (spec) where

import Data.Aeson (Value (..))
import Data.Foldable (toList)
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.CompileGate (compileGateSpec)
import Sabela.Deps (EnvSig (..))
import Sabela.Session.Materialize (
    CandidateSpec (..),
    buildBudgetFor,
    candidateSafetyPrelude,
    expressionCandidate,
 )
import Sabela.Session.Timeout (
    TimeoutConfig (..),
    defaultTimeoutConfig,
 )
import Sabela.State (App (..))
import Sabela.State.SessionManager (haskellEnvOf)
import Test.GateFixture (
    cellCount,
    field,
    generationOf,
    insertSrc,
    sessionIdentity,
    textField,
    withBuildTimeout,
    withFixture,
 )
import Test.Live (requireLiveIntegration)

{- | The dependencies the running kernel actually has, read from the environment
recorded against it at spawn.
-}
liveDeps :: App -> IO (Set.Set T.Text)
liveDeps app = maybe Set.empty (esDeps . snd) <$> haskellEnvOf (appSessions app)

spec :: Spec
spec = describe "G1 compile-gated notebook writes" $ do
    describe "gate-safe-haskell (live_test24, live_test33_wine)" $ do
        it "a deliberate commit is not judged under -XSafe" $ do
            let p = candidateSafetyPrelude (compileGateSpec Nothing "x = 1")
            p `shouldNotSatisfy` T.isInfixOf "-XSafe"

        it "a speculative trial is not judged under -XSafe either" $ do
            let p = candidateSafetyPrelude (expressionCandidate "1 + 1")
            p `shouldNotSatisfy` T.isInfixOf "-XSafe"

        it "both still drop unsafePerformIO from scope" $ do
            let gate = candidateSafetyPrelude (compileGateSpec Nothing "x = 1")
                trial = candidateSafetyPrelude (expressionCandidate "1 + 1")
            gate `shouldSatisfy` T.isInfixOf "-System.IO.Unsafe"
            trial `shouldSatisfy` T.isInfixOf "-System.IO.Unsafe"

    describe "dep-cell-deadlock (live_test21)" $ do
        it "a gate compile is a deliberate commit, never a trial" $
            candidateDeliberate
                (compileGateSpec Nothing "-- cabal: build-depends: dataframe\nx = 1")
                `shouldBe` True

        it "a deliberate commit gets the live build ceiling, not the trial one" $ do
            let tc = defaultTimeoutConfig
                gate = compileGateSpec Nothing "-- cabal: build-depends: dataframe"
            buildBudgetFor gate tc `shouldBe` tcBuildUs tc
            buildBudgetFor gate tc `shouldSatisfy` (> tcTryBuildUs tc)

        it "a speculative trial keeps the tighter budget, so it still fails fast" $
            buildBudgetFor (expressionCandidate "1 + 1") defaultTimeoutConfig
                `shouldBe` tcTryBuildUs defaultTimeoutConfig
    it
        "a non-compiling insert leaves the notebook, generation, and live session untouched"
        $ do
            requireLiveIntegration
            withFixture "sabela-compilegate-red" $ \(app, store, rn) -> do
                _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"
                countBefore <- cellCount app
                genBefore <- generationOf app
                sessionBefore <- sessionIdentity app

                let brokenSrc = "import Data.Maybe (fromJust)\nbroken = 1 +"
                ack <- insertSrc app store rn brokenSrc

                textField "notCommitted" ack `shouldBe` Just "compile-gate"
                textField "verdict" ack `shouldBe` Just "diagnostic"
                textField "diagnostic" ack `shouldSatisfy` maybe False (/= "")
                textField "source" ack `shouldBe` Just brokenSrc

                countAfter <- cellCount app
                genAfter <- generationOf app
                sessionAfter <- sessionIdentity app
                countAfter `shouldBe` countBefore
                genAfter `shouldBe` genBefore
                sessionAfter `shouldBe` sessionBefore

    it "commits a cell carrying a type signature (the live_test6 regression)" $ do
        requireLiveIntegration
        withFixture "sabela-compilegate-signature" $ \(app, store, rn) -> do
            countBefore <- cellCount app
            ack <- insertSrc app store rn "sabelaSigned :: Int\nsabelaSigned = 21 * 2"
            field "notCommitted" ack `shouldBe` Nothing
            textField "diagnostic" ack `shouldBe` Nothing
            field "cellId" ack `shouldSatisfy` (/= Nothing)
            cellCount app `shouldReturn` (countBefore + 1)

    it
        "a compiling insert commits and runs exactly as before (no gate-caused behaviour change)"
        $ do
            requireLiveIntegration
            withFixture "sabela-compilegate-green" $ \(app, store, rn) -> do
                countBefore <- cellCount app

                ack <- insertSrc app store rn "sabelaOk = (21 :: Int) * 2"

                field "error" ack `shouldBe` Nothing
                field "notCommitted" ack `shouldBe` Nothing
                field "cellId" ack `shouldSatisfy` (/= Nothing)
                countAfter <- cellCount app
                countAfter `shouldBe` countBefore + 1

    it
        "a dep-adding cell that fails to compile never triggers the live install/restart"
        $ do
            requireLiveIntegration
            withFixture "sabela-compilegate-dep-red" $ \(app, store, rn) -> do
                depsBefore <- liveDeps app

                ack <-
                    insertSrc
                        app
                        store
                        rn
                        "-- cabal: build-depends: containers\n\
                        \import qualified Data.Map.Strict as M\n\
                        \broken = M.notARealFunctionAnywhere"

                textField "notCommitted" ack `shouldBe` Just "compile-gate"
                textField "verdict" ack `shouldBe` Just "diagnostic"

                depsAfter <- liveDeps app
                depsAfter `shouldBe` depsBefore
                Set.member "containers" depsAfter `shouldBe` False
                count <- cellCount app
                count `shouldBe` 0

    it
        "the infra-failure path (gate build budget breached) returns the infra verdict and commits nothing"
        $ do
            requireLiveIntegration
            withBuildTimeout "1" $
                withFixture "sabela-compilegate-infra" $ \(app, store, rn) -> do
                    countBefore <- cellCount app

                    ack <-
                        insertSrc
                            app
                            store
                            rn
                            "-- cabal: build-depends: split\n1 + (1 :: Int)"

                    -- Was "compile-gate": a build-budget breach never reaches
                    -- the candidate, so nothing compiled it to refuse it.
                    textField "notCommitted" ack `shouldBe` Just "trial-blocked"
                    textField "verdict" ack `shouldBe` Just "no-verdict-infra"
                    countAfter <- cellCount app
                    countAfter `shouldBe` countBefore

    it
        "replays the live_test4 hole probe: rejected with a diagnostic, notebook still has only cell 0"
        $ do
            requireLiveIntegration
            withFixture "sabela-compilegate-livetest4" $ \(app, store, rn) -> do
                seedAck <- insertSrc app store rn "print 5"
                field "error" seedAck `shouldBe` Nothing
                countAfterSeed <- cellCount app
                countAfterSeed `shouldBe` 1

                probeAck <-
                    insertSrc
                        app
                        store
                        rn
                        "import Sabela.Notebook\nline (_ :: Point) (_ :: Point)"

                textField "notCommitted" probeAck `shouldBe` Just "compile-gate"
                textField "verdict" probeAck `shouldBe` Just "diagnostic"
                textField "diagnostic" probeAck `shouldSatisfy` maybe False (/= "")

                countAfterProbe <- cellCount app
                countAfterProbe `shouldBe` 1

                field "self_heal" probeAck `shouldBe` Nothing
                field "self_heal_suggestions" probeAck `shouldBe` Nothing
                textField "diagnostic" probeAck
                    `shouldSatisfy` maybe False (not . T.isInfixOf "GHC.Data.UnionFind")
                textField "source" probeAck
                    `shouldBe` Just "import Sabela.Notebook\nline (_ :: Point) (_ :: Point)"

                textField "diagnostic" probeAck
                    `shouldSatisfy` maybe False (T.isInfixOf "Point")
                (field "holeProbe" probeAck >>= field "provenance")
                    `shouldBe` Just (String "via: hole-probe")
                case field "holeProbe" probeAck >>= field "facts" of
                    Just (Array facts) ->
                        T.concat [f | String f <- toList facts]
                            `shouldSatisfy` T.isInfixOf
                                "no producer of `Point` found in scope"
                    _ -> expectationFailure "expected the rejection to carry holeProbe facts"
                case field "holeProbe" probeAck >>= field "holes" of
                    Just (Array holes) -> case toList holes of
                        [hole] -> do
                            field "goalType" hole `shouldBe` Just (String "Point")
                            field "producers" hole `shouldBe` Just (Array mempty)
                        _ -> expectationFailure "expected exactly one probed hole"
                    _ -> expectationFailure "expected the rejection to carry a probed hole"

    it "a gated hole whose goal type IS in scope has its fits harvested by the gate" $ do
        requireLiveIntegration
        withFixture "sabela-compilegate-holefits" $ \(app, store, rn) -> do
            countBefore <- cellCount app
            genBefore <- generationOf app

            let holed = "import Data.List (sort)\nsabelaHole = max (_ :: Int) (_ :: Int)"
            ack <- insertSrc app store rn holed

            textField "notCommitted" ack `shouldBe` Just "compile-gate"
            textField "verdict" ack `shouldBe` Just "diagnostic"
            case field "holeFits" ack of
                Just (Array fits) -> length fits `shouldSatisfy` (> 0)
                _ -> expectationFailure "expected the rejection to carry holeFits"
            case field "holeProbe" ack >>= field "facts" of
                Just (Array facts) ->
                    T.concat [f | String f <- toList facts]
                        `shouldSatisfy` T.isInfixOf "`Int` is produced by:"
                _ -> expectationFailure "expected the rejection to carry holeProbe facts"
            (field "holeProbe" ack >>= field "provenance")
                `shouldBe` Just (String "via: hole-probe")

            countAfter <- cellCount app
            genAfter <- generationOf app
            countAfter `shouldBe` countBefore
            genAfter `shouldBe` genBefore
