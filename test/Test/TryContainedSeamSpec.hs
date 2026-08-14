{-# LANGUAGE OverloadedStrings #-}

{- | The seam try's contained backend is reached through. Every property drives
the real @execTry@ with the materialisation replaced by a recorder, so what a
trial submits and which route it takes are observed rather than assumed.
-}
module Test.TryContainedSeamSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import Data.IORef (newIORef, readIORef)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import Sabela.AI.Capabilities.Try.Candidate (probePrefix)
import Sabela.AI.Capabilities.TryPlan (TrialTier (..))
import Sabela.AI.ToolDoc (tierTypecheckOnly, trialTierNames)
import Sabela.Server (newApp)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    emptyResult,
    evalCandidate,
 )
import Sabela.Session.Materialize.Pipeline (runCandidate)
import qualified Sabela.SessionTypes as ST
import Test.TryFrontierGen
import Test.TrySeamFixtures

spec :: Spec
spec = describe "the seam try's contained backend is reached through" $ do
    app <- runIO (newApp "." Set.empty Nothing Nothing [])

    describe "C1-5a: a trial performs nothing it submits" $ do
        prop "a contained candidate is submitted with no expression to evaluate" $
            forAll genCandidate $ \c -> monadicIO $ do
                (specs, _) <- run (tryWith app (const green) (candSource c))
                monitor (counterexample (show (map candidateExpression specs)))
                assert
                    ( tierOf (candSource c)
                        /= Just TierContained
                        || all ((== Nothing) . candidateExpression) specs
                    )

        prop "no setup a trial submits carries a statement GHCi would perform" $
            forAll genCandidate $ \c -> monadicIO $ do
                (specs, _) <- run (tryWith app (const green) (candSource c))
                let offending =
                        [ (candidateSetup s, executingPieces (candidateSetup s))
                        | s <- specs
                        , not (null (executingPieces (candidateSetup s)))
                        ]
                monitor (counterexample (show offending))
                assert (null offending)

        prop "an effect spelling is answered, and the answer says nothing ran" $
            forAll genEffectCandidate $ \c -> monadicIO $ do
                (specs, v) <- run (tryWith app (const green) (candSource c))
                monitor (counterexample (show v))
                assert (length specs == 1)
                assert (field "executed" v == Just (Bool False))
                assert (textField "tier" v == Just "typecheck_only")

    describe "C1-5a: the tier a payload names is the tier that ran" $ do
        prop "never names a tier the answer's own execution fact contradicts" $
            forAll genCandidate $ \c ->
                forAll genContainedAnswer $ \answer -> monadicIO $ do
                    (_, v) <- run (tryWith app (const answer) (candSource c))
                    monitor (counterexample (show v))
                    assert (tierAgreesWithExecution v)

        prop "an IO expression the session declined is never a pure evaluation" $
            forAll genIOExpressionCandidate $ \c ->
                forAll (elements ioMarkers) $ \marker ->
                    forAll (elements ioTypes) $ \typ -> monadicIO $ do
                        let answer = const (ioDeclined typ marker)
                        (_, v) <- run (tryWith app answer (candSource c))
                        monitor (counterexample (show v))
                        assert (field "executed" v == Just (Bool False))
                        assert (textField "tier" v == Just tierTypecheckOnly)

        prop "the live route names no tier its own answer contradicts" $
            forAll (elements libGrid) $ \lib ->
                forAll genLiveResult $ \probe -> monadicIO $ do
                    live <- run (appWithLive probe)
                    (_, v) <- run (tryWith live (const green) (libPure lib))
                    monitor (counterexample (show v))
                    assert (tierAgreesWithExecution v)

        prop "no tier is named that the surface does not publish" $
            forAll genCandidate $ \c ->
                forAll genContainedAnswer $ \answer -> monadicIO $ do
                    (_, v) <- run (tryWith app (const answer) (candSource c))
                    monitor (counterexample (show v))
                    assert (maybe True (`elem` trialTierNames) (textField "tier" v))

    describe "C1-5a: the contained session evaluates no IO either" $ do
        prop "an IO-typed expression is answered by its type, never run" $
            forAll (elements ioMarkers) $ \marker ->
                forAll (elements ioTypes) $
                    \typ -> monadicIO $ do
                        ran <- run (newIORef [])
                        backend <- run (recordingBackend ran (ioRejection typ marker))
                        result <-
                            run
                                ( evalCandidate
                                    backend
                                    "writeFile \"escaped\" \"x\""
                                    (emptyResult [])
                                )
                        blocks <- run (readIORef ran)
                        monitor (counterexample (show (blocks, result)))
                        assert (null blocks)
                        assert (disposableVerdict result == DisposableOk)
                        assert (disposableType result == Just typ)

    describe "C1-18: localisation stops probing at its deadline" $ do
        it "spends no probe once the deadline has passed" $ do
            seen <- newIORef []
            now <- getMonotonicTime
            result <- probePrefix (recorder seen (const green)) (now - 1) "a = 1"
            readIORef seen `shouldReturn` []
            disposableVerdict result `shouldBe` DisposableUnavailable

        it "probes while the deadline is ahead" $ do
            seen <- newIORef []
            now <- getMonotonicTime
            result <- probePrefix (recorder seen (const green)) (now + 3600) "a = 1"
            specs <- readIORef seen
            length specs `shouldBe` 1
            disposableVerdict result `shouldBe` DisposableOk

    describe "C1-14d: an IO rejection escalates to the contained backend" $ do
        prop "the fast-path probe's IO rejection reaches the contained run" $
            forAll (elements ioMarkers) $ \marker ->
                forAll (elements ioTypes) $
                    \typ -> monadicIO $ do
                        live <- run (appWithLive (ioRejection typ marker))
                        (specs, v) <-
                            run (tryWith live (const green) "readFile \"notes.txt\"")
                        monitor (counterexample (show v))
                        assert (length specs == 1)
                        assert (textField "route" v == Just "disposable_scratch")
                        assert
                            ( not
                                ( "no qualified containment backend"
                                    `T.isInfixOf` renderAll v
                                )
                            )

        it "the escalated run is answered by the contained backend, not refused" $ do
            live <- appWithLive (ioRejection "IO ()" firstIOMarker)
            (specs, v) <- tryWith live (const green) "readFile \"notes.txt\""
            map candidateExpression specs `shouldBe` [Just "readFile \"notes.txt\""]
            textField "verdict" v `shouldBe` Just "ok"

    describe "a red setup's import knock-ons are scrubbed at the seam" $
        it "surfaces only the import failure the setup earned" $ do
            ran <- newIORef []
            backend <- recordingBackend ran (ioRejection "IO ()" "")
            let src =
                    "import Data.List.Split (splitOn)\n\
                    \rows = map (splitOn \",\") [\"a,b\"]" ::
                        T.Text
                blob =
                    "<no location info>: error:\n\
                    \    Could not find module \8216Data.List.Split\8217\n\
                    \\n\
                    \<interactive>:792:19: error: [GHC-88464]\n\
                    \    Variable not in scope: \
                    \splitOn :: t0 -> String -> [a0]"
                submitted =
                    CandidateSpec
                        { candidateMetadataSource = src
                        , candidateSetup = src
                        , candidateExpression = Nothing
                        , candidateReplacesCellId = Nothing
                        , candidateDeliberate = False
                        }
            result <-
                runCandidate
                    backend{ST.sbRunBlock = \_ -> pure ("", blob)}
                    submitted
                    (emptyResult [])
            disposableVerdict result `shouldBe` DisposableCompileError
            fmap failureStage (disposableFailure result)
                `shouldBe` Just StageCandidateSetup
            let message = maybe "" failureMessage (disposableFailure result)
            message `shouldSatisfy` T.isInfixOf "Could not find module"
            message `shouldSatisfy` (not . T.isInfixOf "Variable not in scope")

    describe "C1-18: a rejected trial carries its localisation" $
        prop "the rejection names the component that stopped compiling" $
            forAll genPoisoned $ \p -> monadicIO $ do
                let bad = poisonNames p !! poisonIndex p
                    defines src =
                        any
                            ((bad <> " =") `T.isPrefixOf`)
                            (T.lines (candidateMetadataSource src))
                    answer s
                        | defines s =
                            red ("error: [GHC-88464] Variable not in scope: " <> bad)
                        | otherwise = green
                (_, v) <- run (tryWith app answer (poisonSource p))
                let loc = field "localisation" v
                monitor (counterexample (show v))
                assert ((loc >>= textField "firstFailingComponent") == Just bad)
                assert
                    ( (loc >>= field "provenComponents")
                        == Just (toJSON (take (poisonIndex p) (poisonNames p)))
                    )
