{-# LANGUAGE OverloadedStrings #-}

{- | How an episode is allowed to end: a silent reply is not a completion, a
kernel that answered nothing is its own stop class, and only a verified episode
becomes an exemplar. Pins NEW-1, NEW-2, C1-20.
-}
module Test.VerdictStopSpec (verdictStopSpec) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Loop (AgentRun (..))
import Test.TruthGen (genGhcDiagnostic, genPreambleSource, genSubstantiveSource)
import Test.VerdictEpisode (
    anyVerdict,
    blankReply,
    deadKernelEpisode,
    episode,
    passing,
    withExemplarStore,
 )

verdictStopSpec :: Spec
verdictStopSpec = describe "how an episode is allowed to end" $ do
    terminalTurnSpec
    kernelFailureSpec
    exemplarSpec

terminalTurnSpec :: Spec
terminalTurnSpec = describe "NEW-1/C1-20: an empty reply is not a completion" $ do
    it "never files a silent terminal turn under done" $
        property $
            forAll ((,,) <$> genSubstantiveSource <*> blankReply <*> passing) $
                \(src, blank, verdict) -> ioProperty $ do
                    run <- episode [(src, True)] blank verdict
                    pure (arStopped run `shouldSatisfy` (`notElem` ["done", "done_unverified"]))

    it "never reports healthy progress when nothing substantive was committed" $
        property $
            forAll genPreambleSource $ \src -> ioProperty $ do
                run <- episode [(src, True)] "" (CheckNotApplicable, Nothing)
                pure
                    ( T.unpack (arFinal run)
                        `shouldSatisfy` (not . isInfixOf "written and healthy")
                    )

{- | C1-20(a): a kernel that answered nothing is reported as a kernel that
answered nothing, not as an exhausted turn budget.
-}
kernelFailureSpec :: Spec
kernelFailureSpec = describe "C1-20: an unreachable kernel is its own stop class" $
    it "stops on the transport failure rather than on the turn budget" $
        property $
            forAll ((,) <$> genSubstantiveSource <*> genGhcDiagnostic) $
                \(src, why) -> ioProperty $ do
                    run <- deadKernelEpisode src why
                    pure (arStopped run `shouldBe` "kernel_error")

exemplarSpec :: Spec
exemplarSpec = describe "NEW-2: only a verified episode becomes an exemplar" $
    it "stores a record exactly when the check passed" $
        property $
            forAll ((,) <$> genSubstantiveSource <*> anyVerdict) $
                \(src, verdict) -> ioProperty $ do
                    stored <- withExemplarStore (episode [(src, True)] "done" verdict)
                    pure (stored `shouldBe` (fst verdict == CheckPassed))
