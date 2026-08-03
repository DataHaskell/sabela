{-# LANGUAGE OverloadedStrings #-}

{- | The episode loop may only report what it computed this turn: a write that
was accepted and is still present, and a check that ran against it.
Pins C2-1b, C2-1c, C2-1d, C2-1e, C3-5.
-}
module Test.VerdictEvidenceSpec (verdictEvidenceSpec) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Verdict (VerdictClass (..))
import Siza.Agent.Check (CheckResult (..), noVerdictNote)
import Siza.Agent.Loop (AgentRun (..))
import Siza.Agent.Owned (hasArtifact, noWriteReason)
import Siza.Agent.Tools (offeredNames)
import Test.DiscoverFixtures (textField)
import Test.TruthGen (genPreambleSource, genSubstantiveSource)
import Test.VerdictEpisode (
    acceptedWrite,
    anySource,
    couldNotRunText,
    episode,
    noVerdict,
    nonPassing,
    ownedFor,
    passing,
    precedingToolMsg,
    verdictClasses,
 )

verdictEvidenceSpec :: Spec
verdictEvidenceSpec = describe "a verdict is evidence, not a habit" $ do
    okVerdictSpec
    noVerdictSpec
    signalEvidenceSpec
    channelAttributionSpec

okVerdictSpec :: Spec
okVerdictSpec = describe "C2-1b: only a check that passed reads as ok" $
    it "emits no ok verdict unless the check passed" $
        property $
            forAll ((,) <$> genSubstantiveSource <*> nonPassing) $
                \(src, verdict) -> ioProperty $ do
                    run <- episode [(src, True)] "done" verdict
                    pure (verdictClasses run `shouldSatisfy` notElem VerdictOk)

{- | C2-1b: the detail a no-verdict carries is the reason there is none, so it
may never be assembled into a message that reads as a check that ran and failed.
-}
noVerdictSpec :: Spec
noVerdictSpec = describe "C2-1b: a missing verdict is reported as a missing verdict" $ do
    it "diagnoses a failing check only for a check that ran and failed" $
        property $
            forAll ((,) <$> anySource <*> noVerdict) $
                \(src, verdict) -> ioProperty $ do
                    run <- episode [(src, True)] "done" verdict
                    pure
                        ( verdictClasses run
                            `shouldSatisfy` notElem VerdictDiagnostic
                        )

    it "states the reason there was no verdict, on the could-not-run channel" $
        property $
            forAll ((,) <$> genPreambleSource <*> unreached) $
                \(src, result) -> ioProperty $ do
                    let note = reasonFor src
                    run <- episode [(src, True)] "done" (result, note)
                    pure (couldNotRunText run `shouldSatisfy` any (carries note))

    it "never reports a committed preamble cell as none committed" $
        property $
            forAll genPreambleSource $ \src ->
                fromMaybe "" (reasonFor src)
                    `shouldSatisfy` (not . T.isInfixOf "no cell has been committed")
  where
    carries mNote body = maybe False (`T.isInfixOf` body) mNote
    unreached = elements [CheckUncheckable, CheckNotApplicable]

-- | The reason the gate itself would give for a turn that wrote this source.
reasonFor :: Text -> Maybe Text
reasonFor src = noVerdictNote <$> noWriteReason (ownedFor src)

signalEvidenceSpec :: Spec
signalEvidenceSpec = describe "C2-1d/C2-1e: the signal sits beside its evidence" $ do
    it "never fires for a write the notebook refused" $
        property $
            forAll ((,) <$> genSubstantiveSource <*> passing) $
                \(src, verdict) -> ioProperty $ do
                    run <- episode [(src, False)] "done" verdict
                    pure (verdictClasses run `shouldSatisfy` notElem VerdictOk)

    it "never fires for a committed cell that commits nothing executable" $
        property $
            forAll ((,) <$> genPreambleSource <*> passing) $
                \(src, verdict) -> ioProperty $ do
                    run <- episode [(src, True)] "done" verdict
                    pure (verdictClasses run `shouldSatisfy` notElem VerdictOk)

    it "a landed write and the artifact predicate agree on every source" $
        property $
            forAll ((,) <$> anySource <*> passing) $ \(src, verdict) ->
                ioProperty $ do
                    run <- episode [(src, True)] "done" verdict
                    let landed = hasArtifact (ownedFor src)
                    pure ((VerdictOk `elem` verdictClasses run) `shouldBe` landed)

    it "the tool message nearest before an ok verdict is an accepted write" $
        property $
            forAll ((,) <$> genSubstantiveSource <*> passing) $
                \(src, verdict) -> ioProperty $ do
                    run <- episode [(src, True)] "done" verdict
                    pure (precedingToolMsg run `shouldSatisfy` acceptedWrite)

channelAttributionSpec :: Spec
channelAttributionSpec = describe "C3-5: no message is stamped with a tool nobody called" $
    it "every offered tool name on the transcript matches a call the model made" $
        property $
            forAll ((,) <$> genSubstantiveSource <*> passing) $
                \(src, verdict) -> ioProperty $ do
                    run <- episode [(src, True)] "done" verdict
                    let named =
                            [ n
                            | m <- arTranscript run
                            , textField "role" m == "tool"
                            , let n = textField "tool_name" m
                            , n `elem` offeredNames
                            ]
                    pure (named `shouldBe` ["insert_cell", "list_bindings"])
