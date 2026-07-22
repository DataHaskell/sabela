{-# LANGUAGE OverloadedStrings #-}

{- | R6-T4 verdict totality (search-api.md section 5.3): every verifier-surface
outcome class renders a member of the CLOSED verdict vocabulary, silence is
undecodable, and a transport-swallowed answer decodes as the no-verdict infra
class — never an implicit pass.
-}
module Test.VerdictSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Scratchpad (scratchpadVerdict)
import Sabela.AI.Verdict (
    VerdictClass (..),
    parseVerdict,
    verdictClasses,
    verdictMarker,
    verdictTag,
    verdictVocabulary,
 )

spec :: Spec
spec = describe "Sabela.AI.Verdict (closed verdict vocabulary, section 5.3)" $ do
    describe "the vocabulary is closed and total" $ do
        it "every class renders a tag inside the vocabulary" $
            mapM_
                (\c -> verdictTag c `shouldSatisfy` (`elem` verdictVocabulary))
                verdictClasses
        it "tags are distinct (no two classes collapse)" $ do
            let tags = map verdictTag verdictClasses
            length tags `shouldBe` length verdictVocabulary
            mapM_ (\t -> length (filter (== t) tags) `shouldBe` 1) tags
        it "marker -> parse round-trips for every class" $
            mapM_
                (\c -> parseVerdict (verdictMarker c) `shouldBe` Just c)
                verdictClasses
        it "the marker survives inside surrounding message text" $
            mapM_
                ( \c ->
                    parseVerdict
                        ("some preamble " <> verdictMarker c <> " and a tail")
                        `shouldBe` Just c
                )
                verdictClasses

    describe "silence is undecodable" $ do
        it "prose with no verdict marker decodes to Nothing" $
            parseVerdict "Your cells run, but the check fails." `shouldBe` Nothing
        it "the empty answer decodes to Nothing" $
            parseVerdict "" `shouldBe` Nothing

    describe "a transport-swallowed answer is the no-verdict infra class" $ do
        it "the [infra] transport envelope decodes as VerdictInfra" $
            parseVerdict
                "[infra] no response within 300s. The server is likely STILL \
                \WORKING; a write may have landed."
                `shouldBe` Just VerdictInfra
        it "the [kernel] envelope decodes as VerdictInfra" $
            parseVerdict "[kernel] the kernel died mid-run."
                `shouldBe` Just VerdictInfra
        it "the [payload] envelope decodes as could-not-run (never a pass)" $
            parseVerdict "[payload] HTTP 400: malformed body."
                `shouldBe` Just VerdictCouldNotRun
        it "no transport envelope ever decodes as ok" $
            mapM_
                ( \e ->
                    parseVerdict e `shouldSatisfy` (/= Just VerdictOk)
                )
                [ "[infra] HTTP 404: this tool endpoint does not exist"
                , "[kernel] wedged"
                , "[payload] rejected"
                ]

    describe "the scratchpad outcome grid renders a member (schema, not convention)" $ do
        it "is total over (stdout x stderr)" $
            sequence_
                [ verdictTag (scratchpadVerdict out err)
                    `shouldSatisfy` (`elem` verdictVocabulary)
                | out <- ["", "  \n", "42\n"]
                , err <- ["", "error: Could not load module 'DataFrame'"]
                ]
        it "empty stdout + empty stderr is could-not-run, never a pass" $
            scratchpadVerdict "" "" `shouldBe` VerdictCouldNotRun
        it "whitespace-only output is still could-not-run" $
            scratchpadVerdict "  \n" "  " `shouldBe` VerdictCouldNotRun
        it "a diagnostic-bearing stderr is the diagnostic class" $
            scratchpadVerdict "" "error: parse error" `shouldBe` VerdictDiagnostic
        it "clean output is ok" $
            scratchpadVerdict "42\n" "" `shouldBe` VerdictOk

    describe "tags are lint-stable (single line, lower-kebab)" $
        it "every tag is nonempty, one word, no uppercase" $
            mapM_
                ( \t -> do
                    t `shouldSatisfy` (not . T.null)
                    t `shouldSatisfy` (not . T.any (== ' '))
                    t `shouldBe` T.toLower t
                )
                verdictVocabulary
