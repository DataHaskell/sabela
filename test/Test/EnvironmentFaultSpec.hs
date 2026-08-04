{-# LANGUAGE OverloadedStrings #-}

{- | A tooling fault is not a verdict on the candidate's dependencies. Read as
one it states a limit the session does not have, which stopped the live
hodatime episode installing at all (docs/discover/live/live_hodatime.md).
-}
module Test.EnvironmentFaultSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Try.Payload (disposablePayload)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    environmentFault,
 )
import Test.DisposableFixtures (baseResult, gateOf, reasonOf)

{- | The index fault the live hodatime episode ran onto: cabal could not read
its own package index, and the trial stopped before the candidate.
-}
indexFault :: Text
indexFault =
    "user error (GHCi exited during startup:\nError: cabal: Could not read \
    \index. Did you call 'checkForUpdates'?)"

{- | The claim that misled the live episode: that naming a package is what
broke the stage. A remedy that disclaims it is not making it.
-}
blamesCandidateMetadata :: Maybe Text -> Bool
blamesCandidateMetadata =
    maybe False ("a package your candidate names can break it" `T.isInfixOf`)

-- | The remedy a genuine resolve failure earns: the candidate's own metadata.
namesCandidateMetadata :: Maybe Text -> Bool
namesCandidateMetadata =
    maybe False ("dependencies your candidate declares" `T.isInfixOf`)

resolveFault :: Text
resolveFault = "cabal: Could not resolve dependencies: rejecting nosuchpkg"

atProject :: Text -> DisposableResult
atProject msg =
    baseResult
        { disposableVerdict = DisposableUnavailable
        , disposableFailure = Just (MaterializeFailure StageProject Nothing msg)
        , disposableDependencies = ["hodatime"]
        }

spec :: Spec
spec =
    describe "an environment fault is not the candidate's dependency metadata" $ do
        it "reads an unreadable package index as the environment's own fault" $
            environmentFault indexFault `shouldBe` True
        it "reads a resolve failure as something the candidate contributed to" $
            environmentFault resolveFault `shouldBe` False
        it "does not blame the candidate's dependencies for an index fault" $ do
            let r = atProject indexFault
            reasonOf (disposablePayload r)
                `shouldSatisfy` not . blamesCandidateMetadata
            reasonOf (gateOf "x = 1" r)
                `shouldSatisfy` not . blamesCandidateMetadata
        it "denies the reading that the session cannot add packages" $
            reasonOf (disposablePayload (atProject indexFault))
                `shouldSatisfy` maybe
                    False
                    ( "does not mean this session cannot install packages"
                        `T.isInfixOf`
                    )
        it "says an index fault is retryable, so it is not a standing limit" $ do
            let saysRetry = maybe False ("Retry" `T.isInfixOf`)
            reasonOf (disposablePayload (atProject indexFault))
                `shouldSatisfy` saysRetry
        it "still blames the metadata when the resolve itself failed" $
            reasonOf (disposablePayload (atProject resolveFault))
                `shouldSatisfy` namesCandidateMetadata
