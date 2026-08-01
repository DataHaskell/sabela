{-# LANGUAGE OverloadedStrings #-}

module Test.GateFrontierSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.GateFrontier (
    Frontier (..),
    Step (..),
    frontierPairs,
    startFrontier,
    stepFrontier,
 )
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
 )

failing :: Text -> DisposableResult
failing diag =
    DisposableResult
        "disposable_scratch"
        DisposableCompileError
        Nothing
        ""
        diag
        Nothing
        []
        []
        []

green :: DisposableResult
green = (failing ""){disposableVerdict = DisposableOk}

noModule :: Text
noModule =
    "<no location info>: error: [GHC-35235]\n\
    \    Could not find module \8216DataFrame\8217.\n\
    \    It is not a module in the current program, or in any known package.\n\
    \\n\
    \<interactive>:382:23: error: [GHC-76037]\n\
    \    Not in scope: \8216DataFrame.readParquet\8217"

unmasked :: Text
unmasked =
    "<interactive>:383:4: error: [GHC-83865]\n\
    \    Couldn't match expected type \8216Bool\8217 with actual type \8216IO Bool\8217\n\
    \\n\
    \<interactive>:389:17: error: [GHC-88464]\n\
    \    Variable not in scope: takeRows"

cellSrc :: Text
cellSrc = "import qualified DataFrame\nmain = DataFrame.readParquet \"iris.parquet\""

withDep :: Text
withDep = "-- cabal: build-depends: dataframe\n" <> cellSrc

spec :: Spec
spec = describe "the gate's repair frontier" $ do
    let start = startFrontier cellSrc cellSrc (failing noModule)
        depFix = ["declared build-depends: dataframe"]

    describe "a candidate that clears what it targets advances the frontier" $ do
        it "adopts a dependency fix even though the body's errors then surface" $
            case stepFrontier start (withDep, depFix) (failing unmasked) of
                Advance f -> do
                    frontierSrc f `shouldBe` withDep
                    frontierFixes f `shouldBe` depFix
                _ -> expectationFailure "expected the dependency fix to advance"

        it "commits only on green, carrying every fix applied to get there" $
            case stepFrontier start (withDep, depFix) green of
                Commit f -> frontierFixes f `shouldBe` depFix
                _ -> expectationFailure "expected a green candidate to commit"

        it "skips a candidate that clears nothing" $
            case stepFrontier start (withDep, depFix) (failing noModule) of
                Skip -> pure ()
                _ -> expectationFailure "expected an unchanged diagnostic to be skipped"

        it "skips a probe that never compiled — infra silence is not progress" $
            case stepFrontier
                start
                (withDep, depFix)
                (failing "build timed out"){disposableVerdict = DisposableTimedOut} of
                Skip -> pure ()
                _ -> expectationFailure "expected a timed-out probe to be skipped"

        it "accumulates fixes across rounds, so the disclosure names them all" $ do
            let renamed = withDep <> "\nx = 1"
            case stepFrontier start (withDep, depFix) (failing unmasked) of
                Advance f ->
                    case stepFrontier f (renamed, ["takeRows -> take"]) green of
                        Commit g ->
                            frontierFixes g
                                `shouldBe` depFix <> ["takeRows -> take"]
                        _ -> expectationFailure "expected the second fix to commit"
                _ -> expectationFailure "expected the dependency fix to advance"

    describe "what the rejection tells the model about a partial repair" $ do
        it "says nothing when no fix was measured" $
            frontierPairs start `shouldBe` []

        it "names the applied fixes alongside the source they are already in" $ do
            let f = start{frontierFixes = depFix}
            case object (frontierPairs f) of
                Object o -> KM.lookup "partialRepair" o `shouldBe` Just partial
                _ -> expectationFailure "expected an object"

        it "is a note the model can act on, not a bare list" $ do
            let f = start{frontierFixes = depFix}
            case object (frontierPairs f) of
                Object o -> case KM.lookup "partialRepair" o of
                    Just (Object p) -> case KM.lookup "note" p of
                        Just (String n) ->
                            n `shouldSatisfy` T.isInfixOf "source"
                        _ -> expectationFailure "expected a note"
                    _ -> expectationFailure "expected an object"
                _ -> expectationFailure "expected an object"
  where
    partial =
        object
            [ "applied" .= (["declared build-depends: dataframe"] :: [Text])
            , "note"
                .= ( "Nothing was committed. The fixes are in `source`, which is \
                     \the text the compiler read, and the diagnostic is what \
                     \remains after them. Each fix measurably reduced the \
                     \errors, which is not the same as being right." ::
                        Text
                   )
            ]
