{-# LANGUAGE OverloadedStrings #-}

{- | The gate must report what it actually did. Properties over every
'MaterializeStage', so a failure in code the caller never wrote can never be
returned as "this candidate does not compile" (LEDGER C1-4d, C2-2b, C2-2c,
C1-4e, C2-10c).
-}
module Test.GateStageWireSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.CompileGate (
    GateSource (..),
    rejectionJson,
    submittedOnly,
 )
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    SkippedCell (..),
    blamedCell,
    materializeStages,
    stageReachedCandidate,
 )
import Test.HarnessGen (genCellSource, genDiagnostic)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

baseResult :: DisposableResult
baseResult =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

genStage :: Gen MaterializeStage
genStage = elements materializeStages

genSkipped :: Gen SkippedCell
genSkipped = SkippedCell <$> choose (0, 40) <*> elements ["prose", "python", "empty"]

-- | The four verdicts a recorded failure can carry; a green run records none.
genFailingVerdict :: Gen DisposableVerdict
genFailingVerdict =
    elements
        [ DisposableCompileError
        , DisposableRuntimeError
        , DisposableTimedOut
        , DisposableUnavailable
        ]

genResult :: Gen (DisposableResult, Text)
genResult = do
    stage <- genStage
    verdict <- genFailingVerdict
    diag <- genDiagnostic
    mcid <- oneof [pure Nothing, Just <$> choose (0, 40)]
    replayed <- listOf (choose (0, 40))
    skips <- listOf genSkipped
    pure
        ( baseResult
            { disposableVerdict = verdict
            , disposableFailure = Just (MaterializeFailure stage mcid diag)
            , disposableReplayedCells = replayed
            , disposableSkippedCells = skips
            }
        , diag
        )

reject :: GateSource -> DisposableResult -> Value
reject gsrc = rejectionJson Nothing Nothing gsrc []

stageOf :: DisposableResult -> Maybe MaterializeStage
stageOf = fmap failureStage . disposableFailure

spec :: Spec
spec = describe "the gate reports the stage it actually reached" $ do
    it
        "prop_noCompileClaimBeforeTheCandidate: a failure at a stage before the \
        \candidate is never reported as the candidate failing to compile"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, _), src) ->
            let v = reject (submittedOnly src) result
                reached = maybe False stageReachedCandidate (stageOf result)
                msg = fromMaybe "" (textField "error" v)
             in not reached ==> not ("does not compile" `T.isInfixOf` msg)

    it
        "prop_verdictMatchesStage: a verdict GHC produced keys on the stage, \
        \and no verdict claims a diagnostic about a candidate the compiler \
        \never read or that a candidate it did read was not run"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, _), src) ->
            let v = reject (submittedOnly src) result
                reached = maybe False stageReachedCandidate (stageOf result)
                tag = textField "verdict" v
                compilerSpoke =
                    disposableVerdict result
                        `elem` [DisposableCompileError, DisposableRuntimeError]
             in conjoin
                    [ counterexample "a compiler verdict must key on the stage" $
                        not compilerSpoke
                            .||. tag
                                === Just (if reached then "diagnostic" else "could-not-run")
                    , counterexample "a diagnostic about code never compiled" $
                        tag /= Just "diagnostic" || reached
                    , counterexample "'could not run' about code that was run" $
                        tag /= Just "could-not-run" || not reached
                    ]

    it
        "prop_diagnosticIsAttributed: a payload that quotes an <interactive> \
        \location always says whose code the location belongs to"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, diag), src) ->
            "<interactive>:"
                `T.isInfixOf` diag
                ==> ( case textField "attributedTo" (reject (submittedOnly src) result) of
                        Just t -> not (T.null t)
                        Nothing -> False
                    )

    it
        "prop_carriesEveryFactTheResultHolds: replayed cells, skipped cells and \
        \the blamed cell all survive into the rejection"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, _), src) ->
            let v = reject (submittedOnly src) result
                ints (Just (Array xs)) =
                    [round d | Number d <- foldr (:) [] xs] :: [Int]
                ints _ = []
                blamed = case field "blamedCell" v of
                    Just (Number d) -> Just (round d :: Int)
                    _ -> Nothing
             in conjoin
                    [ counterexample
                        "replayedCells"
                        (ints (field "replayedCells" v) === disposableReplayedCells result)
                    , counterexample
                        "skippedCells count"
                        ( length (skippedIds (field "skippedCells" v))
                            === length (disposableSkippedCells result)
                        )
                    , counterexample "blamedCell" (blamed === blamedCell result)
                    ]

    it
        "prop_theModelSeesItsOwnSource: `source` is byte-identical to what was \
        \submitted, and a harness rewrite is labelled, never substituted"
        $ property
        $ forAll ((,,) <$> genResult <*> genCellSource <*> genCellSource)
        $ \((result, _), submitted, compiledSrc) ->
            let gsrc = GateSource submitted compiledSrc
                v = reject gsrc result
             in conjoin
                    [ counterexample
                        "source is the submission"
                        (textField "source" v === Just submitted)
                    , counterexample
                        "a rewrite is disclosed under its own key"
                        ( textField "compiledSource" v
                            === (if compiledSrc == submitted then Nothing else Just compiledSrc)
                        )
                    ]

    it
        "prop_blamedCellFallsBackToTheLastCellThatRan: a stage that names no \
        \cell is still attributed to the last replayed cell"
        $ property
        $ forAll ((,) <$> genStage <*> listOf (choose (0, 40)))
        $ \(stage, replayed) ->
            let result =
                    baseResult
                        { disposableFailure = Just (MaterializeFailure stage Nothing "boom")
                        , disposableReplayedCells = replayed
                        }
                expected
                    | stageReachedCandidate stage = Nothing
                    | null replayed = Nothing
                    | otherwise = Just (last replayed)
             in blamedCell result === expected

    it "names the offending cell rather than the cell being replaced" $ do
        let result =
                baseResult
                    { disposableFailure = Just (MaterializeFailure StagePrelude Nothing "clash")
                    , disposableReplayedCells = [3, 7, 10]
                    }
            v = rejectionJson Nothing (Just 12) (submittedOnly "x = 1") [] result
        field "cellId" v `shouldBe` Just (Number 12)
        field "blamedCell" v `shouldBe` Just (Number 10)
  where
    skippedIds (Just (Array xs)) = foldr (:) [] xs
    skippedIds _ = []
