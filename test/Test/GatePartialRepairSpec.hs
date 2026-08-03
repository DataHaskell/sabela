{-# LANGUAGE OverloadedStrings #-}

{- | A partial-repair rejection tells the model where the fixes went. The field
the note names must be the field that holds them, in every payload the gate
can emit — a note describing the wrong field is the same lie as a wrong field
(LEDGER C1-4e).
-}
module Test.GatePartialRepairSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.GateFrontier (Frontier (..))
import Sabela.AI.Capabilities.Edit.GateRepair (frontierRejectionJson)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
 )
import Test.HarnessGen (genCellSource, genDiagnostic, genIdent, genPackageName)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

-- | Every field name the note quotes in backticks.
quotedFields :: Text -> [Text]
quotedFields note =
    [t | (i, t) <- zip [(0 :: Int) ..] (T.splitOn "`" note), odd i]

{- | The field the note points at as holding the fixes: everything quoted in
the sentence that says where they are.
-}
fixesLocations :: Text -> [Text]
fixesLocations note = case T.breakOn marker note of
    (_, rest)
        | T.null rest -> []
        | otherwise -> take 1 (quotedFields (T.drop (T.length marker) rest))
  where
    marker = "The fixes are in "

failed :: Text -> DisposableResult
failed diag =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = diag
        , disposableFailure =
            Just (MaterializeFailure StageCandidateTypecheck Nothing diag)
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

{- | A frontier that has measured at least one fix: the harness compiled
something other than what was handed to it, or (the degenerate case) exactly
what it was handed.
-}
genRepairedFrontier :: Gen Frontier
genRepairedFrontier = do
    submitted <- genCellSource
    diag <- genDiagnostic
    fixes <- listOf1 genFix
    rewritten <- arbitrary
    extra <- genPackageName
    let compiled
            | rewritten = "-- cabal: build-depends: " <> extra <> "\n" <> submitted
            | otherwise = submitted
    pure (Frontier submitted compiled fixes (failed diag))
  where
    genFix = do
        from <- genIdent
        to <- genIdent
        pkg <- genPackageName
        elements [from <> " -> " <> to, "declared build-depends: " <> pkg]

reject :: Frontier -> Value
reject = frontierRejectionJson Nothing Nothing [] []

showFrontier :: Frontier -> String
showFrontier f =
    show (frontierSubmitted f, frontierSrc f, frontierFixes f)

noteOf :: Value -> Maybe Text
noteOf v = field "partialRepair" v >>= textField "note"

spec :: Spec
spec = describe "a partial repair says where its fixes are" $ do
    it
        "prop_theNoteNamesTheFieldThatHoldsTheFixes: whatever field the note \
        \points at carries the source the fixes were applied to"
        $ property
        $ forAllShow genRepairedFrontier showFrontier
        $ \frontier ->
            let v = reject frontier
                note = fromMaybe "" (noteOf v)
                named = fixesLocations note
             in counterexample (show (note, named)) $
                    conjoin
                        [ counterexample "the note must name a field" (named =/= [])
                        , counterexample "and that field must hold the repaired source" $
                            conjoin
                                [ textField k v === Just (frontierSrc frontier)
                                | k <- named
                                ]
                        ]

    it
        "prop_theNoteOnlyNamesFieldsThePayloadHas: every field the note quotes \
        \is present in the same object"
        $ property
        $ forAllShow genRepairedFrontier showFrontier
        $ \frontier ->
            let v = reject frontier
                note = fromMaybe "" (noteOf v)
             in conjoin
                    [ counterexample (T.unpack k) (field k v =/= Nothing)
                    | k <- quotedFields note
                    ]

    it
        "prop_sourceStaysTheSubmissionUnderRepair: the field called `source` \
        \is the caller's own text, however far repair got"
        $ property
        $ forAllShow genRepairedFrontier showFrontier
        $ \frontier ->
            textField "source" (reject frontier)
                === Just (frontierSubmitted frontier)

    it "says nothing about fixes when none were measured" $ do
        let frontier = Frontier "x = 1" "x = 1" [] (failed "boom")
        noteOf (reject frontier) `shouldBe` Nothing
