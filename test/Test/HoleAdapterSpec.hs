{-# LANGUAGE OverloadedStrings #-}

{- | The adapter hole: where the compiler rejected an argument's type, the
harness asks it what converts the type the caller holds into the type the slot
wants. Properties are stated over generated calls, identifiers and types.
-}
module Test.HoleAdapterSpec (spec) where

import Data.Aeson (encode, object)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Sabela.AI.ArgRepair (ArgBlame (..), argumentBlame)
import Sabela.AI.Capabilities.Edit.CompileGate (holeAnswerPairs)
import Sabela.AI.Capabilities.Edit.HoleNudge (holeNudgePairs)
import Sabela.AI.Capabilities.Edit.HoleRewrite (
    HolePlan (..),
    Placement (..),
    Trigger (..),
    holeName,
    holeRewritePlan,
    holeWrap,
    runHoleRewrite,
 )
import qualified Test.HoleAdapterCascadeSpec as Cascade
import Test.HoleAdapterGen (
    ArgSource (..),
    argMismatchCode,
    genArgSource,
    genFive,
    genInfixCall,
    genThree,
    genTwo,
    mismatchDiag,
    untaggedMismatchDiag,
    wrappedMismatchDiag,
 )
import qualified Test.HoleAdapterPublishSpec as Publish
import Test.HoleAdapterRig (
    carriesWrapAt,
    changedLines,
    convBlob,
    diagWith,
    endsInNewline,
    holeArguments,
    noBlameDiag,
    plan,
    planWith,
    unbracket,
 )
import qualified Test.HoleAdapterTruthSpec as Truth
import Test.HoleRewriteGen (countingProbe, namedHoleBlob, scopeDiag)

spec :: Spec
spec = describe "the adapter hole (fix A)" $ do
    placementSpec
    operatorSpec
    Cascade.spec
    declineSpec
    costSpec
    Truth.spec
    Publish.spec

-- An operator's argument is as locatable as a prefix head's ---------------

operatorSpec :: Spec
operatorSpec = describe "an argument of a call written infix" $ do
    prop "is located and wrapped, whichever side of the operator it is on" $
        forAll genInfixCall $ \as ->
            forAll genTwo $ \(expected, actual) ->
                case plan as expected actual of
                    Nothing -> counterexample (T.unpack (asSrc as)) False
                    Just p ->
                        conjoin
                            [ counterexample (T.unpack (hpRewritten p)) (carriesWrapAt as (hpRewritten p))
                            , T.count holeName (hpRewritten p) === 1
                            , hpPlacement p
                                === AtArgument (ArgBlame (asFn as) (asIndex as) (asArg as))
                            ]

    prop "is located when its call stands beside a backticked operator" $
        forAll genFive $ \(binder, fn, lhs, inner, arg) ->
            let src =
                    binder
                        <> " = "
                        <> lhs
                        <> " `"
                        <> fn
                        <> "` "
                        <> inner
                        <> " "
                        <> arg
                        <> "\n"
             in fmap
                    hpPlacement
                    (holeRewritePlan (mismatchDiag "Ee" "Aa" inner 1 arg) src)
                    === Just (AtArgument (ArgBlame inner 1 arg))

    prop "is not confused with the same text standing on the other side" $
        forAll genInfixCall $ \as ->
            forAll genTwo $ \(expected, actual) ->
                case plan as expected actual of
                    Nothing -> counterexample "no rewrite was offered" False
                    Just p -> changedLines (asSrc as) (hpRewritten p) === [1]

-- Wrapping the blamed argument -------------------------------------------

placementSpec :: Spec
placementSpec = describe "wrapping the argument the compiler blamed" $ do
    prop "wraps that argument, on its line, and changes nothing else" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                case plan as expected actual of
                    Nothing -> counterexample "no rewrite was offered" False
                    Just p ->
                        let out = hpRewritten p
                         in conjoin
                                [ T.count holeName out === 1
                                , T.count (holeWrap (asArg as)) out === 1
                                , changedLines (asSrc as) out === [fst (asSpan as)]
                                , endsInNewline out === endsInNewline (asSrc as)
                                ]

    prop "asks about that argument whole, never about the parts of it" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                case plan as expected actual of
                    Nothing -> counterexample "no rewrite was offered" False
                    Just p ->
                        counterexample (T.unpack (hpRewritten p)) $
                            map unbracket (holeArguments (hpRewritten p))
                                === [unbracket (asArg as)]

    prop "wraps the argument at its own column, not another of the call's" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                case plan as expected actual of
                    Nothing -> counterexample "no rewrite was offered" False
                    Just p ->
                        let out = hpRewritten p
                         in counterexample (T.unpack out) (carriesWrapAt as out)

    prop "names the placement as an argument of the call" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                fmap hpPlacement (plan as expected actual)
                    === Just
                        (AtArgument (ArgBlame (asFn as) (asIndex as) (asArg as)))

    prop "reports the position the compiler named, past the tenth too" $
        forAll genTwo $ \(fn, arg) ->
            forAll (choose (1, 20)) $ \idx ->
                fmap
                    (\b -> (abFunction b, abIndex b, abExpr b))
                    (argumentBlame (mismatchDiag "Ee" "Aa" fn idx arg))
                    === Just (fn, idx, arg)

    prop "reads the blame the compiler wrapped onto a continuation line" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                planWith wrappedMismatchDiag as expected actual =/= Nothing

    prop "answers a compiler that printed no structural code" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                fmap
                    hpTrigger
                    (planWith untaggedMismatchDiag as expected actual)
                    === Just TriggerMismatchText

    prop "names the structural code the compiler did print" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                fmap hpTrigger (plan as expected actual)
                    === Just (TriggerCode argMismatchCode)

-- When it must not fire ---------------------------------------------------

declineSpec :: Spec
declineSpec = describe "when the adapter must not fire" $ do
    prop "a rejection that blames no argument is not rewritten" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) ->
                holeRewritePlan (noBlameDiag expected actual) (asSrc as)
                    === Nothing

    prop "an argument the candidate never wrote is not located" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, absent) ->
                holeRewritePlan
                    (mismatchDiag expected actual (asFn as) (asIndex as) absent)
                    (asSrc as)
                    === Nothing

    prop "an argument standing twice after its call is left ambiguous" $
        forAll genThree $ \(binder, fn, arg) ->
            holeRewritePlan
                (mismatchDiag "Ee" "Aa" fn 1 arg)
                (binder <> " = " <> fn <> " " <> arg <> " " <> arg <> "\n")
                === Nothing

    prop "an argument that is itself a hole is not wrapped in another" $
        forAll genTwo $ \(binder, fn) ->
            forAll (elements ["_", "(_ :: Int)"]) $ \arg ->
                holeRewritePlan
                    (mismatchDiag "Ee" "Aa" fn 1 arg)
                    (binder <> " = " <> fn <> " " <> arg <> "\n")
                    === Nothing

    prop "a candidate asking an annotated hole of its own is left alone" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, own) ->
                holeRewritePlan
                    (diagWith as expected actual)
                    (own <> " = (_ :: Int)\n" <> asSrc as)
                    === Nothing

    prop "a candidate already carrying the harness's marker is left alone" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, own) ->
                holeRewritePlan
                    (diagWith as expected actual)
                    (own <> " = " <> holeName <> " 1\n" <> asSrc as)
                    === Nothing

    prop "an argument whose text only stands in a comment is not located" $
        forAll genThree $ \(binder, fn, arg) ->
            holeRewritePlan
                (mismatchDiag "Ee" "Aa" fn 1 arg)
                (binder <> " = " <> fn <> " 1  -- " <> arg <> "\n")
                === Nothing

-- What the answer costs ---------------------------------------------------

costSpec :: Spec
costSpec = describe "what the adapter costs" $ do
    prop "answers an argument rejection for one extra compile, never two" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                (probe, count) <-
                    run (countingProbe (convBlob actual expected conv))
                _ <-
                    run
                        ( holeAnswerPairs
                            probe
                            (diagWith as expected actual)
                            (asSrc as)
                        )
                n <- run count
                assert (n == 1)

    prop "does not reach the compiler when no argument can be located" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, absent) -> monadicIO $ do
                (probe, count) <- run (countingProbe "")
                out <-
                    run
                        ( runHoleRewrite
                            probe
                            (mismatchDiag expected actual (asFn as) (asIndex as) absent)
                            (asSrc as)
                        )
                n <- run count
                assert (n == 0 && isNothing out)

    prop "hands a rejection it cannot place through byte for byte" $
        forAll genThree $ \(binder, fn, invented) -> monadicIO $ do
            let src = binder <> " = " <> fn <> " " <> invented <> "\n"
                diagnostic =
                    noBlameDiag "Ee" "Aa" <> "\n\n" <> scopeDiag 88464 invented
                blob = namedHoleBlob "_" "[Int] -> Int" [("zzz", "[Int] -> Int")]
            (probeA, _) <- run (countingProbe blob)
            (probeB, _) <- run (countingProbe blob)
            answered <- run (holeAnswerPairs probeA diagnostic src)
            standing <- run (holeNudgePairs probeB diagnostic src)
            assert
                ( not (null standing)
                    && encode (object answered) == encode (object standing)
                )
