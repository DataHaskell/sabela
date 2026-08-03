{-# LANGUAGE OverloadedStrings #-}

{- | What an adapted rejection may say. Every fact published must come from the
block GHC wrote about the harness's own hole, and a type the compiler was free
to pick must say so rather than stand as the answer.
-}
module Test.HoleAdapterTruthSpec (spec) where

import Data.Aeson (Value (..), object)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Sabela.AI.Capabilities.Edit.HoleRewrite (
    holeName,
    holeRewritePairs,
    holeWrap,
    runHoleRewrite,
 )
import Sabela.AI.Capabilities.Edit.HoleRewrite.Section (typeVariables)
import Sabela.AI.Hints (expectedTypeOf)
import Test.HoleAdapterGen (
    ArgSource (..),
    genArgSource,
    genPrintedType,
    genQualified,
    genThree,
    genTwo,
    genTypeName,
 )
import Test.HoleAdapterRig (
    arrow,
    diagWith,
    fitWrites,
    payloadOf,
    provOf,
    provTyped,
    textAt,
 )
import Test.HoleRewriteGen (countingProbe, field, genIdent, namedHoleBlob)

spec :: Spec
spec = do
    payloadSpec
    defaultingSpec
    expectedTypeSpec
    qualifierSpec
    bottomSpec

-- Every layout GHC prints a mismatch in ---------------------------------------

expectedTypeSpec :: Spec
expectedTypeSpec = describe "the type a mismatch says was wanted" $ do
    prop "is read from the Expected/Actual block" $
        forAll genPrintedType $ \(wanted, got) ->
            expectedTypeOf
                ( "    \8226 Couldn't match type\n      Expected: "
                    <> wanted
                    <> "\n        Actual: "
                    <> got
                )
                === Just wanted

    prop "is read from the inline sentence GHC quotes" $
        forAll genPrintedType $ \(wanted, got) ->
            expectedTypeOf
                ( "    \8226 Couldn't match expected type \8216"
                    <> wanted
                    <> "\8217 with actual type \8216"
                    <> got
                    <> "\8217"
                )
                === Just wanted

    prop "is read from the colon layout, wrapped across two lines" $
        forAll genPrintedType $ \(wanted, got) ->
            expectedTypeOf
                ( "    \8226 Couldn't match expected type: "
                    <> wanted
                    <> "\n                  with actual type: "
                    <> got
                )
                === Just wanted

    prop "is read from the bare two-type form" $
        forAll genPrintedType $ \(wanted, got) ->
            expectedTypeOf
                ( "    \8226 Couldn't match type \8216"
                    <> wanted
                    <> "\8217 with \8216"
                    <> got
                    <> "\8217"
                )
                === Just wanted

    prop "is nothing at all where the compiler matched no types" $
        forAll genTwo $ \(a, b) ->
            expectedTypeOf ("Variable not in scope: " <> a <> " :: " <> b)
                === Nothing

-- A package qualifier is not a type variable ----------------------------------

qualifierSpec :: Spec
qualifierSpec = describe "a name the compiler printed through a package" $ do
    prop "is not reported as a type variable" $
        forAll genQualified $ \(qualified, _) ->
            forAll genIdent $ \v ->
                typeVariables (qualified <> " -> " <> v) === [v]

    prop "is not published in the hole type either" $
        forAll genArgSource $ \as ->
            forAll genQualified $ \(qualified, _) -> monadicIO $ do
                prov <- run (provTyped as ("Ay -> " <> qualified) "zzz")
                assert
                    ( maybe False (not . T.isInfixOf ":") (prov >>= textAt "holeType")
                        && isNothing (prov >>= field "typeVariables")
                    )

-- A fit that inhabits every goal ----------------------------------------------

bottomSpec :: Spec
bottomSpec = describe "a fit that takes nothing and yields a bare variable" $ do
    prop "is not offered, because it converts nothing" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(actual, expected) ->
                forAll genIdent $ \v -> monadicIO $ do
                    let ty = arrow actual expected
                    (probe, _) <-
                        run
                            ( countingProbe
                                (namedHoleBlob holeName ty [("zzz", "forall " <> v <> ". " <> v)])
                            )
                    out <- run (runHoleRewrite probe (diagWith as "Ee" "Aa") (asSrc as))
                    assert (null (fitWrites (provOf out)))

    prop "where a real converter beside it is still offered" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(actual, expected, conv) -> monadicIO $ do
                let ty = arrow actual expected
                (probe, _) <-
                    run
                        ( countingProbe
                            (namedHoleBlob holeName ty [("zzz", "forall aa. aa"), (conv, ty)])
                        )
                out <- run (runHoleRewrite probe (diagWith as "Ee" "Aa") (asSrc as))
                assert (fitWrites (provOf out) == [conv])

payloadSpec :: Spec
payloadSpec = describe "what an adapted rejection may say" $ do
    prop "publishes a converter from the actual type to the expected one" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                prov <- run (provTyped as (arrow actual expected) conv)
                assert
                    ( (prov >>= textAt "holeType") == Just (arrow actual expected)
                        && fitWrites prov == [conv]
                    )

    prop "names the harness, the call, the position and the expression" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                prov <- run (provTyped as (arrow actual expected) conv)
                assert
                    ( (prov >>= textAt "wrapped") == Just (asArg as)
                        && (prov >>= textAt "with") == Just (holeWrap (asArg as))
                        && (prov >>= textAt "of") == Just (asFn as)
                        && (prov >>= field "argument")
                            == Just (Number (fromIntegral (asIndex as)))
                        && maybe
                            False
                            (T.isInfixOf "the harness")
                            (prov >>= textAt "by")
                    )

    prop "hands back no source, so nothing committed can carry the hole" $
        forAll genArgSource $ \as ->
            forAll genThree $ \(expected, actual, conv) -> monadicIO $ do
                pairs <- run (payloadOf as (arrow actual expected) conv)
                let rendered = T.pack (show (object pairs))
                assert (T.count holeName rendered == 1)

    prop "publishes nothing when the compiler wrote no block about its hole" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(expected, actual) -> monadicIO $ do
                (probe, _) <- run (countingProbe "error: something else entirely")
                out <-
                    run
                        ( runHoleRewrite
                            probe
                            (diagWith as expected actual)
                            (asSrc as)
                        )
                assert (null (maybe [] holeRewritePairs out))

defaultingSpec :: Spec
defaultingSpec = describe "a hole type carrying no information" $ do
    prop "discloses a result the compiler was free to pick" $
        forAll genArgSource $ \as ->
            forAll genTypeName $ \actual ->
                forAll (elements ["()", "Any"]) $ \nothingType -> monadicIO $ do
                    prov <- run (provTyped as (arrow actual nothingType) "zzz")
                    assert
                        ( maybe
                            False
                            (T.isInfixOf nothingType)
                            (prov >>= textAt "holeTypeCaveat")
                            && isNothing (prov >>= field "typeVariables")
                        )

    prop "says nothing of the kind about a ground informative result" $
        forAll genArgSource $ \as ->
            forAll genTwo $ \(actual, expected) -> monadicIO $ do
                prov <- run (provTyped as (arrow actual expected) "zzz")
                assert (isNothing (prov >>= field "holeTypeCaveat"))
