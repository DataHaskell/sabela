{-# LANGUAGE OverloadedStrings #-}

{- | Which block a rejection with several of them is answered at: the one the
others follow from, and never a separate rejection that merely shares a name
with it. Properties are over generated calls and identifiers.
-}
module Test.HoleAdapterCascadeSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.ArgRepair (ArgBlame (..))
import Sabela.AI.Capabilities.Edit.HoleRewrite (
    HolePlan (..),
    Placement (..),
    holeRewritePlan,
 )
import Test.HoleAdapterCascadeGen (
    Cascade (..),
    CrossLine (..),
    Separate (..),
    genCascade,
    genCrossLine,
    genSeparate,
    genSeparateLate,
 )

spec :: Spec
spec = describe "a rejection GHC printed several blocks for" $ do
    prop "answers the block the others follow from, in either printed order" $
        forAll genCascade $ \ca ->
            let (fn, idx, expr) = caRoot ca
             in fmap hpPlacement (holeRewritePlan (caDiagnostic ca) (caSrc ca))
                    === Just (AtArgument (ArgBlame fn idx expr))

    prop "says which blames the answered one accounts for" $
        forAll genCascade $ \ca ->
            fmap hpSubsumed (holeRewritePlan (caDiagnostic ca) (caSrc ca))
                === Just [caInner ca]

    prop "declines rather than descend to one when the root cannot be placed" $
        forAll genCascade $ \ca ->
            holeRewritePlan (caRootUnplaceable ca) (caSrc ca) === Nothing

    prop "does not read a rejection reported elsewhere as one that follows" $
        forAll genSeparate $ \sp ->
            case holeRewritePlan (spDiagnostic sp) (spSrc sp) of
                Nothing -> counterexample "declined outright" False
                Just p ->
                    conjoin
                        [ hpPlacement p === AtArgument (blameOf (spFirst sp))
                        , hpSubsumed p === []
                        ]

    prop "names a rejection it did not account for as one it did not" $
        forAll genSeparate $ \sp ->
            fmap hpUnanswered (holeRewritePlan (spDiagnostic sp) (spSrc sp))
                === Just [thirdOf (spSecond sp)]

    prop "declines rather than descend into one reported across two lines" $
        forAll genCrossLine $ \cl ->
            holeRewritePlan (clDiagnostic cl) (clSrc cl) === Nothing

    prop "claims nothing follows from a blame the spans put outside it" $
        forAll genSeparateLate $ \sp ->
            case holeRewritePlan (spDiagnostic sp) (spSrc sp) of
                Nothing -> counterexample "declined outright" False
                Just p ->
                    conjoin
                        [ hpPlacement p === AtArgument (blameOf (spFirst sp))
                        , hpSubsumed p === []
                        , hpUnanswered p === [thirdOf (spSecond sp)]
                        ]

blameOf :: (Text, Int, Text) -> ArgBlame
blameOf (fn, idx, expr) = ArgBlame fn idx expr

thirdOf :: (Text, Int, Text) -> Text
thirdOf (_, _, expr) = expr
