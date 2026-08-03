{-# LANGUAGE OverloadedStrings #-}

{- | Fix H, the write half: which rejections put a hole where a head stood,
which must be left alone, and what the answer costs. Properties are stated over
generated identifiers and over the source shapes real cells take.
-}
module Test.HoleRewriteSpec (spec) where

import Data.Aeson (encode, object)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Sabela.AI.Capabilities.Edit.CompileGate (holeAnswerPairs)
import Sabela.AI.Capabilities.Edit.HoleNudge (holeNudgePairs)
import Sabela.AI.Capabilities.Edit.HoleRewrite (
    Trigger (..),
    headSpans,
    holeName,
    holeRewriteSource,
    rewriteTarget,
    runHoleRewrite,
 )
import qualified Test.HoleAdapterSpec as Adapter
import Test.HoleRewriteGen (
    HeadSource (..),
    applySrc,
    countingProbe,
    genDistinct,
    genHeadSource,
    genIdent,
    holeBlob,
    namedHoleBlob,
    scopeDiag,
    untaggedScopeDiag,
 )
import qualified Test.TryRepairSpec as TryRepair

spec :: Spec
spec = do
    describe "hole-rewrite at the compile gate (fix H)" $ do
        rewriteSpec
        guardSpec
        probeSpec
        fallbackSpec
    Adapter.spec
    TryRepair.spec

-- Placing the hole ------------------------------------------------------

rewriteSpec :: Spec
rewriteSpec = describe "placing the hole" $ do
    prop "writes one hole, over the head, and changes nothing else" $
        forAll genHeadSource $ \hs ->
            case holeRewriteSource (scopeDiag 88464 (hsHead hs)) (hsSrc hs) of
                Nothing -> counterexample "no rewrite was offered" False
                Just (nm, tr, out) ->
                    conjoin
                        [ nm === hsHead hs
                        , tr === TriggerCode 88464
                        , countOf holeName out === 1
                        , countOf (hsHead hs) out
                            === countOf (hsHead hs) (hsSrc hs) - 1
                        , changedLines (hsSrc hs) out === [fst (hsSpan hs)]
                        , endsInNewline out === endsInNewline (hsSrc hs)
                        ]

    prop "finds the head's own column, not another occurrence of its text" $
        forAll genHeadSource $ \hs ->
            take 1 (headSpans (hsHead hs) (hsSrc hs)) === [hsSpan hs]

    prop "counts a head once per application it heads" $
        forAll genHeadSource $ \hs ->
            length (headSpans (hsHead hs) (hsSrc hs)) === hsCalls hs

    prop "reads the structural code when GHC printed one" $
        forAll (genDistinct 2) $ \[binder, headName] ->
            forAll (elements [88464, 39999]) $ \code ->
                fmap
                    snd3
                    (rewriteTarget (scopeDiag code headName) (src binder headName))
                    === Just (TriggerCode code)

    prop "falls back to the text form when no code is printed" $
        forAll (genDistinct 2) $ \[binder, headName] ->
            fmap
                snd3
                ( rewriteTarget
                    (untaggedScopeDiag headName)
                    (src binder headName)
                )
                === Just TriggerText
  where
    src binder headName = applySrc binder headName ["one", "two"]
    snd3 (_, b, _) = b

-- When it must not fire -------------------------------------------------

guardSpec :: Spec
guardSpec = describe "when the rewrite must not fire" $ do
    prop "a source that already asks the compiler an annotated hole is left alone" $
        forAll genHeadSource $ \hs ->
            forAll genIdent $ \own ->
                forAll (elements ["(_ :: Int)", "(1 + (_ :: Int))"]) $ \hole ->
                    holeRewriteSource
                        (scopeDiag 88464 (hsHead hs))
                        (own <> " = " <> hole <> "\n" <> hsSrc hs)
                        === Nothing

    prop "a bare hole of the candidate's own does not block the rewrite" $
        forAll genHeadSource $ \hs ->
            forAll (genIdent `suchThat` (/= hsHead hs)) $ \own ->
                holeRewriteSource
                    (scopeDiag 88464 (hsHead hs))
                    (own <> " = _\n" <> hsSrc hs)
                    =/= Nothing

    prop "a wildcard standing in a pattern does not block" $
        forAll genHeadSource $ \hs ->
            forAll (genDistinct 2) $ \[own, arg] ->
                forAll (patternForms own arg) $ \line ->
                    holeRewriteSource
                        (scopeDiag 88464 (hsHead hs))
                        (line <> "\n" <> hsSrc hs)
                        =/= Nothing

    prop "a source already carrying the harness's own marker is left alone" $
        forAll genHeadSource $ \hs ->
            forAll genIdent $ \own ->
                holeRewriteSource
                    (scopeDiag 88464 (hsHead hs))
                    (own <> " = " <> holeName <> "\n" <> hsSrc hs)
                    === Nothing

    prop "a name in argument position is not a head" $
        forAll (genDistinct 3) $ \[binder, outer, inner] ->
            holeRewriteSource
                (scopeDiag 88464 inner)
                (applySrc binder outer [inner, "two"])
                === Nothing

    prop "an unapplied name is not a head" $
        forAll (genDistinct 2) $ \[binder, headName] ->
            holeRewriteSource
                (scopeDiag 88464 headName)
                (binder <> " = " <> headName <> "\n")
                === Nothing

    prop "a name the cell itself defines is a knock-on, not a call" $
        forAll (genDistinct 2) $ \[binder, arg] ->
            holeRewriteSource
                (scopeDiag 88464 binder)
                (applySrc binder binder [arg])
                === Nothing

    prop "a type-level name cannot be stood in for by a term hole" $
        forAll (genDistinct 2) $ \[binder, headName] ->
            holeRewriteSource
                ( "error: [GHC-88464]\n    Not in scope: type constructor \
                  \or class \8216"
                    <> headName
                    <> "\8217"
                )
                (applySrc binder headName ["one"])
                === Nothing

    prop "a diagnostic that is not about scope does not trigger" $
        forAll (genDistinct 2) $ \[binder, headName] ->
            holeRewriteSource
                "error: [GHC-83865]\n    Couldn't match expected type Int"
                (applySrc binder headName ["one"])
                === Nothing

    prop "a name that only appears inside a string is not rewritten" $
        forAll (genDistinct 2) $ \[binder, headName] ->
            holeRewriteSource
                (scopeDiag 88464 headName)
                (binder <> " = putStrLn \"" <> headName <> " one\"\n")
                === Nothing

    prop "a head standing after a string that escapes a quote is still found" $
        forAll (genDistinct 2) $ \[binder, headName] ->
            holeRewriteSource
                (scopeDiag 88464 headName)
                ( binder
                    <> " = putStrLn \"a \\\" b\" >> print ("
                    <> headName
                    <> " 1)\n"
                )
                =/= Nothing

patternForms :: Text -> Text -> Gen Text
patternForms own arg =
    elements
        [ own <> " _ " <> arg <> " = 1"
        , own <> " = \\(x, _) -> x"
        , own <> " = case " <> arg <> " of _ -> 1"
        , own <> " = let (_, y) = " <> arg <> " in y"
        ]

-- What the answer costs -------------------------------------------------

probeSpec :: Spec
probeSpec = describe "probing the rewrite" $ do
    prop "answers a rejection for one extra compile, never two" $
        forAll genHeadSource $ \hs ->
            forAll genIdent $ \fitName -> monadicIO $ do
                (probe, count) <- run (countingProbe (fitBlob fitName))
                _ <-
                    run
                        ( holeAnswerPairs
                            probe
                            (scopeDiag 88464 (hsHead hs))
                            (hsSrc hs)
                        )
                n <- run count
                assert (n == 1)

    prop "probes exactly once, and never twice" $
        forAll genHeadSource $ \hs -> monadicIO $ do
            (probe, count) <- run (countingProbe (fitBlob "beta"))
            _ <-
                run
                    ( runHoleRewrite
                        probe
                        (scopeDiag 88464 (hsHead hs))
                        (hsSrc hs)
                    )
            n <- run count
            assert (n == 1)

    prop "does not reach the compiler when the trigger does not fire" $
        forAll (genDistinct 3) $ \[binder, outer, inner] -> monadicIO $ do
            (probe, count) <- run (countingProbe (fitBlob "beta"))
            out <-
                run
                    ( runHoleRewrite
                        probe
                        (scopeDiag 88464 inner)
                        (applySrc binder outer [inner])
                    )
            n <- run count
            assert (n == 0 && isNothing out)
  where
    fitBlob nm = holeBlob "[Int] -> Int" [(nm, "[Int] -> Int")]

-- The branch where no hole can be placed ---------------------------------

fallbackSpec :: Spec
fallbackSpec = describe "when no hole can be placed" $ do
    prop "hands the standing nudge through byte for byte" $
        forAll (genDistinct 3) $ \[binder, outer, inner] -> monadicIO $ do
            let diagnostic = scopeDiag 88464 inner
                src = applySrc binder outer [inner]
            (probeA, _) <- run (countingProbe nudgeBlob)
            (probeB, _) <- run (countingProbe nudgeBlob)
            answered <- run (holeAnswerPairs probeA diagnostic src)
            standing <- run (holeNudgePairs probeB diagnostic src)
            assert (encode (object answered) == encode (object standing))

    prop "says nothing it cannot attribute, and pays nothing to say it" $
        forAll genHeadSource $ \hs ->
            forAll (genIdent `suchThat` (/= hsHead hs)) $ \own -> monadicIO $ do
                (probe, count) <- run (countingProbe nudgeBlob)
                out <-
                    run
                        ( holeAnswerPairs
                            probe
                            (scopeDiag 88464 (hsHead hs))
                            (own <> " = (_ :: Int)\n" <> hsSrc hs)
                        )
                n <- run count
                assert (null out && n == 0)
  where
    nudgeBlob = namedHoleBlob "_" "[Int] -> Int" [("beta", "[Int] -> Int")]

-- Helpers ---------------------------------------------------------------

countOf :: Text -> Text -> Int
countOf = T.count

changedLines :: Text -> Text -> [Int]
changedLines a b =
    [ i
    | (i, x, y) <- zip3 [1 ..] (T.splitOn "\n" a) (T.splitOn "\n" b)
    , x /= y
    ]

endsInNewline :: Text -> Bool
endsInNewline = T.isSuffixOf "\n"
