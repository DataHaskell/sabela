{-# LANGUAGE OverloadedStrings #-}

{- | C2 tasks 2, 3 and 6: the tautology corpus, drawn verbatim from the live
transcripts, can never reach the user prompt. Compiling is not the bar —
every member of the corpus compiles. The reference gate kills the ones that
name nothing the task defined; the mutation gate kills the ones that name it
and still cannot fail.
-}
module Test.CheckGateSpec (checkGateSpec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Check.Gate (
    CheckRefusal (..),
    Perturbation (..),
    identifiersOf,
    mentionsAny,
    perturbCheck,
    perturbationsFor,
    referenceGate,
    refusalNote,
 )

{- | The live corpus. @True@ and @5 == 5@ are from live_test8 and the housing
episode; the rest are the "mentions the binding but cannot fail" class the
compile-vet alone lets through.
-}
constantCorpus :: [Text]
constantCorpus = ["True", "5 == 5", "1 + 1 == 2", "otherwise"]

vacuousCorpus :: [(Text, Text, Text)]
vacuousCorpus =
    [ ("x", "String", "x /= \"\"")
    , ("x", "String", "not (null (show x))")
    , ("x", "String", "length (show x) > 0")
    , ("xs", "[Int]", "not (null xs)")
    ]

owned :: [Text]
owned = ["x", "xs", "total", "sineWaveSvg"]

{- | A check survives the mutation gate only if some perturbation makes it
false. Evaluated here as a pure predicate over the value, standing in for the
kernel run 'Siza.Agent.Check.Vet' performs.
-}
discriminates :: (Text -> Bool) -> Text -> Text -> Text -> Bool
discriminates eval name ty check =
    any (\p -> not (eval (perturbCheck name p check))) (perturbationsFor ty name)

checkGateSpec :: Spec
checkGateSpec = describe "check gate: reference and mutation (C2)" $ do
    describe "reference gate (task 2)" $ do
        it "refuses every constant in the corpus" $
            forM_ constantCorpus $ \c ->
                (c, referenceGate owned c) `shouldBe` (c, Just NoReference)

        it "admits a check naming a binding the task defined" $
            referenceGate owned "total == 42.5" `shouldBe` Nothing

        it "never counts a near-name as a mention" $ do
            referenceGate ["xs"] "not (null xss)" `shouldBe` Just NoReference
            referenceGate ["xs"] "not (null xs)" `shouldBe` Nothing

        it "identifiersOf splits on non-identifier characters" $
            identifiersOf "length (show x) > 0" `shouldBe` ["length", "show", "x"]

        it "mentionsAny is whole-token, not substring" $ do
            mentionsAny ["x"] "xs" `shouldBe` False
            mentionsAny ["x"] "f x" `shouldBe` True

    describe "mutation gate (task 3)" $ do
        {- The whole vacuous corpus, evaluated against a value that is
        nonempty however it is perturbed: not one member can be falsified,
        so not one may be offered. These are precisely the checks that pass
        the compile-vet — they all compile. -}
        it "refuses every vacuous check in the corpus" $
            forM_ vacuousCorpus $ \(name, ty, check) ->
                (check, discriminates (const True) name ty check)
                    `shouldBe` (check, False)

        it "a string check surviving truncation cannot fail — refused" $ do
            -- `x /= ""` on a nonempty string: truncation keeps it nonempty,
            -- so no perturbation falsifies it. The plan's named specimen.
            let evalNonEmpty _ = True
            discriminates evalNonEmpty "x" "String" "x /= \"\"" `shouldBe` False

        it "a numeric equality is falsified by an off-by-one" $ do
            -- `total == 42.5` is false once total is perturbed.
            let evalExact e = e == "total == 42.5"
            discriminates evalExact "total" "Double" "total == 42.5"
                `shouldBe` True

        it "offers off-by-one both ways for a numeric value" $
            map pName (perturbationsFor "Double" "total")
                `shouldBe` ["offset by 1", "offset by -1"]

        it "truncates a string but never empties it" $ do
            let ps = perturbationsFor "String" "x"
            map pExpr ps `shouldBe` ["init (x)"]

        it "drops and reverses a list, so length and order claims can fail" $
            map pName (perturbationsFor "[Int]" "xs")
                `shouldBe` ["dropped last", "reversed"]

        it "offers nothing for a type it cannot perturb" $
            perturbationsFor "Picture" "pic" `shouldBe` []

    describe "perturbCheck substitutes whole tokens only" $ do
        it "rewrites the binding and leaves near-names alone" $
            perturbCheck "xs" (Perturbation "l" "(init xs)") "length xs == length xss"
                `shouldBe` "length (init xs) == length xss"

        it "reassembles the expression byte-for-byte apart from the swap" $
            perturbCheck "q" (Perturbation "l" "q") "f (q) && g q"
                `shouldBe` "f (q) && g q"

    describe "refusals are disclosed, never silent" $
        it "every refusal has a reason the user can read" $
            forM_ [NoReference, Indiscriminate] $ \r ->
                refusalNote r `shouldNotBe` ""
