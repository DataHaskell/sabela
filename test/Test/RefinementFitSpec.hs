{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for REFINEMENT hole-fit repair (evalExpr deep-dive,
finding 6 — the general form of the argument-insertion move).

The session already queries hole fits with @-frefinement-level-hole-fits=2@,
and for the original @takeWhile1@ goal GHC's refinement fits propose exactly
@takeWhileP (_ :: Maybe String)@ — the compiler names BOTH the right function
and the missing argument's type, with no error-prose parsing. Today the fit
parser drops refinement fits (it reads plain identifiers only), so the tier
substitutes bare @takeWhileP@ — the exact arity failure the gate measured.

Scope split this pins: refinement fits can only see IN-SCOPE names, so this
path heals @takeWhile1@ → @takeWhileP Nothing@ (megaparsec imported) but can
never propose the unimported @chainl1@ — that stays with the import/hoogle
tier.

Proposed API:

  refinementFits :: Text -> [(Text, Text)]     -- blob -> [(fn, subHoleType)]
    (Sabela.AI.HoleFits)
  holeQueryFor :: Text -> Text                 -- goal -> parseable "_ :: …"
    (sanitizes package-qualified noise; today the raw goal makes the
     query itself unparseable, silently zeroing the tier on these errors)
-}
module Test.RefinementFitSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoleFits (refinementFits)
import Sabela.AI.HoleRepair (
    holeQueryFor,
    substituteNameAt,
    substituteNameAtAll,
 )
import Sabela.AI.SelfHeal (plausibleRename)

-- | The real GHC 9.12 refinement-fit shape for the takeWhile1 goal.
refinementBlob :: T.Text
refinementBlob =
    T.unlines
        [ "  Valid refinement hole fits include"
        , "    takeWhileP (_ :: Maybe String)"
        , "    const (_ :: (Char -> Bool) -> ParsecT Void String Identity String)"
        , "    pure (_ :: (Char -> Bool) -> ParsecT Void String Identity String)"
        ]

-- | The real gemma goal, package-qualified noise included.
gemmaGoal :: T.Text
gemmaGoal =
    "(Char -> Bool) -> ParsecT Void String ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity String"

spec :: Spec
spec = describe "refinement hole-fit repair (intention)" $ do
    describe "refinementFits — parse (fn, sub-hole type) pairs" $ do
        it "reads the head name and the missing argument's type" $
            refinementFits refinementBlob
                `shouldSatisfy` elem ("takeWhileP", "Maybe String")
        it "reads every offered refinement" $
            map fst (refinementFits refinementBlob)
                `shouldBe` ["takeWhileP", "const", "pure"]
        it "is empty for a blob with only plain fits" $
            refinementFits
                ( T.unlines
                    [ "  Valid hole fits include"
                    , "    some :: Alternative f => f a -> f [a]"
                    ]
                )
                `shouldBe` []
        it "reads any library's refinement shape (GHC's own foldl1 example)" $
            refinementFits
                ( T.unlines
                    [ "  Valid refinement hole fits include"
                    , "    foldl1 (_ :: Integer -> Integer -> Integer)"
                    ]
                )
                `shouldBe` [("foldl1", "Integer -> Integer -> Integer")]
        it "reads a package-qualified sub-hole type, parens balanced" $
            -- The sub-hole type must be captured to ITS closing paren, not the
            -- first one — a qualified name inside Maybe (…) breaks a naive
            -- break-on-paren parse. Preserved verbatim; the query layer
            -- sanitizes.
            refinementFits
                ( T.unlines
                    [ "  Valid refinement hole fits include"
                    , "    fromMaybeText (_ :: Maybe (text-2.1.2:Data.Text.Internal.Text))"
                    ]
                )
                `shouldBe` [("fromMaybeText", "Maybe (text-2.1.2:Data.Text.Internal.Text)")]

    describe "holeQueryFor — the query itself must be parseable" $ do
        it "strips package-qualified noise from the goal" $ do
            let q = holeQueryFor gemmaGoal
            q `shouldSatisfy` (not . T.isInfixOf "ghc-internal")
            q `shouldSatisfy` (not . T.isInfixOf ":GHC.")
        it "keeps the arrow structure the fits are searched at" $
            holeQueryFor "Int -> Int" `shouldBe` "_ :: Int -> Int"

    describe "the refinement candidate is gated and spliced like any rename" $ do
        it "the head name passes the distance gate (takeWhile1 ~ takeWhileP)" $
            plausibleRename "takeWhile1" "takeWhileP" `shouldBe` True
        it "a distant refinement head declines (customers ~ const)" $
            plausibleRename "customers" "const" `shouldBe` False
        it "substituteNameAt splices a multi-token replacement at the site" $ do
            let src = "    numStr <- takeWhile1 (\\c -> isDigit c)"
            substituteNameAt (1, 15) "takeWhile1" "(takeWhileP Nothing)" src
                `shouldBe` Just "    numStr <- (takeWhileP Nothing) (\\c -> isDigit c)"

    describe "substituteNameAtAll — one candidate heals EVERY site of the name" $ do
        -- A name failing at two sites yields one identical diagnostic in the
        -- message-set health, so a single-site fix shows no improvement and
        -- reverts; the candidate must rewrite all sites at once.
        let src =
                T.unlines
                    [ "float = f (takeWhile1 p)"
                    , "number = g (takeWhile1 q)"
                    ]
        it "substitutes at both sites" $
            substituteNameAtAll [(1, 12), (2, 13)] "takeWhile1" "(takeWhileP Nothing)" src
                `shouldBe` Just
                    ( T.unlines
                        [ "float = f ((takeWhileP Nothing) p)"
                        , "number = g ((takeWhileP Nothing) q)"
                        ]
                    )
        it "handles two sites on ONE line (later column first)" $
            substituteNameAtAll
                [(1, 5), (1, 19)]
                "foo"
                "barbaz"
                "x = foo y + h1 z (foo w)"
                `shouldBe` Just "x = barbaz y + h1 z (barbaz w)"
        it "is Nothing when any site does not hold the name" $
            substituteNameAtAll [(1, 12), (2, 1)] "takeWhile1" "t" src
                `shouldBe` Nothing
        it "is Nothing for no sites" $
            substituteNameAtAll [] "takeWhile1" "t" src `shouldBe` Nothing
