{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for the ARGUMENT-INSERTION repair move (evalExpr deep-dive
finding 2).

The live gate showed gemma finally adopt @takeWhileP@ on its last turn and die
on the arity wall: GHC says \"Probable cause: `takeWhileP' is applied to too
few arguments\" and names the missing FIRST argument's type (@Maybe String@).
No rename can fix this class; the move is to INSERT the missing argument,
filled by a hole fit of its type (@Nothing@). Note the fill-list must be
position-aware: 'vacuousFit' rightly bans @Nothing@ as a full-RHS replacement,
but it is the legitimate feed for an argument slot.

Proposed API (new pure module Sabela.AI.ArgRepair, src-contract):

  tooFewArgsTarget :: Text -> Maybe Text          -- error -> misapplied fn
  missingArgType   :: Text -> Text -> Maybe Text  -- error -> fn -> first-arg type
  argFillCandidates :: Text -> [Text]             -- hole-fit blob -> fills (vacuous KEPT)
  insertArgAt :: (Int, Int) -> Text -> Text -> Text -> Maybe Text
    -- span -> fn -> fill -> source -> source with `fn fill …` at that site
-}
module Test.ArgRepairSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.ArgRepair (
    argFillCandidates,
    insertArgAt,
    missingArgType,
    tooFewArgsTarget,
 )

-- | The real turn-25 gate error, verbatim shape (abridged to the two diagnostics).
arityErr :: Text
arityErr =
    T.unlines
        [ "cell 0, line 26: Couldn't match expected type: ParsecT"
        , "                                Void"
        , "                                String"
        , "                                ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity"
        , "                                String"
        , "            with actual type: (Token s0 -> Bool) -> m0 (Tokens s0)"
        , "Probable cause: `takeWhileP' is applied to too few arguments"
        , "In a stmt of a 'do' block:"
        , "  numStr <- takeWhileP (\\ c -> isDigit c || c == '.')"
        , "cell 0, line 26: Couldn't match expected type: Maybe String"
        , "            with actual type: Char -> Bool"
        , "The lambda expression `\\ c -> ...' has one visible argument,"
        , "  but its type `Maybe String' has none"
        , "In the first argument of `takeWhileP', namely"
        , "  `(\\ c -> isDigit c || c == '.')'"
        ]

spec :: Spec
spec = describe "argument-insertion repair (intention)" $ do
    describe "tooFewArgsTarget — GHC names the misapplied function" $ do
        it "reads the function from the probable-cause line" $
            tooFewArgsTarget arityErr `shouldBe` Just "takeWhileP"
        it "is Nothing when no too-few-arguments cause is named" $
            tooFewArgsTarget "cell 0: Variable not in scope: foo"
                `shouldBe` Nothing

    describe "missingArgType — the first mismatched argument's expected type" $ do
        it "reads the expected type from the first-argument mismatch" $
            missingArgType arityErr "takeWhileP" `shouldBe` Just "Maybe String"
        it "is Nothing for a different function's mismatch" $
            missingArgType arityErr "someOtherFn" `shouldBe` Nothing
        it "preserves a package-qualified expected type verbatim" $ do
            -- Extraction preserves; only the query layer sanitizes.
            let err =
                    T.unlines
                        [ "cell 0, line 3: Couldn't match expected type: text-2.1.2:Data.Text.Internal.Text"
                        , "            with actual type: Char -> Bool"
                        , "In the first argument of `render', namely"
                        , "  `(\\ c -> c)'"
                        ]
            missingArgType err "render"
                `shouldBe` Just "text-2.1.2:Data.Text.Internal.Text"

    describe "argFillCandidates — position-aware: vacuous fits are VALID here" $
        it "keeps Nothing from the hole fits of Maybe String" $
            argFillCandidates
                ( T.unlines
                    [ "  Valid hole fits include"
                    , "    Nothing :: forall a. Maybe a"
                    , "    mempty :: forall a. Monoid a => a"
                    ]
                )
                `shouldSatisfy` elem "Nothing"

    describe "insertArgAt — span-localized argument insertion" $ do
        let src =
                T.unlines
                    [ "pNumber = lexeme $ do"
                    , "    numStr <- takeWhileP (\\c -> isDigit c || c == '.')"
                    , "    pure numStr"
                    ]
        it "inserts the fill right after the function at the reported site" $
            insertArgAt (2, 15) "takeWhileP" "Nothing" src
                `shouldBe` Just
                    ( T.unlines
                        [ "pNumber = lexeme $ do"
                        , "    numStr <- takeWhileP Nothing (\\c -> isDigit c || c == '.')"
                        , "    pure numStr"
                        ]
                    )
        it "declines when the function is not at the reported site" $
            insertArgAt (3, 5) "takeWhileP" "Nothing" src `shouldBe` Nothing
        it "does not touch the same token inside a string literal" $
            insertArgAt (1, 1) "takeWhileP" "Nothing" "x = \"takeWhileP\""
                `shouldBe` Nothing
