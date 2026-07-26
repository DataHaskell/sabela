{-# LANGUAGE OverloadedStrings #-}

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
