{-# LANGUAGE OverloadedStrings #-}

module Test.UnshowableSpec (spec) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Unshowable (
    baseTypeName,
    renderActionFor,
    unshowableContrast,
    unshowableGuidanceMessage,
    unshowableShowType,
    wrapTrailingExpression,
 )

gemmaMsg :: Text
gemmaMsg =
    "cell 7, line 15: No instance for `Show\n\
    \                   sabela-notebook-0.2.0.0:Sabela.Notebook.Picture.Internal.Picture'\n\
    \  arising from a use of `print'\n\
    \In a stmt of an interactive GHCi command: print it"

spec :: Spec
spec = describe "Sabela.AI.Unshowable — unshowable-at-print diagnostic class" $ do
    describe "unshowableShowType" $ do
        it "matches the live_gemma package-qualified, line-wrapped form" $
            unshowableShowType gemmaMsg
                `shouldBe` Just
                    "sabela-notebook-0.2.0.0:Sabela.Notebook.Picture.Internal.Picture"

        it "matches the unqualified ascii-quoted form" $
            unshowableShowType
                "No instance for `Show Picture' arising from a use of `print'"
                `shouldBe` Just "Picture"

        it "matches the unicode-quoted form" $
            unshowableShowType
                "No instance for \8216Show Picture\8217 arising from a use of \8216print\8217"
                `shouldBe` Just "Picture"

        it "matches the parenthesised constraint form" $
            unshowableShowType
                "No instance for (Show Picture) arising from a use of \8216print\8217"
                `shouldBe` Just "Picture"

        it "still fires on a function type, degrading the token" $
            unshowableShowType
                "No instance for (Show (Double -> Picture)) arising from a use of \8216print\8217"
                `shouldSatisfy` isJust

        it "ignores a Show failure with no print clause" $
            unshowableShowType "No instance for `Show Foo'" `shouldBe` Nothing

        it "ignores non-Show no-instance failures" $
            unshowableShowType
                "No instance for \8216Fractional Int\8217 arising from a use of \8216/\8217"
                `shouldBe` Nothing

        it "ignores Show-prefixed classes like ShowS" $
            unshowableShowType
                "No instance for `ShowS Picture' arising from a use of `print'"
                `shouldBe` Nothing

    describe "baseTypeName" $ do
        it "strips the package and module qualification" $
            baseTypeName
                "sabela-notebook-0.2.0.0:Sabela.Notebook.Picture.Internal.Picture"
                `shouldBe` "Picture"
        it "leaves an unqualified name alone" $
            baseTypeName "Picture" `shouldBe` "Picture"

    describe "renderActionFor" $ do
        it "names displayPicture for Picture" $
            renderActionFor "Picture"
                `shouldBe` Just ("displayPicture", Just "Sabela.Notebook")
        it "names display for a widget Input" $
            renderActionFor "Input" `shouldBe` Just ("display", Nothing)
        it "knows nothing about arbitrary types" $
            renderActionFor "Wind" `shouldBe` Nothing

    describe "wrapTrailingExpression" $ do
        it "wraps a trailing expression" $
            wrapTrailingExpression "displayPicture" "plot [(0, 0)]"
                `shouldBe` Just "displayPicture (plot [(0, 0)])\n"
        it "a comprehension arrow is not a bind" $
            wrapTrailingExpression
                "displayPicture"
                "plot [(x, sin x) | x <- [0,0.1..2*pi]]"
                `shouldSatisfy` isJust
        it "never wraps a binding" $
            wrapTrailingExpression "displayPicture" "pic = plot []"
                `shouldBe` Nothing
        it "never wraps an import" $
            wrapTrailingExpression "displayPicture" "import Sabela.Notebook"
                `shouldBe` Nothing
        it "never wraps a bind statement" $
            wrapTrailingExpression "displayPicture" "row <- fetchRow url"
                `shouldBe` Nothing

    describe "unshowableGuidanceMessage" $ do
        it "names the wrap call and its module for a known type" $ do
            let msg = unshowableGuidanceMessage gemmaMsg
            msg `shouldSatisfy` maybe False (T.isInfixOf "displayPicture (")
            msg `shouldSatisfy` maybe False (T.isInfixOf "Sabela.Notebook")
        it "falls back to the discover goal-type query for an unknown type" $ do
            let msg =
                    unshowableGuidanceMessage
                        "No instance for `Show Wind' arising from a use of `print'"
            msg `shouldSatisfy` maybe False (T.isInfixOf "Wind -> IO ()")
        it "is Nothing off-class" $ do
            unshowableGuidanceMessage "Variable not in scope: plot"
                `shouldBe` Nothing
            unshowableGuidanceMessage
                "No instance for \8216Fractional Int\8217 arising from a use of \8216/\8217"
                `shouldBe` Nothing

    describe "unshowableContrast" $ do
        it "gives the gate a one-line fix naming the wrap" $ do
            let c = unshowableContrast gemmaMsg
            c `shouldSatisfy` maybe False (T.isInfixOf "displayPicture")
            c `shouldSatisfy` maybe False (not . T.isInfixOf "\n")
        it "is Nothing off-class" $
            unshowableContrast "Variable not in scope: plot" `shouldBe` Nothing
