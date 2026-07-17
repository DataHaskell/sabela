{-# LANGUAGE OverloadedStrings #-}

{- | The application-spine reader behind arity repair. Every expression here is
one gemma4 actually wrote on the revenueTotal bench task, so the parser is pinned
to real weak-model output rather than invented shapes.
-}
module Test.SpineSpec (spineSpec) where

import Sabela.AI.Spine (Spine (..), renderSpine, splitSpine, trimTo)
import Test.Hspec

spineSpec :: Spec
spineSpec = describe "Sabela.AI.Spine" $ do
    describe "splitSpine" $ do
        it "reads the outer spine, keeping a parenthesised argument whole" $
            splitSpine "D.sum (D.col @Double \"revenue\" df) df"
                `shouldBe` Just
                    (Spine "D.sum" [] ["(D.col @Double \"revenue\" df)", "df"])

        it "reads the over-applied inner spine, separating the type application" $
            splitSpine "D.col @Double \"revenue\" df"
                `shouldBe` Just (Spine "D.col" ["@Double"] ["\"revenue\"", "df"])

        it "reads a bare atom as a head with no arguments" $
            splitSpine "df" `shouldBe` Just (Spine "df" [] [])

        it "keeps parens inside a string literal from breaking the split" $
            splitSpine "putStrLn \"a (b\" x"
                `shouldBe` Just (Spine "putStrLn" [] ["\"a (b\"", "x"])

        -- Fail closed: anything not a plain application yields no candidate,
        -- mirroring the span-less rule in Edit/HoleSearch.
        it "declines an operator application (not a plain spine)" $
            splitSpine "a + b" `shouldBe` Nothing

        it "declines a lambda" $
            splitSpine "\\x -> x" `shouldBe` Nothing

        it "declines a literal head (not an application)" $
            splitSpine "\"revenue\"" `shouldBe` Nothing

        it "declines an unbalanced expression" $
            splitSpine "D.col (x" `shouldBe` Nothing

    describe "renderSpine" $ do
        it "round-trips a spine it split" $
            (splitSpine "D.col @Double \"revenue\" df" >>= Just . renderSpine)
                `shouldBe` Just "D.col @Double \"revenue\" df"

        it "renders a trimmed spine, keeping the type application" $
            renderSpine (Spine "D.col" ["@Double"] ["\"revenue\""])
                `shouldBe` "D.col @Double \"revenue\""

    describe "trimTo (the over-application fix)" $ do
        it "drops the excess trailing argument gemma passed to D.col" $
            (splitSpine "D.col @Double \"revenue\" df" >>= \s -> Just (renderSpine (trimTo 1 s)))
                `shouldBe` Just "D.col @Double \"revenue\""

        it "is a no-op when the spine already has the right arity" $
            (splitSpine "D.col @Double \"revenue\"" >>= \s -> Just (renderSpine (trimTo 1 s)))
                `shouldBe` Just "D.col @Double \"revenue\""

        it "never grows a spine past what it had" $
            (splitSpine "D.col \"revenue\"" >>= \s -> Just (length (spArgs (trimTo 5 s))))
                `shouldBe` Just 1
