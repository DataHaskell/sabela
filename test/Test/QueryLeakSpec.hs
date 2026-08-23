{-# LANGUAGE OverloadedStrings #-}

{- | A query must not become a way to run a cell. GHCi reads @:type expr@ to
the end of the line, so an unwrapped multi-line query leaves its tail as fresh
input, which GHCi then performs: a pre-write check with side effects.
-}
module Test.QueryLeakSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Session.Query (TypecheckResult (..), typecheckValueWith)
import Sabela.Session.Query.Command (QueryCommand (..), toText)

spec :: Spec
spec = do
    describe "a query is one command, whatever its argument looks like" $ do
        it "wraps a multi-line type query so its tail cannot become input" $ do
            let sent = toText (QueryType "print\n  (1 :: Int)")
            T.lines sent `shouldSatisfy` (\ls -> head ls == ":{" && last ls == ":}")
            sent `shouldSatisfy` T.isInfixOf ":type print"

        it "wraps a single-line query the same way, so there is one shape" $
            T.lines (toText (QueryType "1 + 1"))
                `shouldBe` [":{", ":type 1 + 1", ":}"]

        it "leaves the other queries alone" $ do
            toText (QueryBrowse "Data.List") `shouldBe` ":browse Data.List"
            toText QueryBindings `shouldBe` ":show bindings"

    describe "the value gate reads GHCi's answer, not its own echo" $ do
        it "refuses a candidate GHCi called not an expression" $ do
            asked <- newIORef ([] :: [T.Text])
            result <-
                typecheckValueWith
                    (\q -> modifyIORef' asked (q :) >> pure "not an expression: 'x <- pure (1 :: Int)'")
                    (pure "bindings")
                    "print (1 :: Int)"
            tcSucceeded result `shouldBe` False

        it "accepts a candidate GHCi answered with a type" $ do
            result <-
                typecheckValueWith
                    (\_ -> pure "print (1 :: Int) :: IO ()")
                    (pure "bindings")
                    "print (1 :: Int)"
            tcSucceeded result `shouldBe` True

        it "refuses when the query disturbed the live bindings" $ do
            seen <- newIORef (0 :: Int)
            result <-
                typecheckValueWith
                    (\_ -> pure "print (1 :: Int) :: IO ()")
                    (modifyIORef' seen (+ 1) >> fmap (T.pack . show) (readIORef seen))
                    "print (1 :: Int)"
            tcSucceeded result `shouldBe` False
