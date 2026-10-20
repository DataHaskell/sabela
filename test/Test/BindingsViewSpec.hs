{-# LANGUAGE OverloadedStrings #-}

module Test.BindingsViewSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.Api.Bindings (BindingEntry (..), MemoryStats (..))
import Sabela.Session.Query.BindingsView (
    parseBindings,
    parseEntry,
    parseRtsStats,
    valueCharBound,
 )

spec :: Spec
spec = describe "BindingsView (info-panel listing)" $ do
    describe "parseEntry" $ do
        it "splits an evaluated value binding into name, type and value" $
            parseEntry "total :: Int = 600"
                `shouldBe` Just (BindingEntry "total" "value" "Int" (Just "600") False)
        it "leaves an unevaluated thunk without a value" $
            parseEntry "x :: Num a => a = _"
                `shouldBe` Just (BindingEntry "x" "value" "Num a => a" Nothing False)
        it "collapses a wrapped type onto one line" $
            parseEntry "scatterSelectWith ::\n  String -> Input [Int] = _"
                `shouldBe` Just
                    ( BindingEntry
                        "scatterSelectWith"
                        "value"
                        "String -> Input [Int]"
                        Nothing
                        False
                    )
        it "keeps a data declaration under its name" $
            parseEntry "data T = ..."
                `shouldBe` Just (BindingEntry "T" "data" "data T = ..." Nothing False)
        it "drops GHCi's kind signature line" $
            parseEntry "type T :: *" `shouldBe` Nothing
        it "keeps a type synonym" $
            parseEntry "type Pair = (Int, Int)"
                `shouldBe` Just (BindingEntry "Pair" "type" "type Pair = (Int, Int)" Nothing False)
        it "bounds a long shown value" $
            fmap (fmap T.length . beValue) (parseEntry ("xs :: [Int] = " <> T.replicate 400 "1"))
                `shouldBe` Just (Just (valueCharBound + 1))

    describe "parseBindings" $ do
        let listing =
                T.unlines
                    [ "x :: Integer = 1"
                    , "old :: Integer = 2"
                    , "type T :: *"
                    , "data T = ..."
                    , "(+++) :: [a] -> [a] -> [a] = _"
                    ]
            entries = parseBindings ["x", "T", "+++"] listing
        it "keeps GHCi order and drops kind signatures" $
            map beName entries `shouldBe` ["x", "old", "T", "(+++)"]
        it "marks bindings a current cell defines as live, operators included" $
            map beLive entries `shouldBe` [True, False, True, True]

    describe "parseRtsStats" $ do
        it "reads the four figures the statement prints" $
            parseRtsStats "noise\nSABELA_RTS 1024 4096 1048576 7\n"
                `shouldBe` Just (MemoryStats 1024 4096 1048576 7)
        it "is absent when stats are off" $
            parseRtsStats "SABELA_RTS off" `shouldBe` Nothing
