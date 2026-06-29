{-# LANGUAGE OverloadedStrings #-}

{- | The @check_type@ record-surfacing helpers: 'typeConstructors' (the
type-constructor atoms in a resolved @name :: ty@) and 'recordDecl' (filtering a
GHCi @:info@ dump to the data/newtype declaration with its fields). Together they
let @check_type@ on a value reveal its record fields in one call, so the weak
model can record-update without a multi-call hunt for the field names.
-}
module Test.CheckTypeStructSpec (spec) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Sabela.AI.Capabilities.Query (recordDecl, typeConstructors)
import Test.Hspec

-- A representative GHCi @:info TreeConfig@ dump (record + an instance + provenance).
treeConfigInfo :: T.Text
treeConfigInfo =
    T.unlines
        [ "type TreeConfig :: *"
        , "data TreeConfig"
        , "  = TreeConfig {maxTreeDepth :: Int,"
        , "               minSamplesSplit :: Int,"
        , "               minLeafSize :: Int}"
        , "  \t-- Defined in \8216DataFrame.DecisionTree.Types\8217"
        , "instance Show TreeConfig -- Defined in \8216DataFrame.DecisionTree.Types\8217"
        ]

spec :: Spec
spec = do
    describe "typeConstructors" $ do
        it "takes the type-constructor atom from a resolved value type" $
            typeConstructors "DT.defaultTreeConfig :: D.TreeConfig"
                `shouldBe` ["D.TreeConfig"]

        it "drops the value name's qualifier on the LHS of ::" $
            typeConstructors "DT.someValue :: TreeConfig"
                `shouldNotContain` ["DT"]

        it "lists outer-to-inner constructors of a compound type, deduped" $
            typeConstructors "x :: Maybe TreeConfig"
                `shouldBe` ["Maybe", "TreeConfig"]

        it "ignores lowercase type variables" $
            typeConstructors "f :: a -> Maybe a" `shouldBe` ["Maybe"]

    describe "recordDecl" $ do
        let decl = fromMaybe "" (recordDecl treeConfigInfo)

        it "keeps the record declaration with all field names" $ do
            decl `shouldSatisfy` T.isInfixOf "maxTreeDepth"
            decl `shouldSatisfy` T.isInfixOf "minLeafSize"

        it "drops instance lines and provenance comments" $ do
            decl `shouldNotSatisfy` T.isInfixOf "instance"
            decl `shouldNotSatisfy` T.isInfixOf "Defined in"

        it "is Nothing for a type with no record fields" $
            recordDecl (T.unlines ["data Direction = GoLeft | GoRight"])
                `shouldBe` Nothing

        it "is Nothing for a plain :type result (no declaration)" $
            recordDecl "map :: (a -> b) -> [a] -> [b]" `shouldBe` Nothing
