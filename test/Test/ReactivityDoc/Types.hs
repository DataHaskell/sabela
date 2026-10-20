{-# LANGUAGE OverloadedStrings #-}

-- | @reactivity.md@, "Type System & Compile-Time Reactivity".
module Test.ReactivityDoc.Types (spec) where

import Test.CellFixture (mkCell)
import Test.Hspec
import Test.ReactivityDoc.Fixture (indexOf, planFor)

spec :: Spec
spec = describe "reactivity.md: type system and compile-time reactivity" $ do
    describe "data types: removing a field from a record" $ do
        let cells =
                [ mkCell 1 "data User = User { name :: String, age :: Int }"
                , mkCell 2 "bob = User \"Bob\" 41"
                , mkCell 3 "ageOf (User _ a) = a"
                , mkCell 4 "describe :: User -> String\ndescribe = name"
                , mkCell 5 "alice = User { name = \"Alice\", age = 30 }"
                , mkCell 6 "aliceAge = age alice"
                , mkCell 7 "greeting = \"hi\""
                ]
        it "positional construction, pattern matches and signatures are invalidated" $
            planFor cells 1 `shouldSatisfy` \ids -> all (`elem` ids) [1, 2, 3, 4]
        it "an unrelated cell is left alone" $
            planFor cells 1 `shouldNotContain` [7]
        it "record-syntax construction is invalidated" $
            planFor cells 1 `shouldContain` [5]
        it "a field selector is invalidated" $
            planFor cells 1 `shouldContain` [6]

    describe "data types: GADTs" $ do
        let expr =
                "{-# LANGUAGE GADTs #-}\n\
                \data Expr a where\n\
                \  Lit :: Int -> Expr Int\n\
                \  B :: Bool -> Expr Bool"
            cells =
                [ mkCell 1 expr
                , mkCell 2 "eval :: Expr a -> a\neval e = case e of\n  Lit n -> n\n  B b -> b"
                , mkCell 3 "answer = eval (Lit 3)"
                , mkCell 4 "flag = eval (B True) && True"
                , mkCell 5 "elsewhere = 1"
                ]
        it "adding a constructor re-runs every pattern match on the type" $
            planFor cells 1 `shouldBe` [1, 2, 3, 4]
        it "altering a constructor's type index re-runs matches and their users" $
            planFor cells 1 `shouldContain` [4]
        it "the evaluator runs before the cells that call it" $ do
            let ids = planFor cells 1
            ((<) <$> indexOf 2 ids <*> indexOf 3 ids) `shouldBe` Just True
            ((<) <$> indexOf 2 ids <*> indexOf 4 ids) `shouldBe` Just True
        it "a GADT edit does not reach unrelated cells" $
            planFor cells 1 `shouldNotContain` [5]

    describe "typeclasses: structural class changes" $ do
        let cells =
                [ mkCell
                    1
                    "class Shape a where\n  area :: a -> Double\n  perimeter :: a -> Double"
                , mkCell 2 "data Sq = Sq Double"
                , mkCell
                    3
                    "instance Shape Sq where\n  area (Sq s) = s * s\n  perimeter (Sq s) = 4 * s"
                , mkCell 4 "total = area (Sq 2) + perimeter (Sq 2)"
                , mkCell 5 "data Circle = Circle Double"
                , mkCell
                    6
                    "instance Shape Circle where\n  area (Circle r) = pi * r * r\n  perimeter (Circle r) = 2 * pi * r"
                ]
        it "adding or removing a method rebuilds every instance and user" $
            planFor cells 1 `shouldBe` [1, 3, 6, 4]
        it "the class runs before its instances, which run before users" $ do
            let ids = planFor cells 1
            ((<) <$> indexOf 1 ids <*> indexOf 3 ids) `shouldBe` Just True
            ((<) <$> indexOf 3 ids <*> indexOf 4 ids) `shouldBe` Just True
        it "editing one instance re-runs users but not the sibling instance" $
            planFor cells 3 `shouldBe` [3, 4]

    describe "typeclasses: instance behaviour updates" $ do
        let cells =
                [ mkCell 1 "data Color = Red | Green"
                , mkCell
                    2
                    "instance Show Color where\n  show Red = \"Stop\"\n  show Green = \"Go\""
                , mkCell 3 "label = show (Red :: Color)"
                , mkCell 4 "sign = show Red"
                , mkCell 5 "digits = show (1 :: Int)"
                ]
        it "editing the instance re-runs a user that names the type" $
            planFor cells 2 `shouldContain` [3]
        it "a Show user of another type is untouched" $
            planFor cells 2 `shouldNotContain` [5]
        it "editing the instance re-runs a user that only names a constructor" $
            planFor cells 2 `shouldContain` [4]

    describe "typeclasses: overlapping instances" $ do
        let cells =
                [ mkCell 1 "class Default a where\n  def :: a"
                , mkCell 2 "instance {-# OVERLAPPABLE #-} Num a => Default a where\n  def = 0"
                , mkCell 3 "x = def :: Int"
                , mkCell 4 "instance Default Int where\n  def = 42"
                ]
        it "adding a more specific instance makes consumers re-resolve" $
            planFor cells 4 `shouldBe` [4, 3]
        it "editing the general instance also re-runs consumers" $
            planFor cells 2 `shouldBe` [2, 3]
        it "the two instances do not depend on each other" $ do
            planFor cells 4 `shouldNotContain` [2]
            planFor cells 2 `shouldNotContain` [4]

    describe "functional dependencies" $ do
        let cells =
                [ mkCell
                    1
                    "{-# LANGUAGE FunctionalDependencies #-}\n\
                    \class Container f e | f -> e where\n\
                    \  empty :: f\n\
                    \  insert :: e -> f -> f"
                , mkCell 2 "instance Container [Int] Int where\n  empty = []\n  insert = (:)"
                , mkCell 3 "xs = insert 1 empty :: [Int]"
                , mkCell 4 "n = length xs"
                ]
        it "removing the fundep recompiles instances and every consumer" $
            planFor cells 1 `shouldBe` [1, 2, 3, 4]

    describe "closed type families" $ do
        let cells =
                [ mkCell
                    1
                    "{-# LANGUAGE TypeFamilies #-}\ntype family Elem c where\n  Elem [a] = a"
                , mkCell 2 "firstElem :: [a] -> Elem [a]\nfirstElem = head"
                , mkCell 3 "v = firstElem [1 :: Int, 2]"
                , mkCell 4 "w = 7"
                ]
        it "changing an equation invalidates signatures that rely on it" $
            planFor cells 1 `shouldBe` [1, 2, 3]
        it "a cell that mentions no family is not invalidated" $
            planFor cells 1 `shouldNotContain` [4]
