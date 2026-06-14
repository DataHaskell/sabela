{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Use <$>" -}

{- | Tests for the sticky @sabela-notebook@ FRP core. Compiling this module at
all proves the embedded support modules type-check; the examples below pin
their behaviour and the laws the documentation promises.
-}
module Test.NotebookFrpSpec (spec) where

import Sabela.Notebook.Frp
import Test.Hspec

spec :: Spec
spec = do
    describe "Behavior: the two starters" $ do
        it "time is the clock" $ at time 3.5 `shouldBe` 3.5
        it "always ignores the clock" $ at (always (7 :: Int)) 99 `shouldBe` 7

    describe "Behavior: arithmetic lifts pointwise" $ do
        it "2 * time + 1 at 5 is 11" $ at (2 * time + 1) 5 `shouldBe` (11 :: Double)
        it "sin time at 0 is 0" $ at (sin time) 0 `shouldBe` (0 :: Double)
        it "constant folds like a number" $
            at (always 10 + always 5) 0 `shouldBe` (15 :: Int)

    describe "Behavior: Functor/Applicative laws" $ do
        let b = 2 * time + 1
        it "fmap id = id" $
            map (at (fmap id b)) [0, 1, 2] `shouldBe` map (at b) [0, 1, 2]
        it "fmap (f . g) = fmap f . fmap g" $
            map (at (fmap ((+ 1) . (* 2)) b)) [0, 1]
                `shouldBe` map (at (fmap (+ 1) (fmap (* 2) b))) [0, 1]
        it "pure/<*> applies pointwise" $
            at (pure (+ 1) <*> time) 4 `shouldBe` (5 :: Double)

    describe "sampleBetween" $
        it "takes n+1 evenly spaced snapshots" $
            sampleBetween 0 4 4 time `shouldBe` [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]

    describe "Event: construction and mapping" $ do
        it "eventFromList sorts by time" $
            occurrencesOf (eventFromList [(3, 'b'), (1, 'a')])
                `shouldBe` [(1, 'a'), (3, 'b')]
        it "fmap maps values" $
            occurrencesOf (fmap (+ 10) (eventFromList [(1, 1), (2, 2)]))
                `shouldBe` [(1, 11), (2, 12)]
        it "filterE keeps matching values" $
            occurrencesOf (filterE even (eventFromList [(1, 1), (2, 2), (3, 3), (4, 4)]))
                `shouldBe` [(2, 2), (4, 4)]

    describe "Event: Monoid via merge" $ do
        let a = eventFromList [(1, 'a'), (3, 'c')]
            b = eventFromList [(2, 'b')]
        it "merge interleaves in time order" $
            occurrencesOf (merge a b) `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
        it "never is the identity" $ do
            occurrencesOf (merge a never) `shouldBe` occurrencesOf a
            occurrencesOf (merge never a) `shouldBe` occurrencesOf a
        it "is associative" $
            occurrencesOf ((a <> b) <> a) `shouldBe` occurrencesOf (a <> (b <> a))
        it "is left-biased at equal times" $
            occurrencesOf (merge (eventFromList [(1, 'L')]) (eventFromList [(1, 'R')]))
                `shouldBe` [(1, 'L'), (1, 'R')]

    describe "Event: running totals" $ do
        it "scanlE accumulates" $
            occurrencesOf (scanlE (+) 0 (eventFromList [(1, 10), (2, 20), (3, 5)]))
                `shouldBe` [(1, 10), (2, 30), (3, 35)]
        it "accumE applies update functions" $
            occurrencesOf (accumE 0 (eventFromList [(1, (+ 1)), (2, (* 10))]))
                `shouldBe` [(1, 1), (2, 10)]
        it "countE counts occurrences" $
            occurrencesOf (countE (eventFromList [(1, 'a'), (2, 'b'), (3, 'c')]))
                `shouldBe` [(1, 1), (2, 2), (3, 3)]

    describe "Event -> Behavior" $ do
        it "stepper holds the latest value" $
            map (at (stepper 0 (eventFromList [(1, 10), (3, 20)]))) [0, 2, 4]
                `shouldBe` [0, 10, 20 :: Int]
        it "switcher hands over a new behaviour at each occurrence" $
            map
                (at (switcher (always 0) (eventFromList [(1, always 5), (2, always 9)])))
                [0, 1.5, 3]
                `shouldBe` [0, 5, 9 :: Int]

    describe "Reading a behaviour when an event fires" $ do
        let held = stepper (0 :: Int) (eventFromList [(0, 100), (2, 200)])
        it "snapshot combines behaviour and event values" $
            occurrencesOf (snapshot (,) held (eventFromList [(1, 'a'), (3, 'b')]))
                `shouldBe` [(1, (100, 'a')), (3, (200, 'b'))]
        it "tag replaces with the behaviour value" $
            occurrencesOf (tag held (eventFromList [(1, ()), (3, ())]))
                `shouldBe` [(1, 100), (3, 200)]

    describe "integral / derivative" $ do
        it "integrates a constant speed into distance" $
            map (round . at (integral (always 2))) [0, 1, 2, 3]
                `shouldBe` [0, 2, 4, 6 :: Integer]
        it "integralFrom starts accumulating at t0" $
            map (round . at (integralFrom 2 (always 2))) [2, 3, 4]
                `shouldBe` [0, 2, 4 :: Integer]
        it "derivative recovers the slope" $
            map (round . at (derivative (2 * time))) [0, 1, 2]
                `shouldBe` [2, 2, 2 :: Integer]
        it "integralFrom resets a switched-in motion (the bounce fix)" $ do
            -- switch at t=2 to a fresh integral that restarts there.
            let b = switcher (always 0) (eventFromList [(2, integralFrom 2 (always 2))])
            round (at b 4) `shouldBe` (4 :: Integer) -- 2*(4-2), NOT 2*4=8
