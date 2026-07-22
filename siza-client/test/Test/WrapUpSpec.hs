{-# LANGUAGE OverloadedStrings #-}

{- | R6-T3 budget-exhaustion wrap-up, pure half (R8.3, R9.8): the general
(stop-reason x final-content x owned) grid, the fire-once seam, and the
escalation curves. The real-loop half lives in "Test.WrapUpLoopSpec".
-}
module Test.WrapUpSpec (wrapUpSpec, searchAdvicePhrases) where

import Control.Monad (forM_)
import Data.IORef (newIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Loop.Support (nudgeFloor, nudgeK)
import Siza.Agent.Loop.WrapUp (
    BudgetView (..),
    budgetView,
    escalationK,
    missRungFloor,
    wrapUpDue,
    wrapUpFinal,
    wrapUpMarker,
    wrapUpMsg,
    wrapUpOnce,
 )
import Siza.Agent.Owned (OwnedCell (..))
import Test.DiscoverFixtures (textField)

-- Grid material ------------------------------------------------------------

stopReasons :: [Text]
stopReasons =
    [ "done"
    , "max_turns"
    , "repair_budget"
    , "deadline"
    , "stuck"
    , "stuck_reenter"
    , "error"
    ]

blankFinals :: [Text]
blankFinals = ["", "   ", " \n\t "]

ownedShapes :: [(Text, Map.Map Int OwnedCell)]
ownedShapes =
    [ ("no cells", Map.empty)
    , ("healthy", Map.fromList [(0, OwnedCell True "" "x = 1")])
    ,
        ( "red"
        , Map.fromList
            [ (0, OwnedCell True "" "x = 1")
            , (1, OwnedCell False "Variable not in scope: colx" "y = colx df")
            ]
        )
    ]

-- | A fresh, unpressured budget view (a happy path's opening turn).
freshView :: BudgetView
freshView = budgetView 25 0 4 0 0 600

-- | Search-advice phrases banned after the wrap-up or the close (R5.7).
searchAdvicePhrases :: [Text]
searchAdvicePhrases = ["retry", "different shape", "rephrase", "search again"]

wrapUpSpec :: Spec
wrapUpSpec = describe "budget-exhaustion wrap-up (R6-T3: R8.3/R9.8/R5.7)" $ do
    finalGridSpec
    dueOnceSpec
    escalationSpec

-- The pure (stop-reason x final x owned) grid --------------------------------

finalGridSpec :: Spec
finalGridSpec = describe "wrapUpFinal: empty final unrepresentable (full grid)" $ do
    it "is non-blank for every (stop reason x blank final x owned) cell" $
        forM_ stopReasons $ \s ->
            forM_ blankFinals $ \c ->
                forM_ ownedShapes $ \(label, owned) ->
                    ( (s, label, c)
                    , T.null (T.strip (wrapUpFinal s owned c))
                    )
                        `shouldSatisfy` (not . snd)
    it "passes a non-blank final through byte-identically (R9.8)" $
        forM_ stopReasons $ \s ->
            forM_ ownedShapes $ \(_, owned) ->
                wrapUpFinal s owned "All three cells are green."
                    `shouldBe` "All three cells are green."
    it "the synthesised final names the stop reason (R8.3)" $
        forM_ stopReasons $ \s ->
            forM_ ownedShapes $ \(_, owned) ->
                wrapUpFinal s owned "" `shouldSatisfy` T.isInfixOf s
    it "a red owned cell's diagnostic reaches the synthesised final" $
        wrapUpFinal "max_turns" (snd (ownedShapes !! 2)) ""
            `shouldSatisfy` T.isInfixOf "Variable not in scope: colx"

-- wrapUpDue / wrapUpOnce -----------------------------------------------------

dueOnceSpec :: Spec
dueOnceSpec = describe "wrapUpDue and the fire-once seam" $ do
    it "a fresh budget view is never due" $
        wrapUpDue freshView `shouldBe` False
    it "the last turn is due" $ do
        wrapUpDue freshView{bvTurnsLeft = 1} `shouldBe` True
        wrapUpDue freshView{bvTurnsLeft = 0} `shouldBe` True
    it "the last repair round is due only once repair spend exists" $ do
        wrapUpDue freshView{bvRepairsLeft = 1, bvRepairsSpent = 0}
            `shouldBe` False
        wrapUpDue freshView{bvRepairsLeft = 1, bvRepairsSpent = 3}
            `shouldBe` True
    it "a nearly spent deadline is due" $ do
        wrapUpDue freshView{bvTimeLeftFrac = 0.1} `shouldBe` True
        wrapUpDue freshView{bvTimeLeftFrac = 0.3} `shouldBe` False
    it "an unbounded deadline (1/0, the open eval budget) is never due" $
        wrapUpDue (budgetView 25 0 4 0 0 (1 / 0)) `shouldBe` False
    it "fires exactly once, closing the fact channel it echoes" $ do
        ref <- newIORef False
        first <-
            wrapUpOnce
                ref
                (pure ["granite (hidden): cabal line"])
                freshView{bvTurnsLeft = 1}
        second <-
            wrapUpOnce
                ref
                (pure ["granite (hidden): cabal line"])
                freshView{bvTurnsLeft = 1}
        length first `shouldBe` 1
        second `shouldBe` []
    it "never fires while not due, and stays silent afterwards too" $ do
        ref <- newIORef False
        out <- wrapUpOnce ref (pure []) freshView
        out `shouldBe` []
    it "the message names the exhausted budget, carries facts, never says search" $ do
        let msg =
                wrapUpMsg
                    ["`bars` :: [(Text, Double)] -> Plot -> Text"]
                    freshView{bvTurnsLeft = 1}
            content = textField "content" msg
        content `shouldSatisfy` T.isInfixOf wrapUpMarker
        content `shouldSatisfy` T.isInfixOf "turn budget"
        content `shouldSatisfy` T.isInfixOf "`bars` ::"
        content `shouldSatisfy` T.isInfixOf "Do not search further"
        forM_ searchAdvicePhrases $ \p ->
            (p, p `T.isInfixOf` T.toLower content) `shouldBe` (p, False)

-- Budget-proportional escalation curves --------------------------------------

escalationSpec :: Spec
escalationSpec = describe "budget-proportional escalation (R5.6/R5.9)" $ do
    it "escalationK is nudgeK above mid-budget and 1 at or past it" $
        forM_ [6, 10, 25] $ \total ->
            forM_ [0 .. total] $ \remaining ->
                escalationK total remaining
                    `shouldBe` (if 2 * remaining > total then nudgeK else 1)
    it "missRungFloor reaches held facts (rung 2) by mid-budget" $
        forM_ [6, 10, 25] $ \total ->
            forM_ [0 .. total] $ \remaining ->
                let floorRung = missRungFloor total remaining
                 in if 2 * remaining <= total
                        then
                            (total, remaining, floorRung)
                                `shouldSatisfy` \(_, _, r) -> r >= 2
                        else
                            (total, remaining, floorRung)
                                `shouldSatisfy` \(_, _, r) -> r == 1
    it "missRungFloor reaches the act-or-blocker rung at the nudge floor" $
        forM_ [0 .. nudgeFloor] $ \remaining ->
            missRungFloor 25 remaining `shouldBe` 3
    it "both curves are monotone as the budget drains" $
        forM_ [6, 10, 25] $ \total -> do
            let ks = [escalationK total r | r <- [0 .. total]]
                fs = [missRungFloor total r | r <- [total, total - 1 .. 0]]
            ks `shouldBe` sort ks
            fs `shouldBe` sort fs
