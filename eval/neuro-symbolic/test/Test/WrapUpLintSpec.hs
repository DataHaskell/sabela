{-# LANGUAGE OverloadedStrings #-}

module Test.WrapUpLintSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Eval.TranscriptLint (LintIssue (..), stopIssues)
import Siza.Agent.Loop.WrapUp (wrapUpFinal)
import Siza.Agent.Owned (OwnedCell (..))

capStops :: [Text]
capStops = ["max_turns", "repair_budget", "deadline"]

allStops :: [Text]
allStops = capStops ++ ["done", "stuck", "stuck_reenter", "error"]

ownedShapes :: [Map.Map Int OwnedCell]
ownedShapes =
    [ Map.empty
    , Map.fromList [(0, OwnedCell True True "" "x = 1" False True Nothing)]
    , Map.fromList
        [(1, OwnedCell False True "not in scope: colx" "y = colx" False True Nothing)]
    ]

spec :: Spec
spec = describe "R6-T3: the wrap-up final closes the empty-final class" $ do
    it "the round-5 fixture shape (cap stop, blank final) stays red" $
        forM_ capStops $ \stopped ->
            map liRule (stopIssues stopped "") `shouldBe` ["empty-final"]
    it "wrapUpFinal of a blank final passes the lint on the full grid" $
        forM_ allStops $ \stopped ->
            forM_ ownedShapes $ \owned ->
                ( stopped
                , map liRule (stopIssues stopped (wrapUpFinal stopped owned ""))
                )
                    `shouldBe` (stopped, [])
