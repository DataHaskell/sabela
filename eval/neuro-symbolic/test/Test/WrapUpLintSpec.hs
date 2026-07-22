{-# LANGUAGE OverloadedStrings #-}

{- | R6-T3 red-then-green over the round-5 empty-final fixtures (run-130012 /
-132124): the cap-stop blank-final shape stays RED under 'stopIssues', and
'wrapUpFinal' for the same shapes passes for every stop reason and owned state.
-}
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
    , Map.fromList [(0, OwnedCell True "" "x = 1")]
    , Map.fromList [(1, OwnedCell False "not in scope: colx" "y = colx")]
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
