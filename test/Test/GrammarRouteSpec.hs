{-# LANGUAGE OverloadedStrings #-}

{- | E2 the seam between "regrounded" and "keep repairing". A failing in-grammar
cell routes one of two ways: a type error inside an in-grammar construct keeps
repairing (it never abandons the grammar), while a not-in-scope or no-instance
error naming a token the grammar did not offer is grammar-implicated and routes
to discover/hole-fits. Abandonment is gated on grammar-implicated errors only.
-}
module Test.GrammarRouteSpec (grammarRouteSpec) where

import Data.Text (Text)
import Sabela.Diagnose (GrammarRoute (..), routeFailure)
import Test.Hspec

offered :: [Text]
offered = ["bars", "lineGraph", "pie", "defPlot", "displaySvg"]

chartOffered :: [Text]
chartOffered = ["bars", "lineGraph", "pie", "scatter", "boxPlot", "defPlot", "displaySvg"]

revenueChartErr :: Text
revenueChartErr =
    "<no location info>: error: [GHC-35235]\n\
    \    Could not find module \8216Frames\8217.\n\
    \    It is not a module in the current program, or in any known package.\n\
    \<interactive>:232:14: error: [GHC-76037]\n\
    \    Not in scope: type constructor or class \8216Record\8217\n\
    \<interactive>:238:8: error: [GHC-76037]\n\
    \    Not in scope: \8216FI.fromList\8217\n\
    \    Note: No module named \8216FI\8217 is imported."

grammarRouteSpec :: Spec
grammarRouteSpec = describe "Sabela.Diagnose.routeFailure (E2 seam)" $ do
    describe "keep repairing inside an in-grammar construct" $ do
        it "a type mismatch on an offered call does NOT abandon the grammar" $
            routeFailure
                offered
                "<interactive>:1:13: error:\n\
                \    \8226 Couldn't match expected type \8216Double\8217 with actual type \8216[Char]\8217\n\
                \      In the first argument of \8216bars\8217, namely \8216\"12\"\8217"
                `shouldBe` KeepRepairing

        it "an ambiguous type inside an offered call keeps repairing" $
            routeFailure
                offered
                "Ambiguous type variable \8216a0\8217 arising from a use of \8216bars\8217"
                `shouldBe` KeepRepairing

        it "a no-instance arising from an offered name keeps repairing" $
            routeFailure
                offered
                "No instance for (Show Plot) arising from a use of \8216bars\8217"
                `shouldBe` KeepRepairing

        it "a not-in-scope on an offered name keeps repairing (not grammar-implicated)" $
            routeFailure
                offered
                "Variable not in scope: bars :: [(Text, Double)] -> Plot -> Text"
                `shouldBe` KeepRepairing

    describe "rediscover on a grammar-implicated error" $ do
        it "a not-in-scope on an un-offered name routes to discover" $
            routeFailure
                offered
                "Variable not in scope: plot :: [(Text, Double)] -> Plot"
                `shouldBe` Rediscover "plot"

        it "routes a bare not-in-scope (no printed type) to discover" $
            routeFailure offered "Variable not in scope: renderSvg"
                `shouldBe` Rediscover "renderSvg"

        it "a no-instance arising from an un-offered name routes to discover" $
            routeFailure
                offered
                "No instance for (Show Plot) arising from a use of \8216renderChart\8217"
                `shouldBe` Rediscover "renderChart"

    describe "nothing to route" $ do
        it "a clean / benign blob keeps repairing (nothing implicates the grammar)" $ do
            routeFailure offered "" `shouldBe` KeepRepairing
            routeFailure offered "just normal output" `shouldBe` KeepRepairing

    describe "against the recent dataframe failures (what it resolves to today)" $ do
        it
            "revenueChart's invented Frames/vinyl errors resolve to KeepRepairing — a GAP"
            $ routeFailure chartOffered revenueChartErr `shouldBe` KeepRepairing

        it
            "the same miss in GHC's lowercase \8216Variable not in scope\8217 form routes correctly"
            $ routeFailure chartOffered "Variable not in scope: Record"
                `shouldBe` Rediscover "Record"

        it
            "an invented module alone (\8216Could not find module Frames\8217) keeps repairing"
            $ routeFailure chartOffered "Could not find module \8216Frames\8217."
                `shouldBe` KeepRepairing

        it
            "revenueTotal's grade-time not-in-scope resolves to Rediscover the task binding"
            $ routeFailure offered "Variable not in scope: revenueTotal"
                `shouldBe` Rediscover "revenueTotal"
