{-# LANGUAGE OverloadedStrings #-}

{- | The simulated Hackage worlds the goal-ranking spec queries, and the small
readers that pull names and fields out of a discover envelope.
-}
module Test.DiscoverGoalRankSpec.Worlds (
    env0,
    interpFor,
    dh,
    intField,
    names,
    provWorld,
    runGuard,
    constructPlot,
    attachWorld,
    literalWorld,
    bareWorld,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Discover.Types (
    DHit (..),
    Interpreted (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Test.CatalogueSim (SimWorld (..), simWorldCall)
import Test.DiscoverFixtures (SynPkg (..), argText, field, hitText, hitsOf)

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

interpFor :: Text -> Interpreted
interpFor t = Interpreted t t Nothing "construct" "" []

dh :: Text -> Text -> Text -> Text -> DHit
dh n ty m p =
    (mkHit n m p){dhType = ty, dhOrigin = "session"}

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number n) -> round n
    _ -> (-1)

names :: Value -> [Text]
names = map (hitText "name") . hitsOf

provWorld :: SimWorld
provWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [ SynPkg
            "plume"
            "1.0.0"
            False
            [
                ( "Zzz.Deep"
                ,
                    [ ("bars", "[(Text, Double)] -> Plot -> Text")
                    , ("zzzPlot", "Plot")
                    ]
                )
            ]
        , SynPkg
            "chartx"
            "2.0.0"
            False
            [("Aaa.A", [("aaaPlot", "Plot"), ("defaultPlotLineStyle", "LineStyle")])]
        ]

runGuard :: SimWorld -> [Value] -> IO [Value]
runGuard w argsList = do
    ref <- newSearchLedger
    let inner tc = case tcName tc of
            "discover" ->
                Right
                    <$> runDiscoverCall
                        True
                        (simWorldCall w)
                        (argText "query" (tcArgs tc))
                        (tcArgs tc)
            _ -> pure (Right (ToolOk (object [])))
        outOf r = case r of
            Right (ToolOk v) -> v
            Right (ToolErr v) -> v
            Left _ -> object []
    mapM (fmap outOf . guardDiscover ref inner . ToolCall "discover") argsList

constructPlot :: Value
constructPlot =
    object ["query" .= ("Plot" :: Text), "mode" .= ("construct" :: Text)]

attachWorld :: SimWorld
attachWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [ SynPkg
            "plume"
            "1.0.0"
            False
            [("Zzz.Deep", [("bars", "[(Text, Double)] -> Plot -> Text")])]
        , SynPkg
            "framing"
            "2.0.0"
            False
            [
                ( "Fr.M"
                ,
                    [ ("zzzPlot", "Plot")
                    , ("mkPlot", "Text -> Plot")
                    , ("farPlot", "Int -> Int -> Plot")
                    ]
                )
            ]
        ]

literalWorld :: SimWorld
literalWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [ SynPkg
            "plume"
            "1.0.0"
            False
            [("Zzz.Deep", [("gust", "Int -> Wind"), ("zzzPlot", "Plot")])]
        ]

bareWorld :: SimWorld
bareWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [SynPkg "plume" "1.0.0" False [("Nimbus.Sky", [("drizzle", "Sky -> Rain")])]]
