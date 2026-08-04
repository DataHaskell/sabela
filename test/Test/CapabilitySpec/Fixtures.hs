{-# LANGUAGE OverloadedStrings #-}

-- | The capability index and :browse transcript both capability specs search.
module Test.CapabilitySpec.Fixtures (
    cap,
    idx,
    top,
    via,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    Match (..),
    defaultSynonyms,
    searchCapabilities,
 )

cap :: Text -> Text -> Text -> Capability
cap m n t = Capability m n t Nothing

idx :: [Capability]
idx =
    [ cap
        "Sabela.Notebook.Anim"
        "animate"
        "Double -> (Double -> Picture) -> IO ()"
    , cap
        "Sabela.Notebook.Anim"
        "animateB"
        "Double -> Behavior Picture -> IO ()"
    , cap
        "DataFrame.LinearModel.Logistic"
        "defaultLogisticConfig"
        "LogisticConfig"
    , cap
        "DataFrame.LinearModel.Regression"
        "defaultLinearConfig"
        "LinearConfig"
    , cap "DataFrame.Model" "fit" "cfg -> Expr Double -> DataFrame -> model"
    , cap
        "DataFrame.Operations.Statistics"
        "summarize"
        "DataFrame -> DataFrame"
    , cap
        "Granite.Svg"
        "lineGraph"
        "[(Text, [(Double, Double)])] -> Plot -> Text"
    , cap "Granite.Svg" "bars" "[(Text, Double)] -> Plot -> Text"
    , cap "Sabela.Notebook" "group" "[Picture] -> Picture"
    , cap "Sabela.Notebook" "displayPicture" "Picture -> IO ()"
    ]

top :: Text -> Maybe (Text, Text)
top q = case searchCapabilities defaultSynonyms idx q of
    (h : _) -> Just (capModule (hitCap h), capName (hitCap h))
    [] -> Nothing

via :: Text -> Maybe Match
via q = case searchCapabilities defaultSynonyms idx q of
    (h : _) -> Just (hitVia h)
    [] -> Nothing
