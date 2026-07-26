{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Sabela.AI.HoogleIntent (
    ActionClass (..),
    actionNeedQueries,
    classifyAction,
    intentQueries,
) where

import Data.Char (isAlpha)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data ActionClass = RenderAction | LoadAction | ComputeAction | InstallAction
    deriving (Eq, Show)

actionStemTable :: [(ActionClass, [Text])]
actionStemTable =
    [
        ( RenderAction
        ,
            [ "plot"
            , "draw"
            , "render"
            , "anima"
            , "chart"
            , "graph"
            , "visual"
            , "scatter"
            , "svg"
            ]
        )
    ,
        ( LoadAction
        , ["load", "read", "import", "open", "pars", "csv"]
        )
    ,
        ( ComputeAction
        ,
            [ "comput"
            , "calcul"
            , "aggreg"
            , "analy"
            , "summar"
            , "averag"
            , "fit"
            , "train"
            , "predict"
            ]
        )
    ,
        ( InstallAction
        , ["install", "depend", "cabal"]
        )
    ]

actionNeedQueries :: ActionClass -> [Text]
actionNeedQueries RenderAction = ["chart library", "SVG rendering"]
actionNeedQueries LoadAction = ["data import", "file reading"]
actionNeedQueries ComputeAction = ["aggregation function", "dataframe computation"]
actionNeedQueries InstallAction = ["package installation"]

classifyAction :: Text -> Maybe ActionClass
classifyAction raw =
    fmap snd (listToMaybe (sortOn fst matches))
  where
    ws = T.words (T.toLower raw)
    matches =
        mapMaybe
            (\(cls, stems) -> (,cls) <$> earliestIndex stems ws)
            actionStemTable

earliestIndex :: [Text] -> [Text] -> Maybe Int
earliestIndex stems ws =
    listToMaybe
        [i | (i, w) <- zip [0 :: Int ..] ws, any (`T.isPrefixOf` bareWord w) stems]
  where
    bareWord = T.dropAround (not . isAlpha)

intentQueries :: Text -> [Text]
intentQueries raw = maybe [] actionNeedQueries (classifyAction raw)
