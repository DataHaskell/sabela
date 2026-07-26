{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | The action-verb classifier for "Sabela.AI.HoogleProse": names the
ACTION a request needs (render\/load\/compute\/install) so a query for that
need is tried before the ladder ever isolates a bare object noun.
-}
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

-- | The action shape a request's verb names.
data ActionClass = RenderAction | LoadAction | ComputeAction | InstallAction
    deriving (Eq, Show)

{- | Each class's verb stems, matched as a case-insensitive word prefix so
one stem covers a verb's inflections ("plot"/"plots"/"plotting"). Extend this
table for a new class rather than special-casing a library or module name.
-}
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

{- | The query terms a class's need is searched with, tried ahead of the
ladder's object-noun stages — additive, never a replacement for them.
-}
actionNeedQueries :: ActionClass -> [Text]
actionNeedQueries RenderAction = ["chart library", "SVG rendering"]
actionNeedQueries LoadAction = ["data import", "file reading"]
actionNeedQueries ComputeAction = ["aggregation function", "dataframe computation"]
actionNeedQueries InstallAction = ["package installation"]

{- | The request's action class: the class whose verb stem appears EARLIEST
in the text wins, so "install the plotting library" reads as an install
request, not a render one. 'Nothing' when no class's stem appears at all.
-}
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

-- | The action-need queries a raw request emits; empty when unclassified.
intentQueries :: Text -> [Text]
intentQueries raw = maybe [] actionNeedQueries (classifyAction raw)
