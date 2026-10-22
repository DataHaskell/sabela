{-# LANGUAGE OverloadedStrings #-}

{- | The flat chart spec read back from the Vega-Lite the dataframe library
emits, so a chart's own output says what the builder would need to redraw it.
-}
module Sabela.Api.Viz.FromVegaLite (fromVegaLite) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Api.Viz

fromVegaLite :: Value -> Maybe VizSpec
fromVegaLite (Object o) =
    Just
        VizSpec
            { vsBase = base
            , vsLayers = rest
            , vsTitle = text "title" o
            , vsWidth = if defaultSize then Nothing else Just (fromMaybe 600 (int "width" o))
            , vsHeight =
                if defaultSize then Nothing else Just (fromMaybe 400 (int "height" o))
            , vsSelect = Nothing
            }
  where
    layers = case KM.lookup "layer" o of
        Just (Array ls) -> mapMaybe layerOf (foldr (:) [] ls)
        _ -> []
    (base, rest) = case layers of
        (l : ls) -> (l, ls)
        [] -> (layerOfObject o, [])
    defaultSize = maybe True (== 600) (int "width" o) && maybe True (== 400) (int "height" o)
fromVegaLite _ = Nothing

layerOf :: Value -> Maybe VizLayer
layerOf (Object l) = Just (layerOfObject l)
layerOf _ = Nothing

layerOfObject :: KM.KeyMap Value -> VizLayer
layerOfObject l =
    VizLayer
        { vlMark = case KM.lookup "mark" l of
            Just (String m) -> Just m
            Just (Object m) -> text "type" m
            _ -> Nothing
        , vlChannels = case KM.lookup "encoding" l of
            Just (Object e) -> orderChannels (mapMaybe channel (KM.toList e))
            _ -> []
        }

channel :: (Key.Key, Value) -> Maybe (Text, [Enc])
channel (k, Object e) =
    Just
        ( Key.toText k
        ,
            [ Enc
                { encField = maybe "*" fieldName (text "field" e)
                , encAggregate = text "aggregate" e
                , encBin = case KM.lookup "bin" e of
                    Just (Bool True) -> Just 10
                    Just (Object b) -> Just (fromMaybe 10 (int "maxbins" b))
                    _ -> Nothing
                , encType = Nothing
                , encSort = Nothing
                , encScale = case KM.lookup "scale" e of
                    Just (Object s) -> text "type" s
                    _ -> Nothing
                , encTitle = Nothing
                , encStack = Nothing
                , encZero = case KM.lookup "scale" e of
                    Just (Object s) -> case KM.lookup "zero" s of
                        Just (Bool b) -> Just b
                        _ -> Nothing
                    _ -> Nothing
                }
            ]
        )
channel _ = Nothing

{- | A Vega-Lite field reference back to the column name: the library writes
@sepal\\.length@ for a column called @sepal.length@, because an unescaped
dot, bracket or backslash would read as nested access.
-}
fieldName :: Text -> Text
fieldName = T.pack . go . T.unpack
  where
    go ('\\' : c : rest) = c : go rest
    go (c : rest) = c : go rest
    go [] = []

text :: Text -> KM.KeyMap Value -> Maybe Text
text k o = case KM.lookup (Key.fromText k) o of
    Just (String s) -> Just s
    _ -> Nothing

int :: Text -> KM.KeyMap Value -> Maybe Int
int k o = case KM.lookup (Key.fromText k) o of
    Just (Number n) -> Just (truncate n)
    _ -> Nothing
