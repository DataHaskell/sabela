{-# LANGUAGE OverloadedStrings #-}

{- | What the model is told when a question is asked about one cell: the
cell by id and kind, its source, its last error, and what its output shows
(a chart's spec, a table's columns). Built here from the notebook, so the
person types only the question and the box never shows a preamble.
-}
module Sabela.Server.Ai.Focus (focusPreamble) where

import Data.Aeson (Value (..), decodeStrict', encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.Api.Viz.Source (dataExprOf, specFromOutput)
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook,
    OutputItem (..),
    lookupCell,
 )

focusPreamble :: Notebook -> Int -> Maybe Text
focusPreamble nb cid = do
    cell <- lookupCell cid nb
    let kind = kindOf cell
        outputs = map oiOutput (cellOutputs cell)
    pure . T.unlines . catMaybes $
        [ Just ("[The user is asking about cell " <> tshow cid <> " (" <> kind <> ").]")
        , if cellType cell == CodeCell
            then Just ("Source:\n" <> clip 40 (cellSource cell))
            else Just ("Text:\n" <> clip 12 (cellSource cell))
        , ("Last error:\n" <>) . clip 8 <$> cellError cell
        , ("Data expression: " <>) <$> dataExprOf (cellSource cell)
        , (\s -> "Chart spec: " <> jsonText s)
            <$> listToMaybe (mapMaybe specFromOutput outputs)
        , ("Table columns: " <>) <$> listToMaybe (mapMaybe gridColumns outputs)
        , Just
            "Answer about this cell unless the user says otherwise; its id is the one tools take."
        ]
  where
    tshow = T.pack . show
    jsonText = TE.decodeUtf8 . LBS.toStrict . encode

kindOf :: Cell -> Text
kindOf cell
    | cellType cell /= CodeCell = "prose"
    | "C.chart" `T.isInfixOf` src = "chart"
    | any (("class=\"sabela-grid\"" `T.isInfixOf`) . oiOutput) (cellOutputs cell) =
        "table"
    | any
        (`T.isInfixOf` src)
        ["mkWidget", "slider \"", "dropdown \"", "button \"", "textInput \""] =
        "widget"
    | otherwise = "code"
  where
    src = cellSource cell

-- | The first lines of a text, with a note of how many were left out.
clip :: Int -> Text -> Text
clip n t =
    let ls = T.lines t
     in if length ls <= n
            then T.strip t
            else
                T.unlines (take n ls)
                    <> "… ("
                    <> T.pack (show (length ls - n))
                    <> " more lines)"

-- | A table output's columns and types, from the JSON its grid carries.
gridColumns :: Text -> Maybe Text
gridColumns html = do
    let (_, after) = T.breakOn marker html
    rest <- T.stripPrefix marker after
    let json = fst (T.breakOn "</script>" rest)
    Object o <- decodeStrict' (TE.encodeUtf8 json)
    Array cols <- KM.lookup "cols" o
    let types = case KM.lookup "types" o of
            Just (Array ts) -> [t | String t <- foldr (:) [] ts]
            _ -> []
        names = [c | String c <- foldr (:) [] cols]
    pure
        ( T.intercalate
            ", "
            ( zipWith
                (\c t -> if T.null t then c else c <> " : " <> t)
                names
                (types ++ repeat "")
            )
        )
  where
    marker = "class=\"sabela-grid-data\">"
