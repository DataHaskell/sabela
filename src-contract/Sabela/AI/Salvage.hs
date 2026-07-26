{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Salvage (
    salvageCell,
    salvageInsertSource,
    salvageCap,
    dropToolCallLines,
    toolNames,
) where

import Data.Text (Text)
import qualified Data.Text as T

salvageCell :: Text -> Maybe Text
salvageCell content
    | T.null openRest = Nothing
    | T.strip tag `notElem` ["", "haskell"] = Nothing
    | T.null close = Nothing
    | T.null (T.strip cleaned) = Nothing
    | otherwise = Just (T.stripEnd cleaned)
  where
    (_, openRest) = T.breakOn "```" content
    (tag, body0) = T.break (== '\n') (T.drop 3 openRest)
    (code, close) = T.breakOn "```" (T.drop 1 body0)
    cleaned = dropToolCallLines code

salvageInsertSource :: Int -> Text -> Maybe Text
salvageInsertSource toolCount content
    | toolCount /= 0 = Nothing
    | T.count "```" content /= 2 = Nothing
    | otherwise = case salvageCell content of
        Just src | T.length src <= salvageCap -> Just src
        _ -> Nothing

salvageCap :: Int
salvageCap = 4000

dropToolCallLines :: Text -> Text
dropToolCallLines = T.unlines . filter (not . isBareToolCall) . T.lines
  where
    isBareToolCall line =
        let t = T.strip line
         in any (`isCallOf` t) toolNames
    isCallOf name t = case T.stripPrefix name t of
        Just rest ->
            let args = T.strip rest
             in not (T.null args)
                    && T.head args == '('
                    && T.last args == ')'
        Nothing -> False

toolNames :: [Text]
toolNames =
    [ "insert_cell"
    , "replace_cell_source"
    , "try"
    , "scratchpad"
    , "execute_cell"
    , "delete_cell"
    , "search_capability"
    , "find_function"
    , "find_by_type"
    , "find_example_cell"
    , "check_type"
    , "list_cells"
    , "list_bindings"
    , "read_cell"
    , "await_idle"
    ]
