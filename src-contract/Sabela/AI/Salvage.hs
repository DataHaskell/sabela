{-# LANGUAGE OverloadedStrings #-}

{- | Recover a code cell a weak model emitted as a fenced block in prose instead
of via a tool call (the "narrate-then-stop" failure). 'salvageCell' extracts the
block; 'salvageInsertSource' is the product policy that decides whether a turn
should be salvaged into an @insert_cell@. Shared by the in-notebook orchestrator
and the eval harness so both recover the same way.
-}
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

{- | Product salvage policy: a narrate-then-stop message (no tool call ran this
turn, @toolCount == 0@) whose content is EXACTLY one fenced Haskell block, under
'salvageCap', becomes a candidate @insert_cell@ source. 'Nothing' otherwise.
Conservative by design: it never salvages after any tool ran, and rejects
ambiguous multi-block or oversized content, so it cannot duplicate or truncate a
real edit.
-}
salvageInsertSource :: Int -> Text -> Maybe Text
salvageInsertSource toolCount content
    | toolCount /= 0 = Nothing
    | T.count "```" content /= 2 = Nothing
    | otherwise = case salvageCell content of
        Just src | T.length src <= salvageCap -> Just src
        _ -> Nothing

-- | Upper bound (characters) on a salvaged block; larger content is not salvaged.
salvageCap :: Int
salvageCap = 4000

{- | Drop lines of an extracted block that are a bare tool-call statement — a
known tool name (after trimming) immediately followed by @(@ and ending in @)@
with nothing else, e.g. @insert_cell()@ / @scratchpad ()@. gpt-oss sometimes
writes a tool call inside the ```haskell fence, where it would compile as
@insert_cell :: () -> t@ and wedge the episode. Conservative: only an EXACT bare
call is removed; real Haskell that merely mentions a tool name is untouched.
-}
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

-- | The known siza tool names a stray bare call inside the fence may be.
toolNames :: [Text]
toolNames =
    [ "insert_cell"
    , "replace_cell_source"
    , "scratchpad"
    , "execute_cell"
    , "delete_cell"
    , "search_capability"
    , "find_package"
    , "find_function"
    , "find_by_type"
    , "find_example_cell"
    , "check_type"
    , "list_cells"
    , "list_bindings"
    , "read_cell"
    , "await_idle"
    ]
