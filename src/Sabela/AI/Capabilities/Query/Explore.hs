{-# LANGUAGE OverloadedStrings #-}

{- | @explore_result@ drill-down into handles returned by other tools, split
from "Sabela.AI.Capabilities.Query" (module size cap): the four enum ops over
a stored 'LargeResult'.
-}
module Sabela.AI.Capabilities.Query.Explore (
    ExploreOp (..),
    execExploreResult,
    parseExploreOp,
    runExplore,
) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (fieldInt, fieldText)
import Sabela.AI.Handles (
    HandleId (..),
    LargeResult (..),
    grepLines,
    headLines,
    lookupHandle,
    sliceLines,
    tailLines,
 )
import Sabela.AI.Store
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)

execExploreResult :: AIStore -> Value -> IO ToolOutcome
execExploreResult store input = do
    let hidText = fieldText "handle_id" input
        op = fieldText "op" input
    if T.null hidText
        then pure (errOutcome (errorJson "handle_id required"))
        else do
            mLr <- lookupHandle (aiHandles store) (HandleId hidText)
            case mLr of
                Nothing ->
                    pure
                        (errOutcome (errorJson ("Handle not found (may have expired): " <> hidText)))
                -- 'runExplore' returns 'errorJson' for an unknown op; propagate
                -- that as a typed tool error rather than masquerading as
                -- success.
                Just lr -> case parseExploreOp op of
                    Nothing ->
                        pure (errOutcome (runExplore op input lr))
                    Just _ ->
                        pure (okOutcome (runExplore op input lr))

{- | The four 'explore_result' ops the schema declares an enum for.
Parsed at the boundary so 'runExplore' is total.
-}
data ExploreOp = ExHead | ExTail | ExSlice | ExGrep
    deriving (Eq, Show)

parseExploreOp :: Text -> Maybe ExploreOp
parseExploreOp "head" = Just ExHead
parseExploreOp "tail" = Just ExTail
parseExploreOp "slice" = Just ExSlice
parseExploreOp "grep" = Just ExGrep
parseExploreOp _ = Nothing

runExplore :: Text -> Value -> LargeResult -> Value
runExplore rawOp input lr = case parseExploreOp rawOp of
    Nothing -> errorJson ("Unknown op: " <> rawOp <> " (use head|tail|slice|grep)")
    Just ExHead ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= headLines n lr, "totalLines" .= lrTotalLines lr]
    Just ExTail ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= tailLines n lr, "totalLines" .= lrTotalLines lr]
    Just ExSlice ->
        let from = fromMaybe 1 (fieldInt "from" input)
            to = fromMaybe (from + 20) (fieldInt "to" input)
         in object
                [ "lines" .= sliceLines from to lr
                , "from" .= from
                , "to" .= to
                , "totalLines" .= lrTotalLines lr
                ]
    Just ExGrep ->
        let pat = fieldText "pattern" input
            hits = [object ["line" .= i, "text" .= t] | (i, t) <- grepLines pat lr]
         in object ["hits" .= hits, "totalLines" .= lrTotalLines lr]
