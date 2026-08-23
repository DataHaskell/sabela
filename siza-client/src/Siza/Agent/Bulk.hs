{-# LANGUAGE OverloadedStrings #-}

{- | Reading and writing a notebook in bulk, on the client side.

The server answers whole cells and whole notebooks, which is right for it and
wasteful for an agent that wants the prose of a forty-cell notebook or the one
markdown table beside a rendered chart. These are the filters that narrow what
comes back, and the plan a batch of edits turns into.
-}
module Siza.Agent.Bulk (
    applyCellFilters,
    keptOutputs,
    replaceCellsPlan,
    CellEdit,
    serverArgs,
    narrowOutcome,
    narrowCellOutputs,
    runReplaceCells,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Transport (Conn, callTool)

-- | Aeson's arrays are vectors; these keep the module to what is already a
-- dependency.
toList :: (Foldable f) => f a -> [a]
toList = foldr (:) []

filterArray :: (Foldable f, Applicative g, Monoid (g a)) => (a -> Bool) -> f a -> g a
filterArray keep = foldr (\x acc -> if keep x then pure x <> acc else acc) mempty

-- | One entry of a batch: which cell, the new source, and the hash to guard on.
type CellEdit = (Int, Text, Maybe Text)

lookupText :: Text -> Value -> Maybe Text
lookupText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> Just s
    _ -> Nothing
lookupText _ _ = Nothing

lookupInt :: Text -> Value -> Maybe Int
lookupInt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (Number n) -> Just (round n)
    _ -> Nothing
lookupInt _ _ = Nothing

{- | Narrows a notebook or cell list to one kind of cell. An unrecognised kind
leaves the answer whole: a filter nobody meant should not empty a notebook.
-}
applyCellFilters :: Value -> Value -> Value
applyCellFilters args payload = case lookupText "cell_type" args of
    Just want
        | want `elem` ["CodeCell", "ProseCell"] -> narrow want payload
    _ -> payload
  where
    narrow want (Object o) = case KM.lookup "cells" o of
        Just (Array cs) ->
            Object (KM.insert "cells" (Array (filterArray (isKind want) cs)) o)
        _ -> Object o
    narrow _ v = v
    isKind want c = lookupText "type" c == Just want

{- | Keeps the outputs of one mime type. Nothing asked for means everything
kept; a mime the cell did not produce keeps nothing, rather than falling back
to all of them and burying the caller in the thing they filtered out.
-}
keptOutputs :: Maybe Text -> [Value] -> [Value]
keptOutputs Nothing outs = outs
keptOutputs (Just mime) outs = [o | o <- outs, lookupText "oiMime" o == Just mime]

{- | Reads a batch of edits, or says why it will not run any of them. An entry
without a source would write an empty cell, so one bad entry refuses the whole
batch rather than leaving the notebook half edited.
-}
replaceCellsPlan :: Value -> Either Text [CellEdit]
replaceCellsPlan args = case args of
    Object o -> case KM.lookup "edits" o of
        Just (Array es)
            | null (toList es) -> Left "edits is empty: nothing to do"
            | otherwise -> traverse entry (toList es)
        _ -> Left "expected an `edits` array of {cell_id, new_source}"
    _ -> Left "expected an object with an `edits` array"
  where
    entry e = case (lookupInt "cell_id" e, lookupText "new_source" e) of
        (Just cid, Just src) -> Right (cid, src, lookupText "expected_hash" e)
        (Nothing, _) -> Left "an entry has no cell_id"
        (_, Nothing) -> Left "an entry has no new_source"

{- | Client-side argument keys the server has never heard of, stripped before
the call so a filter cannot look like a bad request.
-}
clientOnlyArgs :: [Text]
clientOnlyArgs = ["cell_type", "output_mime"]

serverArgs :: Value -> Value
serverArgs (Object o) =
    Object (foldr (KM.delete . K.fromText) o clientOnlyArgs)
serverArgs v = v

narrowOutcome :: (Value -> Value) -> ToolOutcome -> ToolOutcome
narrowOutcome f (ToolOk v) = ToolOk (f v)
narrowOutcome _ other = other

narrowCellOutputs :: Value -> Value -> Value
narrowCellOutputs args payload = case (lookupText "output_mime" args, payload) of
    (Just mime, Object o) -> case KM.lookup "outputs" o of
        Just (Array outs) ->
            Object
                ( KM.insert
                    "outputs"
                    (toJSON (keptOutputs (Just mime) (foldr (:) [] outs)))
                    o
                )
        _ -> payload
    _ -> payload

{- | A batch of replacements, run in order and stopped at the first failure, so
the caller is told what landed rather than left to guess.
-}
runReplaceCells :: Conn -> Text -> Value -> IO ToolOutcome
runReplaceCells conn base args = case replaceCellsPlan args of
    Left hint -> pure (ToolErr (object ["error" .= hint]))
    Right edits -> go edits []
  where
    go [] done = pure (ToolOk (object ["applied" .= reverse done]))
    go ((cid, src, mHash) : rest) done = do
        out <- callTool conn base ReplaceCellSource (editArgs cid src mHash)
        case out of
            Right (ToolOk v) -> go rest (v : done)
            Right (ToolErr e) -> pure (stopped cid e (reverse done))
            Left transport ->
                pure (stopped cid (object ["error" .= transport]) (reverse done))
    stopped cid e done =
        ToolErr
            ( object
                [ "error" .= ("stopped at cell " <> tshow cid :: Text)
                , "cause" .= e
                , "applied" .= done
                ]
            )
    editArgs cid src mHash =
        object
            ( ["cell_id" .= cid, "new_source" .= src]
                <> maybe [] (\h -> ["expected_hash" .= h]) mHash
            )
    tshow = T.pack . show
