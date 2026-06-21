{-# LANGUAGE OverloadedStrings #-}

{- | Lightweight introspection tools: GHCi @:type@/@:info@/@:kind@/@:browse@/@:doc@,
the static API reference card, and 'explore_result' drill-down into
handles returned by other tools.
-}
module Sabela.AI.Capabilities.Query (
    execGhciQuery,
    execApiReference,
    execExploreResult,

    -- * Pieces
    runExplore,
    GhciOp (..),
    parseGhciOp,
    ExploreOp (..),
    parseExploreOp,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (field, fieldInt, fieldText)
import Sabela.AI.Handles (
    HandleId (..),
    LargeResult (..),
    grepLines,
    headLines,
    lookupHandle,
    sliceLines,
    tailLines,
 )
import Sabela.AI.ReferenceCard (sliceApiReference)
import Sabela.AI.Store
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession)

{- | The five GHCi introspection ops the schema declares an enum for.
Parsed once at the boundary so the dispatcher is total.
-}
data GhciOp = OpType | OpInfo | OpKind | OpBrowse | OpDoc
    deriving (Eq, Show)

parseGhciOp :: Text -> Maybe GhciOp
parseGhciOp "type" = Just OpType
parseGhciOp "info" = Just OpInfo
parseGhciOp "kind" = Just OpKind
parseGhciOp "browse" = Just OpBrowse
parseGhciOp "doc" = Just OpDoc
parseGhciOp _ = Nothing

execGhciQuery :: App -> Value -> IO ToolOutcome
execGhciQuery app input = do
    let rawOp = fieldText "op" input
        arg = fieldText "arg" input
    case (T.null arg, parseGhciOp rawOp) of
        (True, _) -> pure (errOutcome (errorJson "arg required"))
        (_, Nothing) ->
            pure
                ( errOutcome
                    ( errorJson
                        ( "Unknown op: "
                            <> rawOp
                            <> " (use type|info|kind|browse|doc)"
                        )
                    )
                )
        (False, Just op) -> do
            mBackend <- getHaskellSession (appSessions app)
            case mBackend of
                Nothing ->
                    pure
                        ( errOutcome
                            (errorJson "No live Haskell session — run a cell first to start GHCi.")
                        )
                Just backend -> do
                    result <- runGhciOp backend op arg
                    pure $
                        okOutcome $
                            object ["op" .= rawOp, "arg" .= arg, "result" .= result]

runGhciOp :: SessionBackend -> GhciOp -> Text -> IO Text
runGhciOp backend op arg = case op of
    OpType -> sbQueryType backend arg
    OpInfo -> sbQueryInfo backend arg
    OpKind -> sbQueryKind backend arg
    OpBrowse -> sbQueryBrowse backend arg
    OpDoc -> sbQueryDoc backend arg

execApiReference :: Value -> IO ToolOutcome
execApiReference input = do
    let mName = case field "module" input of
            Just (String s) -> s
            _ -> ""
        body = sliceApiReference mName
    pure $ okOutcome $ object ["module" .= mName, "reference" .= body]

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
