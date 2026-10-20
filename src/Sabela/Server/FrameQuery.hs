{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A table's expression box: the expression is applied in the kernel to
the frame the table was printed from, and whatever it displays comes back.
-}
module Sabela.Server.FrameQuery (
    frameQueryH,
    frameQueryBlock,
    validSlot,
    queryFunction,
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Servant (Handler)

import Sabela.Api.FrameQuery (FrameQueryRequest (..), FrameQueryResult (..))
import Sabela.Errors (scrubHarnessFrames)
import Sabela.Server.FrameFilter (filterBlock)
import Sabela.Errors.Json (plainDiagnostics)
import Sabela.Handlers.Shared (partitionExports)
import Sabela.Model (OutputItem (..), textToMime)
import Sabela.Output (parseMimeOutputs)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession)

frameQueryH :: App -> FrameQueryRequest -> Handler FrameQueryResult
frameQueryH app (FrameQueryRequest slot expr mQuery mCols mTypes) = liftIO $ do
    mSess <- getHaskellSession (appSessions app)
    case (mSess, queryFunction mQuery) of
        (_, Nothing) -> pure (failed "this table names no query function")
        _ | not (validSlot slot) -> pure (failed "this table carries no frame slot")
        _ | any isCommand (T.lines expr) -> pure (failed "GHCi commands cannot be run here")
        (Nothing, _) -> pure (failed "no GHCi session")
        (Just backend, Just fn) -> do
            busy <- ST.sbBusy backend
            if busy
                then pure (failed "the kernel is busy; try again when the cell finishes")
                else run backend fn
  where
    failed = FrameQueryResult [] . Just
    isCommand l = ":" `T.isPrefixOf` T.stripStart l
    block fn = case mCols of
        Just cols ->
            filterBlock fn slot (zip cols (fromMaybe [] mTypes ++ repeat "")) expr
        Nothing -> frameQueryBlock fn slot expr
    run backend fn = do
        r <- try (ST.sbRunBlock backend (block fn))
        pure $ case r of
            Left (e :: SomeException) -> failed (T.pack (show e))
            Right (out, err) ->
                FrameQueryResult
                    (outputItems out)
                    (errorText (ST.sbJsonDiagnostics backend) err)

outputItems :: Text -> [OutputItem]
outputItems out =
    [ OutputItem (textToMime m) b
    | (m, b) <- snd (partitionExports (parseMimeOutputs out))
    , not (T.null (T.strip b))
    ]

errorText :: Bool -> Text -> Maybe Text
errorText jsonOn err
    | T.null (T.strip plain) = Nothing
    | otherwise = Just (T.strip plain)
  where
    plain
        | jsonOn = plainDiagnostics err
        | otherwise = scrubHarnessFrames err

-- | A slot key is @cell#n@ with both parts numeric; nothing else reaches GHCi.
validSlot :: Text -> Bool
validSlot s = case T.breakOn "#" s of
    (cell, rest) ->
        let n = T.drop 1 rest
         in not (T.null cell)
                && T.all isDigit cell
                && not (T.null n)
                && T.all isDigit n

{- | The query function a table may name: one of the harness's own
@_sabela…Query@ bindings, nothing else; unnamed, the untyped one.
-}
queryFunction :: Maybe Text -> Maybe Text
queryFunction Nothing = Just "_sabelaFrameQuery"
queryFunction (Just fn)
    | "_sabela" `T.isPrefixOf` fn
        && "Query" `T.isSuffixOf` fn
        && T.all (\c -> isAlphaNum c || c == '_') fn =
        Just fn
    | otherwise = Nothing

{- | The GHCi input: the expression as the body of a lambda over @df@, each
line indented so a multi-line expression stays one statement.
-}
frameQueryBlock :: Text -> Text -> Text -> Text
frameQueryBlock fn slot expr =
    T.unlines $
        [":{", fn <> " " <> T.pack (show (T.unpack slot)) <> " (\\df ->"]
            ++ ["    " <> l | l <- T.lines expr, not (T.null (T.strip l))]
            ++ ["  )", ":}"]
