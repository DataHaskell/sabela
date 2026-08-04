{-# LANGUAGE OverloadedStrings #-}

{- | Asking a live session a question without changing it. The GHCi commands,
the binding baseline, the value-subset type-check and the pure evaluator each
live in a submodule; this is the surface the rest of the server uses.
-}
module Sabela.Session.Query (
    queryComplete,
    queryType,
    queryInfo,
    queryKind,
    queryBrowse,
    queryDoc,
    queryHoleFits,
    queryBindings,
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckValueWith,
    typecheckLetDeclarations,
    evalPureLive,
    captureBindingsBaseline,
    scrubBindings,
    groupEntries,
) where

import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Sabela.Session (Session (..))
import Sabela.Session.Query.Bindings (
    captureBindingsBaseline,
    groupEntries,
    scrubBindings,
 )
import Sabela.Session.Query.Command (QueryCommand (..), runQueryCommand)
import Sabela.Session.Query.PureEval (evalPureLive)
import Sabela.Session.Query.Typecheck (
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckLetDeclarations,
    typecheckValueWith,
 )

queryComplete :: Session -> Text -> IO [Text]
queryComplete sess prefix = do
    surfacing <- isJust <$> lookupEnv "SABELA_INSTANCE_SURFACING"
    let countArg = if surfacing then "1000000 " else ""
    res <- runQueryCommand sess (QueryComplete (countArg <> "\"" <> prefix <> "\""))
    pure (concatMap parseCompletionLine (T.lines res))

parseCompletionLine :: Text -> [Text]
parseCompletionLine line =
    let stripped = T.strip line
     in case T.stripPrefix "\"" stripped of
            Just rest -> case T.stripSuffix "\"" rest of
                Just inner -> [inner]
                Nothing -> []
            Nothing -> []

queryType :: Session -> Text -> IO Text
queryType sess name = runQueryCommand sess (QueryType name)

queryInfo :: Session -> Text -> IO Text
queryInfo sess name = runQueryCommand sess (QueryInfo name)

queryKind :: Session -> Text -> IO Text
queryKind sess name = runQueryCommand sess (QueryKind name)

queryBrowse :: Session -> Text -> IO Text
queryBrowse sess mname = runQueryCommand sess (QueryBrowse mname)

queryDoc :: Session -> Text -> IO Text
queryDoc sess name = runQueryCommand sess (QueryDoc name)

queryHoleFits :: Session -> Text -> IO Text
queryHoleFits sess goal = runQueryCommand sess (QueryHoleFits goal)

queryBindings :: Session -> IO Text
queryBindings sess = do
    raw <- runQueryCommand sess QueryBindings
    baseline <- readIORef (sessBaselineBindings sess)
    pure (scrubBindings baseline raw)
