{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Query (
    execCheckType,
    execFindByType,
    execDescribeFunction,
    execExploreResult,
    execPeekData,
    runExplore,
    ExploreOp (..),
    parseExploreOp,
    guidedOutcome,
    typeConstructors,
    recordDecl,
    instanceClasses,
    importScopeLines,
    withBackend,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Artefact (artefactPairs, atRowLimit)
import Sabela.AI.Capabilities.Files (readErrorOutcome)
import Sabela.AI.Capabilities.Query.CheckType (
    checkTypeAnswer,
    importScopeLines,
 )
import Sabela.AI.Capabilities.Query.Explore (
    ExploreOp (..),
    execExploreResult,
    parseExploreOp,
    runExplore,
 )
import Sabela.AI.Capabilities.Query.Struct (
    instanceClasses,
    recordDecl,
    typeConstructors,
 )
import Sabela.AI.Capabilities.Util (fieldInt, fieldText)
import Sabela.AI.Files (readLocal)
import Sabela.AI.FitRule (holeFitsJson)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Diagnose (diagnoseWith, guidancePairs)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (getHaskellSession)

withBackend :: App -> (SessionBackend -> IO ToolOutcome) -> IO ToolOutcome
withBackend app k = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing ->
            pure
                ( errOutcome
                    (errorJson "No live Haskell session — run a cell first to start GHCi.")
                )
        Just backend -> k backend

{- | A query answer, with guidance refined against @submitted@ — the text the
caller asked about, which is the only source this surface has and the one the
answer is about.
-}
guidedOutcome :: Text -> [Pair] -> Text -> ToolOutcome
guidedOutcome submitted fields result =
    okOutcome $
        object
            ( fields
                <> ["result" .= result]
                <> guidancePairs (diagnoseWith Nothing submitted result)
            )

execCheckType :: App -> Value -> IO ToolOutcome
execCheckType app input = do
    answered <- checkTypeAnswer app input
    pure $ case answered of
        Left err -> errOutcome err
        Right (fields, result) ->
            guidedOutcome expr (["expr" .= expr] <> fields) result
  where
    expr = T.strip (fieldText "expr" input)

execFindByType :: App -> Value -> IO ToolOutcome
execFindByType app input = do
    let goal = T.strip (fieldText "goal" input)
    if T.null goal
        then
            pure
                (errOutcome (errorJson "goal required (a type like \"[Int] -> Int\")"))
        else withBackend app $ \backend -> do
            let hole = if "_" `T.isPrefixOf` goal then goal else "_ :: " <> goal
            result <- sbQueryHoleFits backend hole
            let fits = holeFitsJson findByTypeCap result
            pure
                ( okOutcome
                    ( object
                        [ "goal" .= goal
                        , "fits" .= fits
                        , "shown" .= length fits
                        , "probe" .= ("typecheck-only; nothing was committed" :: Text)
                        ]
                    )
                )

findByTypeCap :: Int
findByTypeCap = 8

execDescribeFunction :: App -> Value -> IO ToolOutcome
execDescribeFunction app input = do
    let name = T.strip (fieldText "name" input)
    if T.null name
        then pure (errOutcome (errorJson "name required"))
        else withBackend app $ \backend -> do
            result <- sbQueryDoc backend name
            pure (guidedOutcome name ["name" .= name] result)

defaultPeekRows :: Int
defaultPeekRows = 10

{- | Reads through the same seam read_file does, so a path that yields no
artefact is refused in the same words by both, and only ever for the reason
the read actually met.
-}
execPeekData :: App -> Value -> IO ToolOutcome
execPeekData app input
    | T.null relPath = pure (errOutcome (errorJson "path required"))
    | otherwise = do
        got <- readLocal (envWorkDir (appEnv app)) relPath
        case got of
            Left e -> readErrorOutcome app relPath e
            Right a ->
                pure . okOutcome . object $
                    ["path" .= relPath] <> artefactPairs (atRowLimit n a)
  where
    relPath = fieldText "path" input
    n = fromMaybe defaultPeekRows (fieldInt "n" input)
