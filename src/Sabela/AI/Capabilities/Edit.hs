{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.Edit (
    InsertRoute (..),
    insertRoute,
    InsertAttempt (..),
    nextInsertAttempt,
    insertRetryFuel,
    discloseSupersede,
    execReplaceCellSource,
    execProposeEdit,
    execInsertCell,
    execDeleteCell,
    applyReplaceCellSource,
    proceedProposeEdit,
    conflictJson,
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
) where

import Control.Monad (when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Ack (WriteRun (..), ackWriteAndRun, withNote)
import Sabela.AI.Capabilities.Edit.Admission (
    Admission (..),
    admissionNotes,
 )
import Sabela.AI.Capabilities.Edit.Admit (
    conflictJson,
    pendingErrorFor,
    sigBodyProposalFor,
    supersedeNote,
    supersedesRedCell,
    violationJson,
 )
import Sabela.AI.Capabilities.Edit.GateRepair (gatedCandidate)
import Sabela.AI.Capabilities.Edit.Propose (
    execProposeEdit,
    proceedProposeEdit,
 )
import Sabela.AI.Capabilities.Edit.Replace (
    applyReplaceCellSource,
    execReplaceCellSource,
    execSupersedeCell,
 )
import Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
 )
import Sabela.AI.Capabilities.Edit.Submission (
    Submission,
    compiledText,
    insertSubmission,
    submissionNotes,
 )
import Sabela.AI.Capabilities.Edit.ValueGate (prewriteValueVeto)
import Sabela.AI.Capabilities.Try.Payload.Checked (RunRecord)
import Sabela.AI.Capabilities.Util (
    fieldInt,
    fieldText,
    parseCellLang,
    parseCellType,
 )
import Sabela.AI.CellResult (deferredCellResult)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.AI.WriteAck (deferredNote)
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (InsertAt (..), errorJson)
import Sabela.Handlers (
    NotebookViolation (..),
    ReactiveNotebook (..),
    checkedAppend,
    checkedInsertAt,
    pendingError,
 )
import Sabela.Model
import Sabela.Parse (validateCellShape)
import Sabela.Reactivity (markRootedDirty)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

{- | Where an insert goes, given the notebook's pending error and the source
being written. Every route that can commit must pass a compile gate first.
-}
data InsertRoute
    = RouteGateThenAppend
    | RouteSupersede Int
    | RouteRefuse Int
    deriving (Eq, Show)

insertRoute :: Maybe (Int, Text) -> Text -> InsertRoute
insertRoute Nothing _ = RouteGateThenAppend
insertRoute (Just (cid, redSrc)) src
    | supersedesRedCell src redSrc = RouteSupersede cid
    | otherwise = RouteRefuse cid

{- | Says an append became an overwrite of @cid@, on whatever the replace came
back as: a reroute the caller cannot see is a reroute it will repeat.
-}
discloseSupersede :: Int -> ToolOutcome -> ToolOutcome
discloseSupersede cid = withNote (supersedeNote cid)

-- | Every note the harness owes the caller, oldest first.
withNotes :: [Text] -> ToolOutcome -> ToolOutcome
withNotes ns out = foldl' (flip withNote) out ns

execInsertCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execInsertCell app store rn cancelTok input = do
    let src = fieldText "source" input
        rawType = fieldText "cell_type" input
        rawLang = fieldText "language" input
        mLang = if T.null rawLang then Just Haskell else parseCellLang rawLang
        mType = if T.null rawType then Just CodeCell else parseCellType rawType
    case (mType, mLang) of
        (Nothing, _) ->
            pure
                ( errOutcome
                    ( errorJson
                        ("Unknown cell_type: " <> rawType <> ". Expected CodeCell or ProseCell.")
                    )
                )
        (_, Nothing) ->
            pure
                ( errOutcome
                    ( errorJson
                        ("Unknown language: " <> rawLang <> ". Expected Haskell or Python.")
                    )
                )
        (Just rawTp, Just rawLg) -> do
            let (cellTp, sub) = insertSubmission rawTp src
                src' = compiledText sub
                lang = if cellTp /= rawTp then Haskell else rawLg
            mVeto <- prewriteValueVeto app lang cellTp src'
            case (mVeto, validateCellShape cellTp src') of
                (Just veto, _) -> pure veto
                (_, Just msg) | lang == Haskell -> pure (errOutcome (errorJson msg))
                _
                    | Just prop <- sigBodyProposalFor lang cellTp src' ->
                        pure (errOutcome prop)
                _ -> do
                    nb <- readNotebook (appNotebook app)
                    case placementOf (fieldInt "after_cell_id" input) nb of
                        Left err -> pure (errOutcome (errorJson err))
                        Right mAt -> do
                            nid <- freshCellId (appNotebook app)
                            let cell = Cell nid cellTp lang src' [] Nothing True
                            routeInsert app store rn cancelTok input cell sub mAt insertRetryFuel

{- | Where an insert lands: absent means append, -1 the top, otherwise after
the named cell — refused up front when that anchor does not exist. An anchor
deleted mid-flight degrades to append rather than failing the write.
-}
placementOf :: Maybe Int -> Notebook -> Either Text (Maybe InsertAt)
placementOf Nothing _ = Right Nothing
placementOf (Just (-1)) _ = Right (Just AtBeginning)
placementOf (Just n) nb
    | Just _ <- lookupCell n nb = Right (Just (After n))
    | otherwise =
        Left ("after_cell_id " <> T.pack (show n) <> " does not name a cell.")

{- | Sends the write down the one route its notebook state allows. A red
notebook never reaches the plain append, so no branch can land a cell that
the compile gate has not proven.
-}
routeInsert ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Value ->
    Cell ->
    Submission ->
    Maybe InsertAt ->
    Int ->
    IO ToolOutcome
routeInsert app store rn cancelTok input cell sub mAt fuel = do
    peek <- readNotebook (appNotebook app)
    let redWithSource = do
            (cid, _) <- pendingError peek
            c <- lookupCell cid peek
            pure (cid, cellSource c)
        src' = compiledText sub
        notes = submissionNotes sub
    case insertRoute redWithSource src' of
        RouteSupersede cid -> supersede app store rn cancelTok cid sub
        RouteRefuse cid ->
            pure
                ( errOutcome
                    ( maybe
                        (violationJson (VPendingError cid ""))
                        (pendingErrorFor cid . cellSource)
                        (lookupCell cid peek)
                    )
                )
        RouteGateThenAppend -> do
            gate <- gatedCandidate app Nothing (cellLang cell) (cellType cell) sub
            case gate of
                Left rejection -> pure (withNotes notes (errOutcome rejection))
                Right admission ->
                    commitInsert
                        app
                        store
                        rn
                        cancelTok
                        input
                        cell{cellSource = admittedSource admission}
                        sub
                        (`admissionNotes` admission)
                        mAt
                        fuel

supersede ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    Submission ->
    IO ToolOutcome
supersede app store rn cancelTok cid sub =
    discloseSupersede cid <$> execSupersedeCell app store rn cancelTok cid sub

commitInsert ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Value ->
    Cell ->
    Submission ->
    (RunRecord -> [Text]) ->
    Maybe InsertAt ->
    Int ->
    IO ToolOutcome
commitInsert app store rn cancelTok input cell sub gateNotes mAt fuel = do
    let admit = maybe checkedAppend checkedInsertAt mAt
    res <- atomicEditNotebook (appNotebook app) $ \nb ->
        case admit cell nb of
            Left v -> (nb, Left v)
            Right nb' -> (nb', Right ())
    case res of
        Left v -> case nextInsertAttempt fuel v of
            RetryInsert fuel' ->
                routeInsert app store rn cancelTok input cell sub mAt fuel'
            AbandonInsert v' -> pure (errOutcome (violationJson v'))
        Right () -> do
            broadcastNotebook app
            mode <- getRunMode app
            let runnable =
                    cellType cell == CodeCell
                        && cellLang cell == Haskell
                        && not (T.null (T.strip (cellSource cell)))
                deferred = runnable && mode == RunDeferred
                disposition
                    | deferred = SettleWith (toJSON deferredCellResult)
                    | runnable = RunNow
                    | otherwise = SettleWith Null
            when deferred $ do
                modifyNotebook (appNotebook app) (markRootedDirty (cellId cell))
                broadcastNotebookState app
            ackWriteAndRun
                app
                store
                rn
                cancelTok
                input
                cell
                disposition
                (\run -> submissionNotes sub <> gateNotes run <> [deferredNote | deferred])

{- | Whether a notebook that turned red between the route's read and the
atomic append earns another pass. Bounded: the two can keep flipping, and a
retry costs a full disposable build.
-}
data InsertAttempt = RetryInsert Int | AbandonInsert NotebookViolation

nextInsertAttempt :: Int -> NotebookViolation -> InsertAttempt
nextInsertAttempt fuel v = case v of
    VPendingError _ _ | fuel > 0 -> RetryInsert (fuel - 1)
    _ -> AbandonInsert v

insertRetryFuel :: Int
insertRetryFuel = 2

execDeleteCell :: App -> Value -> IO ToolOutcome
execDeleteCell app input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            modifyNotebook (appNotebook app) $ \nb ->
                nb{nbCells = filter (\c -> cellId c /= cid) (nbCells nb)}
            broadcastNotebook app
            pure (okOutcome (object ["deleted" .= True, "cellId" .= cid]))
