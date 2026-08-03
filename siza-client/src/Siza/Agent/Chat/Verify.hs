{-# LANGUAGE OverloadedStrings #-}

{- | The chat loop's covering check: proposing one, vetting it against the
names this turn defined, and running it. Split out of "Siza.Agent.Chat" so the
gate's honesty rules read in one place.
-}
module Siza.Agent.Chat.Verify (
    NotebookKey,
    notebookKey,
    progress,
    runConfirmedTest,
    verifyGate,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (Manager)
import System.IO (hFlush, isEOF, stdout)

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome, toolOutcomeValue)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..), chat)
import Siza.Agent.Check (
    CheckResult (..),
    NoVerdict (..),
    classifyMarker,
    counterexampleFor,
    degenerateCheck,
    extractTestExpr,
    feedbackContinuation,
    interpretConfirm,
    markerSrc,
    noVerdictNote,
    runMarkerWith,
 )
import Siza.Agent.Check.Vet (ownedDefines, vetProposal)
import Siza.Agent.Owned (OwnedCell, noWriteReason)
import Siza.Agent.Tools (dispatch, renderOutcome)
import Siza.Transport (Conn, callTool)

progress :: Text -> IO ()
progress = TIO.putStrLn . ("  " <>)

{- | The covering check for this turn. It runs only when this episode has a
committed, substantive cell to check, is proposed against the notebook as it
stands (re-proposed whenever the cells change), and is read against the names
this episode caused to exist.
-}
verifyGate ::
    Manager ->
    Conn ->
    Text ->
    Text ->
    IORef (Maybe (NotebookKey, Text)) ->
    IORef (Maybe Text) ->
    Map CellId OwnedCell ->
    IO (CheckResult, Maybe Text)
verifyGate mgr conn base model gateRef feedbackRef owned
    | Just why <- noWriteReason owned =
        pure (CheckNotApplicable, Just (noVerdictNote why))
    | otherwise = do
        nb <- dispatch conn base (ToolCall "list_cells" (object []))
        let key = notebookKey nb
        cached <- readIORef gateRef
        test <- case cached of
            Just (k, t) | k == key -> pure t
            _ -> do
                progress "\183 proposing a check\8230"
                proposed <- proposeTest mgr model nb
                vetted <-
                    vetProposal
                        (callTool conn base)
                        (ownedDefines (Map.keys owned) nb)
                        proposed
                t <-
                    if T.null (T.strip vetted)
                        then do
                            progress noCheckApplies
                            pure ""
                        else confirmTest feedbackRef vetted
                writeIORef gateRef (Just (key, t))
                pure t
        runConfirmedTest conn base test

{- | The cells a check was proposed against. A check outlives its notebook
only if nothing was inserted, replaced or deleted since.
-}
type NotebookKey = [(Int, Text)]

notebookKey :: Either Text ToolOutcome -> NotebookKey
notebookKey out =
    [ (round i, hashOf c)
    | Object cells <- [either (const Null) toolOutcomeValue out]
    , Just (Array cs) <- [KM.lookup "cells" cells]
    , Object c <- toList cs
    , Just (Number i) <- [KM.lookup "id" c]
    ]
  where
    hashOf c = case KM.lookup "hash" c of
        Just (String s) -> s
        _ -> ""

noCheckApplies :: Text
noCheckApplies =
    "  no covering check survived vetting for this deliverable"

runConfirmedTest :: Conn -> Text -> Text -> IO (CheckResult, Maybe Text)
runConfirmedTest conn base test
    | T.null test =
        pure (CheckNotApplicable, Just (noVerdictNote NoCheckProposed))
    | degenerateCheck test = do
        TIO.putStrLn
            ("  \9888 no covering check \8212 reads no notebook binding: " <> test)
        pure (CheckNotApplicable, Just (noVerdictNote (CheckReadsNothing test)))
    | otherwise = do
        run <- runMarkerWith (callTool conn base) (markerSrc test)
        case classifyMarker run of
            CheckPassed ->
                TIO.putStrLn ("  \10003 check passed: " <> test)
                    >> pure (CheckPassed, Just test)
            CheckFailed -> do
                TIO.putStrLn ("  \10007 check failed: " <> test)
                mCe <- counterexampleFor (callTool conn base) test
                pure (CheckFailed, mCe)
            _ -> do
                TIO.putStrLn
                    ("  \9888 cannot verify \8212 the check did not run: " <> test)
                pure
                    ( CheckNotApplicable
                    , Just (noVerdictNote (CheckDidNotRun test))
                    )

proposeTest :: Manager -> Text -> Either Text ToolOutcome -> IO Text
proposeTest mgr model nb = do
    let msgs =
            [ object ["role" .= ("system" :: Text), "content" .= proposeSystem]
            , object
                [ "role" .= ("user" :: Text)
                , "content"
                    .= ("Notebook cells:\n" <> renderOutcome nb <> "\n\nPropose the check.")
                ]
            ]
    r <- chat mgr model msgs []
    pure $ case r of
        Right t -> extractTestExpr (turnContent t)
        Left _ -> ""

proposeSystem :: Text
proposeSystem =
    "Propose ONE Haskell boolean expression that checks the notebook's result is \
    \correct, over the bindings the cells define (e.g. `total == 600` or \
    \`length xs == 3`). Reply with ONLY the expression, no prose, no code fence."

confirmTest :: IORef (Maybe Text) -> Text -> IO Text
confirmTest feedbackRef proposed = do
    TIO.putStrLn ("  proposed check: " <> proposed)
    TIO.putStr
        "  [Enter]=accept \183 type a test to edit \183 'skip'=no check \8250 "
    hFlush stdout
    eof <- isEOF
    input <- if eof then pure "skip" else TIO.getLine
    case feedbackContinuation proposed input of
        Just fb -> do
            writeIORef feedbackRef (Just fb)
            TIO.putStrLn
                "  (read as feedback, not a test; continuing with it as your next request.)"
        Nothing -> pure ()
    pure (interpretConfirm proposed input)
