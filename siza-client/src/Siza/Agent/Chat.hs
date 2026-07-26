{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Chat (
    ChatConfig (..),
    runChat,
) where

import Control.Exception (AsyncException (UserInterrupt), throwIO, try)
import Control.Monad (when)
import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import System.IO (hFlush, isEOF, stdout)
import System.Timeout (timeout)

import Data.Map.Strict (Map)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..), chat, chatSeeded)
import Siza.Agent.Check (
    CheckResult (..),
    classifyCheck,
    counterexampleFor,
    degenerateCheck,
    extractTestExpr,
    feedbackContinuation,
    interpretConfirm,
    markerSrc,
    runMarkerWith,
 )
import Siza.Agent.Check.Vet (vetProposal)
import Siza.Agent.Compact (compactSeed, recallResult, recallToolName)
import Siza.Agent.Discover (isOwningTool)
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (GrammarOn),
    runEpisodeSeeded,
 )
import Siza.Agent.Tools (catalogueWith, dispatch, renderOutcome)
import Siza.Transport (Conn, callTool)

data ChatConfig = ChatConfig
    { ccModel :: Text
    , ccVerbose :: Bool
    , ccBudget :: EpisodeBudget
    , ccMaxTurns :: Int
    , ccRequestTimeoutSecs :: Int
    }

runChat :: ChatConfig -> Manager -> Conn -> Text -> IO ()
runChat cfg mgr conn base = do
    TIO.putStrLn banner
    TIO.putStrLn instructions
    loop []
  where
    model = ccModel cfg
    verbose = ccVerbose cfg
    budget = ccBudget cfg
    maxTurns = ccMaxTurns cfg
    cat = catalogueWith True
    banner =
        "siza chat \183 "
            <> model
            <> " \183 "
            <> base
            <> (if verbose then " \183 verbose (full audit + thinking)" else "")
    instructions =
        "This edits the LIVE notebook at that URL (adds and changes cells). Type a \
        \request; Ctrl-C cancels the current request, Ctrl-D quits.\n"
    loop prev = do
        TIO.putStr "\8250 "
        hFlush stdout
        eof <- isEOF
        if eof
            then TIO.putStrLn "\nbye"
            else do
                line <- TIO.getLine
                if T.strip line `elem` ["quit", "exit", ":q"]
                    then TIO.putStrLn "bye"
                    else runTurn prev line
    runTurn prev line = do
        res <-
            try (timeout (ccRequestTimeoutSecs cfg * 1000000) (turn prev line)) ::
                IO (Either AsyncException (Maybe ([Value], Maybe Text)))
        case res of
            Left UserInterrupt ->
                TIO.putStrLn "\n  (cancelled \8212 back to the prompt)" >> loop prev
            Left e -> throwIO e
            Right Nothing -> do
                TIO.putStrLn
                    ( "\n  (timed out after "
                        <> tshow (ccRequestTimeoutSecs cfg)
                        <> "s \8212 back to the prompt)"
                    )
                loop prev
            Right (Just (prev', Just feedback)) -> do
                TIO.putStrLn ("\8250 " <> feedback)
                runTurn prev' feedback
            Right (Just (prev', Nothing)) -> loop prev'
    turn prev userText = do
        let (seed, store) = compactSeed prev
        gateRef <- newIORef Nothing
        wroteRef <- newIORef False
        seenRef <- newIORef ([] :: [Text])
        feedbackRef <- newIORef Nothing
        let chatFn msgs = do
                progress "\183 thinking\8230"
                chatSeeded True Nothing mgr model msgs cat
            driver =
                Driver
                    { drvChat = chatFn
                    , drvDispatch =
                        tracedDispatch
                            wroteRef
                            seenRef
                            (withRecall store (dispatch conn base))
                    , drvNow = realToFrac <$> getPOSIXTime
                    , drvVerify = verifyGate mgr conn base model gateRef wroteRef feedbackRef
                    }
            emit = if verbose then TIO.putStr else const (pure ())
        run <- runEpisodeSeeded seed emit GrammarOn budget driver userText maxTurns
        TIO.putStrLn ("\n" <> arFinal run)
        TIO.putStrLn
            ("  [" <> arStopped run <> ", " <> tshow (arToolCalls run) <> " tool calls]\n")
        pending <- readIORef feedbackRef
        pure (arTranscript run, pending)

verifyGate ::
    Manager ->
    Conn ->
    Text ->
    Text ->
    IORef (Maybe Text) ->
    IORef Bool ->
    IORef (Maybe Text) ->
    IO (CheckResult, Maybe Text)
verifyGate mgr conn base model gateRef wroteRef feedbackRef = do
    wrote <- readIORef wroteRef
    if not wrote
        then pure (CheckNotApplicable, Nothing)
        else do
            cached <- readIORef gateRef
            test <- case cached of
                Just t -> pure t
                Nothing -> do
                    progress "\183 proposing a check\8230"
                    proposed <- proposeTest mgr conn base model
                    vetted <- vetProposal (callTool conn base) proposed
                    t <-
                        if T.null (T.strip vetted)
                            then do
                                progress noCheckApplies
                                pure ""
                            else confirmTest feedbackRef vetted
                    writeIORef gateRef (Just t)
                    pure t
            runConfirmedTest conn base test

noCheckApplies :: Text
noCheckApplies =
    "  cells ran clean; no machine check applies to this deliverable"

runConfirmedTest :: Conn -> Text -> Text -> IO (CheckResult, Maybe Text)
runConfirmedTest conn base test
    | T.null test = pure (CheckNotApplicable, Nothing)
    | degenerateCheck test = do
        TIO.putStrLn
            ("  \9888 no covering check \8212 reads no notebook binding: " <> test)
        pure (CheckNotApplicable, Nothing)
    | otherwise = do
        (_, out) <- runMarkerWith (callTool conn base) (markerSrc test)
        case classifyCheck out of
            CheckPassed ->
                TIO.putStrLn ("  \10003 check passed: " <> test)
                    >> pure (CheckPassed, Nothing)
            CheckFailed -> do
                TIO.putStrLn ("  \10007 check failed: " <> test)
                mCe <- counterexampleFor (callTool conn base) test
                pure (CheckFailed, mCe)
            CheckUncheckable -> do
                TIO.putStrLn
                    ("  \9888 cannot verify \8212 the check does not compile: " <> test)
                TIO.putStrLn
                    "    accepting the clean-running cells; define a pure binding to check the value."
                pure (CheckNotApplicable, Nothing)

proposeTest :: Manager -> Conn -> Text -> Text -> IO Text
proposeTest mgr conn base model = do
    nb <- dispatch conn base (ToolCall "list_cells" (object []))
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

tracedDispatch ::
    IORef Bool ->
    IORef [Text] ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
tracedDispatch wroteRef seenRef dsp call = do
    repeated <- noteCall seenRef call
    progress ("\8594 " <> tcName call <> repeated)
    out <- dsp call
    progress ("  " <> clip 100 (firstLine (renderOutcome out)))
    when (isOwningTool (tcName call)) (writeIORef wroteRef True)
    pure out

noteCall :: IORef [Text] -> ToolCall -> IO Text
noteCall seenRef call = do
    let sig = tcName call <> " " <> clip 160 (tshow (tcArgs call))
    modifyIORef' seenRef (sig :)
    pure ""

progress :: Text -> IO ()
progress = TIO.putStrLn . ("  " <>)

firstLine :: Text -> Text
firstLine = headOr "" . filter (not . T.null . T.strip) . T.lines
  where
    headOr d [] = d
    headOr _ (x : _) = x

clip :: Int -> Text -> Text
clip n t
    | T.length t <= n = t
    | otherwise = T.take n t <> "\8230"

tshow :: (Show a) => a -> Text
tshow = T.pack . show

withRecall ::
    Map Int Text ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
withRecall store inner tc
    | tcName tc == recallToolName =
        pure (Right (ToolOk (object ["result" .= recallResult store (tcArgs tc)])))
    | otherwise = inner tc
