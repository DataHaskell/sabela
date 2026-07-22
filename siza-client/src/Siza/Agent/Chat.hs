{-# LANGUAGE OverloadedStrings #-}

{- | @siza chat@: an interactive session that drives the local model through the
shared agent loop (scaffold, salvage, diagnostics, the verify-gate), reading
free-form requests from the terminal against one persistent notebook.

Unlike the eval harness this is built on, the verify-gate here is the light
'Siza.Agent.Check' (propose a boolean, confirm, run it) rather than the benchmark
grader, and the default view is terse: the full audit stream (system prompt,
thinking, raw tool JSON) is behind @--verbose@. Ctrl-C cancels the running
request and returns to the prompt; Ctrl-D quits.
-}
module Siza.Agent.Chat (
    ChatConfig (..),
    runChat,
) where

import Control.Exception (AsyncException (UserInterrupt), throwIO, try)
import Control.Monad (when)
import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import System.IO (hFlush, isEOF, stdout)
import System.Timeout (timeout)

import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..), chat, chatSeeded)
import Siza.Agent.Check (
    CheckResult (..),
    classifyCheck,
    counterexampleFor,
    extractTestExpr,
    interpretConfirm,
    markerSrc,
    runMarkerWith,
 )
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

{- | Interactive session configuration. The budget's deadline is the loop's
between-turn cap; 'ccRequestTimeoutSecs' is a hard wall-clock cap around the whole
request (a safety net for a single wedged model/tool call the loop cannot preempt).
-}
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
    -- Bound each request by a hard wall-clock cap and let Ctrl-C (GHC's
    -- 'UserInterrupt') abort just this request, returning to the prompt with the
    -- prior transcript intact rather than tearing down the whole session.
    runTurn prev line = do
        res <-
            try (timeout (ccRequestTimeoutSecs cfg * 1000000) (turn prev line)) ::
                IO (Either AsyncException (Maybe [Value]))
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
            Right (Just prev') -> loop prev'
    turn prev userText = do
        gateRef <- newIORef Nothing
        wroteRef <- newIORef False
        seenRef <- newIORef ([] :: [Text])
        let chatFn msgs = do
                progress "\183 thinking\8230"
                chatSeeded True Nothing mgr model msgs cat
            driver =
                Driver
                    { drvChat = chatFn
                    , drvDispatch = tracedDispatch wroteRef seenRef (dispatch conn base)
                    , drvNow = realToFrac <$> getPOSIXTime
                    , drvVerify = verifyGate mgr conn base model gateRef wroteRef
                    }
            emit = if verbose then TIO.putStr else const (pure ())
        run <- runEpisodeSeeded prev emit GrammarOn budget driver userText maxTurns
        TIO.putStrLn ("\n" <> arFinal run)
        TIO.putStrLn
            ("  [" <> arStopped run <> ", " <> tshow (arToolCalls run) <> " tool calls]\n")
        pure (arTranscript run)

{- | After a writing turn, gate acceptance on a covering check: propose one
Haskell boolean, let the user confirm/edit/skip, and run it off-notebook. A
non-writing turn passes straight through.
-}
verifyGate ::
    Manager ->
    Conn ->
    Text ->
    Text ->
    IORef (Maybe Text) ->
    IORef Bool ->
    IO (CheckResult, Maybe Text)
verifyGate mgr conn base model gateRef wroteRef = do
    wrote <- readIORef wroteRef
    if not wrote
        then pure (CheckPassed, Nothing)
        else do
            cached <- readIORef gateRef
            test <- case cached of
                Just t -> pure t
                Nothing -> do
                    progress "\183 proposing a check\8230"
                    proposed <- proposeTest mgr conn base model
                    t <- confirmTest proposed
                    writeIORef gateRef (Just t)
                    pure t
            runConfirmedTest conn base test

{- | Run the confirmed covering check: a compiling-but-False check is a real
denial (with its example); a non-compiling one has no clear target, so the
clean-running cells are accepted and the acceptance is disclosed.
-}
runConfirmedTest :: Conn -> Text -> Text -> IO (CheckResult, Maybe Text)
runConfirmedTest conn base test
    | T.null test = pure (CheckPassed, Nothing)
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
                pure (CheckPassed, Nothing)

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

-- | Prompt the user to accept, edit with a test, or skip; prose is feedback.
confirmTest :: Text -> IO Text
confirmTest proposed = do
    TIO.putStrLn ("  proposed check: " <> proposed)
    TIO.putStr
        "  [Enter]=accept \183 type a test to edit \183 'skip'=no check \8250 "
    hFlush stdout
    eof <- isEOF
    input <- if eof then pure "skip" else TIO.getLine
    let result = interpretConfirm proposed input
    when (declinedAsProse input result) $
        TIO.putStrLn
            "  (read as feedback, not a test; skipping the check. Send it as a request at the prompt.)"
    pure result

declinedAsProse :: Text -> Text -> Bool
declinedAsProse input result =
    T.null result && not (T.null low) && low `notElem` ["skip", "no", "n"]
  where
    low = T.toLower (T.strip input)

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

{- | Record this call's (tool, args) signature and flag it when it repeats one
already made this turn — the model going in circles (re-reading the same cell,
re-applying the same failing edit) instead of making progress.
-}
noteCall :: IORef [Text] -> ToolCall -> IO Text
noteCall seenRef call = do
    seen <- readIORef seenRef
    let sig = tcName call <> " " <> clip 160 (tshow (tcArgs call))
        n = length (filter (== sig) seen)
    writeIORef seenRef (sig : seen)
    pure $
        if n > 0
            then "  \8635 repeat \215" <> tshow (n + 1) <> " \8212 going in circles?"
            else ""

progress :: Text -> IO ()
progress = TIO.putStrLn . ("  " <>)

-- | The first non-blank line, for one-line progress digests.
firstLine :: Text -> Text
firstLine = headOr "" . filter (not . T.null . T.strip) . T.lines
  where
    headOr d [] = d
    headOr _ (x : _) = x

-- | Truncate to @n@ characters with an ellipsis, for one-line progress digests.
clip :: Int -> Text -> Text
clip n t
    | T.length t <= n = t
    | otherwise = T.take n t <> "\8230"

tshow :: (Show a) => a -> Text
tshow = T.pack . show
