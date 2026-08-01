{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Chat (
    ChatConfig (..),
    runChat,
    seedTranscript,
) where

import Control.Exception (AsyncException (UserInterrupt), throwIO, try)
import Data.Aeson (Value)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import System.IO (hFlush, isEOF, stdout)
import System.Timeout (timeout)

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), chatSeeded)
import Siza.Agent.Chat.Export (exportCommand, exportFileName, exportText)
import Siza.Agent.Chat.Verify (progress, verifyGate)
import Siza.Agent.Compact (compactWith)
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (GrammarOn),
    runEpisodeSeeded,
 )
import Siza.Agent.Recall (withRecallStore)
import Siza.Agent.Stack (Surface (..))
import Siza.Agent.Tools (catalogueFor, dispatch, renderOutcome)
import Siza.Transport (Conn)

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
    cat = catalogueFor ChatSurface
    banner =
        "siza chat \183 "
            <> model
            <> " \183 "
            <> base
            <> (if verbose then " \183 verbose (full audit + thinking)" else "")
    instructions =
        "This edits the LIVE notebook at that URL (adds and changes cells). Type a \
        \request; /export [path] saves the transcript as markdown; Ctrl-C cancels \
        \the current request, Ctrl-D quits.\n"
    loop prev = do
        TIO.putStr "\8250 "
        hFlush stdout
        eof <- isEOF
        if eof
            then TIO.putStrLn "\nbye"
            else do
                line <- TIO.getLine
                case exportCommand line of
                    Just mPath -> exportTranscript prev mPath >> loop prev
                    Nothing
                        | T.strip line `elem` ["quit", "exit", ":q"] ->
                            TIO.putStrLn "bye"
                        | otherwise -> runTurn prev line
    exportTranscript prev mPath
        | null prev = TIO.putStrLn "  nothing to export yet"
        | otherwise = do
            path <- maybe (exportFileName <$> getCurrentTime) pure mPath
            TIO.writeFile path (exportText model prev)
            TIO.putStrLn
                ( "  exported "
                    <> tshow (length prev)
                    <> " messages -> "
                    <> T.pack path
                )
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
        seed <- seedTranscript prev
        gateRef <- newIORef Nothing
        seenRef <- newIORef ([] :: [Text])
        feedbackRef <- newIORef Nothing
        let chatFn msgs = do
                progress "\183 thinking\8230"
                chatSeeded True Nothing mgr model msgs cat
            driver =
                Driver
                    { drvChat = chatFn
                    , drvDispatch = tracedDispatch seenRef (dispatch conn base)
                    , drvNow = realToFrac <$> getPOSIXTime
                    , drvVerify = verifyGate mgr conn base model gateRef feedbackRef
                    }
            emit = if verbose then TIO.putStr else const (pure ())
        run <- runEpisodeSeeded seed emit GrammarOn budget driver userText maxTurns
        TIO.putStrLn ("\n" <> arFinal run)
        TIO.putStrLn
            ("  [" <> arStopped run <> ", " <> tshow (arToolCalls run) <> " tool calls]\n")
        pending <- readIORef feedbackRef
        pure (arTranscript run, pending)

tracedDispatch ::
    IORef [Text] ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
tracedDispatch seenRef dsp call = do
    repeated <- noteCall seenRef call
    progress ("\8594 " <> tcName call <> repeated)
    out <- dsp call
    progress ("  " <> clip 100 (firstLine (renderOutcome out)))
    pure out

noteCall :: IORef [Text] -> ToolCall -> IO Text
noteCall seenRef call = do
    let sig = tcName call <> " " <> clip 160 (tshow (tcArgs call))
    modifyIORef' seenRef (sig :)
    pure ""

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

{- | The seed one prompt starts from: the transcript compacted against what
this conversation has already elided, with anything newly elided published so
the marker it leaves resolves for the rest of the conversation.
-}
seedTranscript :: [Value] -> IO [Value]
seedTranscript prev = withRecallStore (`compactWith` prev)
