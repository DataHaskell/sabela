{-# LANGUAGE OverloadedStrings #-}

{- | siza-chat: an interactive session that drives the local model through a
harness loop (scaffold, salvage, diagnostics, the verify-gate)
but reading free-form requests instead of a fixed task, against one persistent
notebook.
-}
module Eval.Chat (
    runChat,
    extractTestExpr,
    interpretConfirm,
) where

import Control.Monad (when)
import Data.Aeson (object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import System.IO (hFlush, isEOF, stdout)

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeTraced,
    runEpisodeWith,
 )
import Eval.Discover (isOwningTool)
import Eval.Ollama (ToolCall (..), Turn (..), chat, chatSeeded)
import Eval.Salvage (salvageCell)
import Eval.Task (Grader (..), Task (..), Verdict (..), grade)
import Eval.Tools (catalogue, dispatch, renderOutcome)
import Sabela.AI.Types (ToolOutcome)
import Siza.Transport (Conn)

runChat ::
    Bool -> EpisodeBudget -> Int -> Manager -> Conn -> Text -> Text -> IO ()
runChat debug budget maxTurns mgr conn base model = do
    TIO.putStrLn ("siza-chat \183 " <> model <> " \183 " <> base <> debugTag)
    TIO.putStrLn
        "Type a request; the model works in the notebook and proposes a check you confirm. Ctrl-D to quit.\n"
    loop
  where
    debugTag = if debug then " \183 debug (full audit + thinking)" else ""
    loop = do
        TIO.putStr "\8250 "
        hFlush stdout
        eof <- isEOF
        if eof
            then TIO.putStrLn "\nbye"
            else do
                line <- TIO.getLine
                if T.strip line `elem` ["quit", "exit", ":q"]
                    then TIO.putStrLn "bye"
                    else turn line >> loop
    turn userText = do
        gateRef <- newIORef Nothing
        wroteRef <- newIORef False
        let chatFn msgs = do
                progress "\183 thinking\8230"
                (if debug then chatSeeded True Nothing else chat) mgr model msgs catalogue
            driver =
                Driver
                    { drvChat = chatFn
                    , drvDispatch = tracedDispatch wroteRef (dispatch conn base)
                    , drvNow = realToFrac <$> getPOSIXTime
                    , drvVerify = verifyGate mgr conn base model gateRef wroteRef
                    }
            task = Task "_chat" userText Untested
        run <-
            if debug
                then runEpisodeTraced TIO.putStr GrammarOn budget driver task maxTurns
                else runEpisodeWith budget driver task maxTurns
        TIO.putStrLn ("\n" <> arFinal run)
        TIO.putStrLn
            ("  [" <> arStopped run <> ", " <> tshow (arToolCalls run) <> " tool calls]\n")

verifyGate ::
    Manager -> Conn -> Text -> Text -> IORef (Maybe Text) -> IORef Bool -> IO Bool
verifyGate mgr conn base model gateRef wroteRef = do
    wrote <- readIORef wroteRef
    if not wrote
        then pure True
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

runConfirmedTest :: Conn -> Text -> Text -> IO Bool
runConfirmedTest conn base test
    | T.null test = pure True
    | otherwise = do
        (v, _) <- grade conn base (Task "_chk" "" (ByValue test))
        let ok = v == Surfaced
            mark = if ok then "\10003 check passed: " else "\10007 check failed: "
        TIO.putStrLn ("  " <> mark <> test)
        pure ok

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

interpretConfirm :: Text -> Text -> Text
interpretConfirm proposed input
    | low `elem` ["skip", "no", "n"] = ""
    | T.null low || low `elem` ["y", "yes"] = proposed
    | looksLikeTest stripped = stripped
    | otherwise = ""
  where
    stripped = T.strip input
    low = T.toLower stripped

looksLikeTest :: Text -> Bool
looksLikeTest t =
    any
        (`T.isInfixOf` t)
        ["==", "/=", "<=", ">=", "<", ">", "&&", "||", " elem ", "isInfixOf"]

declinedAsProse :: Text -> Text -> Bool
declinedAsProse input result =
    T.null result && not (T.null low) && low `notElem` ["skip", "no", "n"]
  where
    low = T.toLower (T.strip input)

extractTestExpr :: Text -> Text
extractTestExpr reply = case salvageCell reply of
    Just code -> deQuote (firstNonEmptyLine code)
    Nothing -> case inlineCode reply of
        Just span_ -> T.strip span_
        Nothing -> deQuote (firstNonEmptyLine reply)
  where
    deQuote = T.strip . T.dropAround (== '`') . T.strip

firstNonEmptyLine :: Text -> Text
firstNonEmptyLine = headOr "" . filter (not . T.null . T.strip) . T.lines
  where
    headOr d [] = d
    headOr _ (x : _) = x

inlineCode :: Text -> Maybe Text
inlineCode t = case T.breakOn "`" t of
    (_, rest)
        | not (T.null rest)
        , (code, close) <- T.breakOn "`" (T.drop 1 rest)
        , not (T.null close)
        , not (T.null (T.strip code)) ->
            Just code
    _ -> Nothing

tracedDispatch ::
    IORef Bool ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
tracedDispatch wroteRef dsp call = do
    progress ("\8594 " <> tcName call)
    out <- dsp call
    progress ("  " <> clip 100 (firstNonEmptyLine (renderOutcome out)))
    when (isOwningTool (tcName call)) (writeIORef wroteRef True)
    pure out

progress :: Text -> IO ()
progress = TIO.putStrLn . ("  " <>)

-- | Truncate to @n@ characters with an ellipsis, for one-line progress digests.
clip :: Int -> Text -> Text
clip n t
    | T.length t <= n = t
    | otherwise = T.take n t <> "\8230"

tshow :: (Show a) => a -> Text
tshow = T.pack . show
