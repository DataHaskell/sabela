{-# LANGUAGE OverloadedStrings #-}

{- | The scripted episode the verdict-evidence properties are driven through,
and the generators they draw their writes and verdicts from. Shared so the
evidence and the stop-class properties run the same loop.
-}
module Test.VerdictEpisode (
    episode,
    deadKernelEpisode,
    withExemplarStore,
    anySource,
    anyVerdict,
    blankReply,
    noVerdict,
    nonPassing,
    passing,
    couldNotRunText,
    verdictClasses,
    precedingToolMsg,
    acceptedWrite,
    ownedFor,
) where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (setEnv, unsetEnv)
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.AI.Verdict (VerdictClass (..), parseVerdict)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..), noVerdictNote, noVerdicts)
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )
import Siza.Agent.Owned (OwnedCell (..))
import Test.DiscoverFixtures (textField)
import Test.TruthGen (
    genCheckOver,
    genIdent,
    genPreambleSource,
    genSubstantiveSource,
 )

anySource :: Gen Text
anySource = oneof [genPreambleSource, genSubstantiveSource]

-- | A passing verdict always carries the check that produced it.
passing :: Gen (CheckResult, Maybe Text)
passing = do
    n <- genIdent
    check <- genCheckOver [n]
    pure (CheckPassed, Just check)

nonPassing :: Gen (CheckResult, Maybe Text)
nonPassing =
    (,)
        <$> elements [CheckFailed, CheckUncheckable, CheckNotApplicable]
        <*> pure (Just "the check did not run")

anyVerdict :: Gen (CheckResult, Maybe Text)
anyVerdict = oneof [passing, nonPassing]

-- | A check that reached no verdict, carrying the reason it reached none.
noVerdict :: Gen (CheckResult, Maybe Text)
noVerdict =
    (,)
        <$> elements [CheckUncheckable, CheckNotApplicable]
        <*> (Just . noVerdictNote <$> elements noVerdicts)

blankReply :: Gen Text
blankReply = elements ["", "   ", "\n\t", " \n "]

couldNotRunText :: AgentRun -> [Text]
couldNotRunText run =
    [ body
    | m <- arTranscript run
    , let body = textField "content" m
    , parseVerdict body == Just VerdictCouldNotRun
    ]

verdictClasses :: AgentRun -> [VerdictClass]
verdictClasses = mapMaybe (parseVerdict . textField "content") . arTranscript

-- | The tool message immediately before the first verdict-bearing message.
precedingToolMsg :: AgentRun -> Maybe Value
precedingToolMsg run = case break isVerdict (arTranscript run) of
    (before, _ : _) -> lastToolMsg before
    _ -> Nothing
  where
    isVerdict m = isJust (parseVerdict (textField "content" m))
    lastToolMsg ms = case [m | m <- ms, textField "role" m == "tool"] of
        [] -> Nothing
        xs -> Just (last xs)

acceptedWrite :: Maybe Value -> Bool
acceptedWrite Nothing = False
acceptedWrite (Just m) =
    textField "tool_name" m
        == "insert_cell"
        && not ("notCommitted" `T.isInfixOf` textField "content" m)

ownedFor :: Text -> Map.Map Int OwnedCell
ownedFor src = Map.singleton 0 (OwnedCell True True "" src False True Nothing)

{- | A scripted episode: one turn per write, then a read-only turn (so the
done probe has somewhere to fire), then the terminal reply.
-}
episode :: [(Text, Bool)] -> Text -> (CheckResult, Maybe Text) -> IO AgentRun
episode writes finalReply verdict = do
    counter <- newIORef (0 :: Int)
    chat <- scriptedChat (map (writeCall . fst) writes ++ [[readOnly]]) finalReply
    let driver =
            Driver
                { drvChat = chat
                , drvDispatch = dispatchScript counter (accepted writes)
                , drvNow = pure 0
                , drvVerify = const (pure verdict)
                }
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        (EpisodeBudget{ebMaxRepairs = 2, ebDeadlineSecs = 600})
        driver
        "add the binding the request asks for"
        6

{- | An episode whose every tool call fails in transport, over enough turns for
the loop to tell the kernel apart from the call.
-}
deadKernelEpisode :: Text -> Text -> IO AgentRun
deadKernelEpisode src why = do
    chat <- scriptedChat (replicate 4 (writeCall src)) "done"
    let driver =
            Driver
                { drvChat = chat
                , drvDispatch = const (pure (Left why))
                , drvNow = pure 0
                , drvVerify = const (pure (CheckNotApplicable, Nothing))
                }
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        (EpisodeBudget{ebMaxRepairs = 9, ebDeadlineSecs = 600})
        driver
        "add the binding the request asks for"
        8

accepted :: [(Text, Bool)] -> Text -> Bool
accepted writes src = or [ok | (s, ok) <- writes, s == src]

writeCall :: Text -> [ToolCall]
writeCall src = [ToolCall "insert_cell" (object ["source" .= src])]

readOnly :: ToolCall
readOnly = ToolCall "list_bindings" (object [])

scriptedChat :: [[ToolCall]] -> Text -> IO ([Value] -> IO (Either Text Turn))
scriptedChat batches finalReply = do
    step <- newIORef (0 :: Int)
    pure $ \_ -> do
        i <- readIORef step
        modifyIORef' step (+ 1)
        pure . Right $
            if i < length batches
                then Turn (assistant "acting") "" (batches !! i)
                else Turn (assistant finalReply) finalReply []
  where
    assistant :: Text -> Value
    assistant t = object ["role" .= ("assistant" :: Text), "content" .= t]

dispatchScript ::
    IORef Int -> (Text -> Bool) -> ToolCall -> IO (Either Text ToolOutcome)
dispatchScript counter ok tc = case tcName tc of
    "insert_cell" -> do
        n <- readIORef counter
        modifyIORef' counter (+ 1)
        pure $
            if ok (textField "source" (tcArgs tc))
                then
                    Right
                        ( ToolOk
                            ( object
                                [ "cellId" .= n
                                , "execution" .= object ["ok" .= True]
                                ]
                            )
                        )
                else
                    Right
                        ( ToolErr
                            ( object
                                [ "notCommitted" .= ("compile-gate" :: Text)
                                , "source" .= textField "source" (tcArgs tc)
                                ]
                            )
                        )
    "list_cells" -> pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
    _ -> pure (Right (ToolOk (object ["result" .= ("" :: Text)])))

-- | Runs the episode with an exemplar store, and reports whether it grew.
withExemplarStore :: IO AgentRun -> IO Bool
withExemplarStore act = do
    tmp <- getTemporaryDirectory
    let path = tmp <> "/siza-truth-exemplars.jsonl"
    bracket_ (setEnv "SIZA_EXEMPLAR_STORE" path) (unsetEnv "SIZA_EXEMPLAR_STORE") $ do
        exists0 <- doesFileExist path
        when exists0 (removeFile path)
        _ <- act
        exists <- doesFileExist path
        if exists then removeFile path >> pure True else pure False
