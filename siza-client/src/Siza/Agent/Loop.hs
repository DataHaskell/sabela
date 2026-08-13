{-# LANGUAGE OverloadedStrings #-}

{- |
Technique: episode facade plus transcript seeding [Episode].
Guarantee: the model reply is repaired (recoverTurn wraps drvChat) before the loop sees it.
Entry: 'runEpisodeSeeded'. Next: Siza.Agent.Loop.Step (the actual machine).
-}
module Siza.Agent.Loop (
    AgentRun (..),
    StopDecision (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    SampleVerify (..),
    SampleResult (..),
    defaultBudget,
    runEpisodeWith,
    runEpisodeWith',
    runEpisodeTraced,
    runEpisodeSeeded,
    sampleVerifyOne,
    ownedCellOutcome,
    stopDecision,
    discoverModules,
    systemPrompt,
    mcpInstructions,
    sampleK,
    writeSource,
    qualifiedBaseNames,
    episodeStack,
) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Siza.Agent.Discover.HistoryGuard (seedSearchLedger)
import Siza.Agent.EmitLedger (EmitLedger, dedupInjected, newEmitLedger)
import Siza.Agent.Exemplars (retrieveForPrompt)
import Siza.Agent.GrammarCards (
    GrammarMode (..),
    discoverModules,
    proactiveDiscover,
 )
import Siza.Agent.Loop.Episode (newEpisode)
import Siza.Agent.Loop.Prompt (mcpInstructions, systemPrompt)
import Siza.Agent.Loop.Step (runTurns)
import Siza.Agent.Loop.Support (qualifiedBaseNames, sampleK, writeSource)
import Siza.Agent.Loop.Types (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    defaultBudget,
 )
import Siza.Agent.Owned (StopDecision (..), ownedCellOutcome, stopDecision)
import Siza.Agent.Sample (SampleResult (..), SampleVerify (..), sampleVerifyOne)
import Siza.Agent.Scaffold (runScaffoldStage)
import Siza.Agent.Stack (
    Dispatch,
    StackSession (..),
    newStackSession,
    stackDispatch,
 )
import Siza.Agent.ToolRoute (recoverTurn)
import Siza.Agent.Tools (offeredArgKeys)

runEpisodeWith :: EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith = runEpisodeWith' GrammarOn

runEpisodeWith' ::
    GrammarMode -> EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith' = runEpisodeTraced (const (pure ()))

runEpisodeTraced ::
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
runEpisodeTraced = runEpisodeSeeded []

runEpisodeSeeded ::
    [Value] ->
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
runEpisodeSeeded seed emit mode budget driver0 prompt maxTurns = do
    sess <- newStackSession mode prompt
    emits <- newEmitLedger
    let driver =
            driver0
                { drvChat =
                    fmap (fmap (recoverTurn offeredArgKeys)) . drvChat driver0
                , drvDispatch = episodeStack sess (drvDispatch driver0)
                }
    episodeCore sess emits seed emit budget driver prompt maxTurns

{- | The stack the chat loop wraps every tool call in. Named so the parity
spec can drive the same layers the MCP server drives.
-}
episodeStack :: StackSession -> Dispatch -> Dispatch
episodeStack = stackDispatch

episodeCore ::
    StackSession ->
    IORef EmitLedger ->
    [Value] ->
    (Text -> IO ()) ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
episodeCore sess emits seed emit budget driver prompt maxTurns = do
    ep <- newEpisode sess emits emit budget driver prompt maxTurns
    msgs0 <- if null seed then seededTranscript else pure (seed ++ [userMsg])
    start <- drvNow driver
    runTurns ep start 0 0 0 Map.empty msgs0
  where
    seededTranscript = do
        exemplars <- retrieveForPrompt prompt
        pre <- runScaffoldStage (drvDispatch driver) prompt
        seedSearchLedger (drvDispatch driver) (ssLedger sess)
        proactive <- proactiveDiscover (ssGrammar sess) (drvDispatch driver)
        injected0 <- dedupInjected emits 0 (exemplars ++ pre ++ proactive)
        pure (initial ++ injected0)
    initial =
        [ object ["role" .= ("system" :: Text), "content" .= systemPrompt]
        , userMsg
        ]
    userMsg = object ["role" .= ("user" :: Text), "content" .= prompt]
