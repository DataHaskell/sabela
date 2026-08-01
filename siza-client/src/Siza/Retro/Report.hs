{-# LANGUAGE OverloadedStrings #-}

-- | The JSON @siza retro --transcript@ prints.
module Siza.Retro.Report (transcriptMetricsValue) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AK
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Siza.Retro.Distribution (Spread (..), payloadSpread)
import Siza.Retro.Metrics (
    ElisionCounts (..),
    TranscriptMetrics (..),
    WriteCounts (..),
 )

transcriptMetricsValue :: TranscriptMetrics -> Value
transcriptMetricsValue m =
    object
        [ "mode" .= ("transcript" :: Text)
        , "sections" .= tmSections m
        , "turns" .= tmTurns m
        , "toolCalls" .= tmToolCalls m
        , "perTool" .= counts (tmPerTool m)
        , "writes" .= writes (tmWrites m)
        , "repeatedRejections" .= tmRepeatedRejections m
        , "unchangedDiagnostics" .= tmUnchangedDiagnostics m
        , "unknownDiagnostics" .= tmUnknownDiagnostics m
        , "duplicateResults" .= tmDuplicateResults m
        , "elided" .= elided (tmElision m)
        , "doneSignals" .= tmDoneSignals m
        , "reroutedResults" .= tmReroutedResults m
        , "doneSignalsAfterRejection" .= tmDoneAfterRejection m
        , "falseSuccessClaims" .= tmFalseSuccess m
        , "payloadChars"
            .= object
                [ "total" .= tmPayloadChars m
                , "perTool" .= counts (tmPayloadPerTool m)
                , "spreadPerTool" .= spreads (payloadSpread (tmPayloadRows m))
                ]
        , "thinkingChars" .= tmThinkingChars m
        , "promptChars" .= tmPromptChars m
        , "turnsToFirstCommit" .= tmTurnsToFirstCommit m
        , "turnsToLastCommit" .= tmTurnsToLastCommit m
        ]
  where
    counts t = object [AK.fromText k .= n | (k, n) <- M.toList t]
    spreads t = object [AK.fromText k .= spread s | (k, s) <- M.toList t]
    spread s =
        object
            [ "calls" .= spCount s
            , "median" .= spMedian s
            , "p90" .= spP90 s
            , "max" .= spMax s
            , "total" .= spTotal s
            ]
    writes w =
        object
            [ "attempted" .= wcAttempted w
            , "committed" .= wcCommitted w
            , "rejected" .= wcRejected w
            , "unresolved" .= wcUnresolved w
            ]
    elided e =
        object
            [ "results" .= ecResults e
            , "fullChars" .= ecFullChars e
            , "stubChars" .= ecStubChars e
            , "hiddenChars" .= (ecFullChars e - ecStubChars e)
            ]
