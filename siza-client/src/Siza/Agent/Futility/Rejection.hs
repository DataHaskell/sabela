{-# LANGUAGE OverloadedStrings #-}

{- |
Technique: dispatch-rejection classification [Gating/Repair].
Guarantee: diagnostic equality and source deltas report measured facts only.
Entry: 'normaliseDiagnostic'. Parent: 'Siza.Agent.Futility'.
-}
module Siza.Agent.Futility.Rejection (
    CachedRejection (..),
    RejectionRun (..),
    cacheableRejection,
    cachedFact,
    completeWrite,
    markUnchanged,
    normaliseDiagnostic,
    settledNoMutation,
    unchangedState,
    worldChanging,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.SelfHeal (sourceDelta)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.VerifyMemo (Seal)

data CachedRejection = CachedRejection
    { cachedOutcome :: !ToolOutcome
    , cachedSeal :: !Seal
    }

data RejectionRun = RejectionRun
    { rrCount :: !Int
    , rrSources :: !(Set Text)
    , rrLast :: !Text
    }

cacheableRejection :: Either Text ToolOutcome -> Bool
cacheableRejection (Right (ToolErr (Object o))) =
    textField "notCommitted" o == "compile-gate"
        && textField "verdict" o == "diagnostic"
        && textField "attributedTo" o == "your candidate"
        && textField "stage" o == "candidate_setup"
        && not (T.null (T.strip (textField "diagnostic" o)))
cacheableRejection _ = False

settledNoMutation :: Either Text ToolOutcome -> Bool
settledNoMutation (Right (ToolErr (Object o))) =
    not (T.null (T.strip (textField "notCommitted" o)))
settledNoMutation _ = False

completeWrite :: ToolCall -> Bool
completeWrite (ToolCall "insert_cell" (Object o)) = stringAt "source" o
completeWrite (ToolCall "replace_cell_source" (Object o)) =
    numberAt "cell_id" o && stringAt "new_source" o
completeWrite _ = False

worldChanging :: ToolCall -> Bool
worldChanging (ToolCall name _) =
    maybe False actsOnNotebook (parseToolName name)
        || name
            `elem` ["kernel_restart", "run_pending", "set_run_mode", "eval_live", "replace_cells"]

textField :: Text -> KM.KeyMap Value -> Text
textField k o = case KM.lookup (K.fromText k) o of
    Just (String t) -> t
    _ -> ""

stringAt, numberAt :: Text -> KM.KeyMap Value -> Bool
stringAt k o = case KM.lookup (K.fromText k) o of Just (String _) -> True; _ -> False
numberAt k o = case KM.lookup (K.fromText k) o of Just (Number _) -> True; _ -> False

cachedFact :: Int -> Either Text ToolOutcome -> Either Text ToolOutcome
cachedFact epoch (Right (ToolErr (Object o))) =
    Right . ToolErr . object $
        [ "notCommitted" .= ("compile-gate" :: Text)
        , "verdict" .= ("diagnostic" :: Text)
        , "state" .= ("unchanged" :: Text)
        , "basis"
            .= object
                [ "priorOutcome" .= ("deterministic compile-gate rejection" :: Text)
                , "sameArguments" .= True
                , "worldSealMatched" .= True
                , "diagnosticSummary" .= summary
                , "diagnosticChars" .= T.length diagnostic
                , "summaryTruncated" .= (T.length summaryText > summaryCap)
                ]
        , "cached"
            .= object
                [ "worldEpoch" .= epoch
                , "dispatchSkipped" .= True
                ]
        ]
  where
    diagnostic = textField "diagnostic" o
    summaryText = T.unwords (T.words diagnostic)
    summary = T.take summaryCap summaryText
cachedFact _ out = out

summaryCap :: Int
summaryCap = 320

normaliseDiagnostic :: Text -> Text
normaliseDiagnostic t = case T.breakOn marker t of
    (_, rest) | T.null rest -> t
    (pre, rest) ->
        let body = T.drop (T.length marker) rest
            (pos, after) = T.span positionChar body
         in pre <> marker <> (if T.null pos then "" else "L:C") <> normaliseDiagnostic after
  where
    positionChar c = isDigit c || c == ':' || c == '-'
    marker = "<interactive>:"

markUnchanged ::
    Text -> Either Text ToolOutcome -> RejectionRun -> Either Text ToolOutcome
markUnchanged src (Right (ToolErr (Object o))) prev =
    Right (ToolErr (Object (KM.insert "unchanged" detail marked)))
  where
    marked = KM.insert "state" (String unchangedState) o
    sources = Set.filter (not . T.null) (Set.insert src (rrSources prev))
    changed = src /= rrLast prev
    (removed, added) = sourceDelta (rrLast prev) src
    dropped = length removed + length added - length (shown removed) - length (shown added)
    detail =
        object
            ( [ "priorCalls" .= rrCount prev
              , "distinctSources" .= Set.size sources
              , "sourceChanged" .= changed
              , "note" .= unchangedNote (rrCount prev) (Set.size sources)
              ]
                <> [ "changedLines"
                        .= object
                            ( [ "removed" .= toJSON (shown removed)
                              , "added" .= toJSON (shown added)
                              ]
                                <> ["furtherLines" .= dropped | dropped > 0]
                            )
                   | changed
                   ]
            )
    shown = take deltaLines
markUnchanged _ out _ = out

deltaLines :: Int
deltaLines = 12

unchangedState :: Text
unchangedState = "unchanged"

unchangedNote :: Int -> Int -> Text
unchangedNote priorCalls distinct =
    "This diagnostic is identical, ignoring <interactive> line and column \
    \numbers, to the one "
        <> T.pack (show priorCalls)
        <> " earlier call(s) in this world epoch."
        <> if distinct <= 0
            then ""
            else
                " Counting this call, "
                    <> T.pack (show distinct)
                    <> " distinct submitted source(s) produced it."
