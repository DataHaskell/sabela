{-# LANGUAGE OverloadedStrings #-}

{- | Top-level helpers for 'Siza.Agent.Loop': the episode's budget constants, the
discovery-budget nudge, the per-step rejection-sampling grounding, and the small
tool-call utilities. Split out of the loop module so the state machine there stays
readable; none of these close over episode state.
-}
module Siza.Agent.Loop.Support (
    nudgeK,
    nudgeFloor,
    maxChatRetries,
    maxStuckVerifies,
    stuckFinal,
    callActs,
    sampleK,
    writeSource,
    replaceCall,
    groundingMsgs,
    qualifiedBaseNames,
    nubShort,
    updateNudge,
    factsBlock,
    forceActMsgWith,
    streakHints,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isLower)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Owned (OwnedCell (..))
import Siza.Agent.Streak (bumpStreak, streakContrast)
import Siza.Agent.Tools (renderOutcome)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

{- | R5.6: read-only calls tolerated AFTER a call-ready fact (name + complete
signature) is held before the loop nudges the model to act — keyed on the
ledger state, so it fires with budget left and never on a fact-free ledger.
-}
nudgeK :: Int
nudgeK = 2

-- | The nudge must leave at least this many turns of budget when it fires.
nudgeFloor :: Int
nudgeFloor = 3

-- | N10: how many times a chat error is retried before the turn gives up.
maxChatRetries :: Int
maxChatRetries = 2

{- | Consecutive "declared done, check fails, nothing changed" turns tolerated
before the loop gives up. Any acting turn resets the counter, so a model still
trying is bounded by the repair budget and deadline, not by this.
-}
maxStuckVerifies :: Int
maxStuckVerifies = 3

{- | The give-up summary when 'maxStuckVerifies' trips: honest about the cause and
library/task-agnostic — the covering check is wrong, or it tests an effect a pure
expression cannot observe.
-}
stuckFinal :: Text
stuckFinal =
    "Gave up: the deliverable's check kept failing and the last few turns changed \
    \nothing. The check may be testing the wrong value, or an effect that a pure \
    \expression cannot observe (such as an IO action's result)."

-- | Whether a tool call acts on the notebook (so it resets the discovery budget).
callActs :: ToolCall -> Bool
callActs = maybe False actsOnNotebook . parseToolName . tcName

-- | Per-step rejection-sampling fan-out K, from @SIZA_SAMPLE_K@ (default 1 = off).
sampleK :: IO Int
sampleK = maybe 1 (max 1) . (>>= readMaybe) <$> lookupEnv "SIZA_SAMPLE_K"

-- | The cell source a write call carries (@source@ or @new_source@), if non-empty.
writeSource :: ToolCall -> Maybe Text
writeSource tc = case tcArgs tc of
    Object o -> case KM.lookup "source" o of
        Just (String s) | not (T.null (T.strip s)) -> Just s
        _ -> case KM.lookup "new_source" o of
            Just (String s) | not (T.null (T.strip s)) -> Just s
            _ -> Nothing
    _ -> Nothing

-- | A @replace_cell_source@ call for a cell id and candidate source.
replaceCall :: Int -> Text -> ToolCall
replaceCall cid src =
    ToolCall "replace_cell_source" (object ["cell_id" .= cid, "new_source" .= src])

{- | Ground the re-ask: look up the qualified library calls the failed cell used
(@D.col@ → @col@) via @find_function@ and hand back their REAL signatures, so the
re-sample stops guessing types. Empty when the cell used no qualified calls.
-}
groundingMsgs ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> Text -> IO [Value]
groundingMsgs disp src = do
    let names = take 5 (nubShort (qualifiedBaseNames src))
    parts <-
        mapM
            ( \n ->
                (,) n . renderOutcome
                    <$> disp (ToolCall "find_function" (object ["query" .= n]))
            )
            names
    let body =
            T.intercalate
                "\n"
                ["`" <> n <> "`:\n" <> r | (n, r) <- parts, not (T.null (T.strip r))]
    pure
        [ object
            [ "role" .= ("user" :: Text)
            , "content"
                .= ( "Real API from the live index for the functions you used. Use these EXACT \
                     \names, types, and modules; do not guess types or wrap pure functions in `<-`:\n"
                        <> body
                   )
            ]
        | not (T.null (T.strip body))
        ]

{- | Base names of qualified identifiers in a snippet (@D.col@ → @col@), lowercase
headed (a value/function, not a constructor/type), for a targeted API lookup.
-}
qualifiedBaseNames :: Text -> [Text]
qualifiedBaseNames src =
    [ base
    | tok <-
        T.split (\c -> not (isAlphaNum c || c == '.' || c == '_' || c == '\'')) src
    , T.any (== '.') tok
    , let base = T.takeWhileEnd (/= '.') tok
    , not (T.null base)
    , maybe False (isLower . fst) (T.uncons base)
    ]

-- | @nub@ preserving first-seen order, kept small (no Ord constraint churn).
nubShort :: [Text] -> [Text]
nubShort = go []
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs

{- | Consecutive read-only calls while a call-ready fact is held: acting (or a
fact-free ledger) resets the counter. Crossing @k@ ('escalationK', budget-
proportional) with 'nudgeFloor' turns left emits the ledger-closing @mkNudge@.
-}
updateNudge ::
    IO Value -> IORef Int -> Int -> Bool -> Int -> [ToolCall] -> IO [Value]
updateNudge mkNudge ref k factReady remaining calls
    | any callActs calls || not factReady = writeIORef ref 0 >> pure []
    | otherwise = do
        c0 <- readIORef ref
        let c = c0 + length calls
        if c >= k && remaining >= nudgeFloor
            then do
                writeIORef ref 0
                nudge <- mkNudge
                pure [nudge]
            else writeIORef ref c >> pure []

{- | The act-or-blocker nudge (R5.6/R5.7): carries the held facts it asks the
caller to act on plus the remaining budget, and never asks for more searching.
-}
forceActMsgWith :: [Text] -> Text -> Value
forceActMsgWith facts remaining =
    object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( "You have made several discovery/read calls in a row without \
                 \writing to the notebook. Searching further cannot help — act now: \
                 \add or edit a cell (insert_cell / replace_cell_source), or state \
                 \the blocker plainly. "
                    <> remaining
                    <> factsBlock facts
               )
        ]

{- | The held-facts paragraph the act nudge and the wrap-up share. Blank-
separated so it rides as one dedupable block ('Siza.Agent.EmitLedger'):
repeats back-reference, growth diffs.
-}
factsBlock :: [Text] -> Text
factsBlock facts
    | null facts = ""
    | otherwise =
        "\n\nFacts already established:\n"
            <> T.unlines (map ("- " <>) facts)

{- | Bump each red owned cell's streak this turn and return the contrasts due:
a diagnostic persisting 'Siza.Agent.Streak.streakThreshold' turns earns its
wrong-vs-real message mid-loop, once per streak.
-}
streakHints ::
    IORef (Map CellId (Text, Int)) -> Map CellId OwnedCell -> IO [Text]
streakHints ref owned = do
    m0 <- readIORef ref
    let reds =
            [ (c, ocDiagnostic oc)
            | (c, oc) <- Map.toList owned
            , not (ocHealthy oc)
            ]
        (m', hints) = foldl step (m0, []) reds
        step (m, hs) (c, d) =
            let (m2, n) = bumpStreak m c d
             in (m2, hs ++ maybeToList (streakContrast n d))
    writeIORef ref m'
    pure hints
