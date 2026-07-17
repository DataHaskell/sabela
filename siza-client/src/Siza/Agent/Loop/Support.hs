{-# LANGUAGE OverloadedStrings #-}

{- | Top-level helpers for 'Siza.Agent.Loop': the episode's budget constants, the
discovery-budget nudge, the per-step rejection-sampling grounding, and the small
tool-call utilities. Split out of the loop module so the state machine there stays
readable; none of these close over episode state.
-}
module Siza.Agent.Loop.Support (
    readDiscoveryBudget,
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
    updateBudget,
    forceActMsg,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isLower)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Tools (renderOutcome)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

{- | N4: consecutive read-only/discovery calls tolerated before the loop nudges
the model to act — catches the "burned the budget searching" pathology
without bothering a normal multi-read sync.
-}
readDiscoveryBudget :: Int
readDiscoveryBudget = 8

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

{- | Track consecutive discovery calls: an acting call resets the counter, else it
grows by the call count. Crossing 'readDiscoveryBudget' resets it and returns a
one-shot forcing message to inject. Library- and task-agnostic.
-}
updateBudget :: IORef Int -> [ToolCall] -> IO [Value]
updateBudget ref calls
    | any callActs calls = writeIORef ref 0 >> pure []
    | otherwise = do
        c0 <- readIORef ref
        let c = c0 + length calls
        if c >= readDiscoveryBudget
            then writeIORef ref 0 >> pure [forceActMsg]
            else writeIORef ref c >> pure []

-- | The just-in-time, library-agnostic nudge injected when discovery runs long.
forceActMsg :: Value
forceActMsg =
    object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( "You have made several discovery/read calls in a row without writing to the \
                 \notebook. If you have enough to proceed, act now — add or edit a cell \
                 \(insert_cell / replace_cell_source). If a value, function, or package you \
                 \need does not exist, say so rather than searching further." ::
                    Text
               )
        ]
