{-# LANGUAGE OverloadedStrings #-}

{- | What an episode transcript proves about the harness that produced it.
Every number here is defendable from the transcript alone; where elision hid
what a number would need, the reading is reported as unknown instead.
-}
module Siza.Retro.Metrics (
    ElisionCounts (..),
    TranscriptMetrics (..),
    WriteCounts (..),
    metricsFromText,
    transcriptMetrics,
) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (
    ToolName (DeleteCell),
    actsOnNotebook,
    parseToolName,
 )
import Siza.Retro.Episode (EpisodeCall (..), Section (..), parseEpisode)
import Siza.Retro.Result (
    Elision (..),
    WriteOutcome (..),
    elisionOf,
    isDoneSignal,
    isDuplicate,
    jsonString,
    normaliseSource,
    visibleText,
    writeOutcome,
 )

data WriteCounts = WriteCounts
    { wcAttempted :: Int
    , wcCommitted :: Int
    , wcRejected :: Int
    , wcUnresolved :: Int
    }
    deriving (Eq, Show)

data ElisionCounts = ElisionCounts
    { ecResults :: Int
    , ecFullChars :: Int
    , ecStubChars :: Int
    }
    deriving (Eq, Show)

data TranscriptMetrics = TranscriptMetrics
    { tmSections :: Int
    , tmTurns :: Int
    , tmToolCalls :: Int
    , tmPerTool :: M.Map Text Int
    , tmWrites :: WriteCounts
    , tmRepeatedRejections :: Int
    , tmUnchangedDiagnostics :: Int
    , tmUnknownDiagnostics :: Int
    , tmDuplicateResults :: Int
    , tmElision :: ElisionCounts
    , tmDoneSignals :: [Int]
    , tmDoneAfterRejection :: [Int]
    , tmReroutedResults :: [Int]
    , tmFalseSuccess :: [Int]
    , tmPayloadChars :: Int
    , tmPayloadPerTool :: M.Map Text Int
    , tmPayloadRows :: [(Text, Int)]
    , tmThinkingChars :: Int
    , tmPromptChars :: Int
    , tmTurnsToFirstCommit :: Maybe Int
    , tmTurnsToLastCommit :: Maybe Int
    }
    deriving (Eq, Show)

metricsFromText :: Text -> TranscriptMetrics
metricsFromText = transcriptMetrics . parseEpisode

-- | One write submission, with whatever its answering result still shows.
data Submission = Submission
    { subSource :: Text
    , subOutcome :: WriteOutcome
    , subDiagnostic :: Maybe Text
    }

transcriptMetrics :: [Section] -> TranscriptMetrics
transcriptMetrics secs =
    TranscriptMetrics
        { tmSections = length secs
        , tmTurns = length [() | s <- secs, secRole s == "assistant"]
        , tmToolCalls = length calls
        , tmPerTool = tally (map ecTool calls)
        , tmWrites = writeCounts submissions
        , tmRepeatedRejections = countPairs repeated
        , tmUnchangedDiagnostics = countPairs unchanged
        , tmUnknownDiagnostics =
            length
                [ ()
                | s <- submissions
                , subOutcome s == Rejected
                , isNothing (subDiagnostic s)
                ]
        , tmDuplicateResults = length [() | r <- results, isDuplicate (secContent r)]
        , tmElision = elisionCounts results
        , tmDoneSignals = [secIndex r | r <- results, isDoneSignal (secContent r)]
        , tmDoneAfterRejection = doneAfterRejection secs
        , tmReroutedResults = reroutedResults secs
        , tmFalseSuccess = falseSuccess secs
        , tmPayloadChars = sum (map (T.length . secContent) results)
        , tmPayloadPerTool = M.fromListWith (+) payloadRows
        , tmPayloadRows = payloadRows
        , tmThinkingChars = sum (map (T.length . secThinking) secs)
        , tmPromptChars =
            sum [T.length (secContent s) | s <- secs, secRole s `elem` prompts]
        , tmTurnsToFirstCommit = headMay commitTurns
        , tmTurnsToLastCommit = lastMay commitTurns
        }
  where
    calls = concatMap secCalls secs
    results = [s | s <- secs, secRole s == "tool"]
    payloadRows = [(toolOf r, T.length (secContent r)) | r <- results]
    prompts = ["system", "user"] :: [Text]
    submissions = writeSubmissions secs
    commitTurns = committedTurns secs
    countPairs p = length [() | (a, b) <- adjacent submissions, p a b]
    repeated a b = bothRejected a b && sameDiag a b && sameSource a b
    unchanged a b = bothRejected a b && sameDiag a b && not (sameSource a b)
    bothRejected a b = subOutcome a == Rejected && subOutcome b == Rejected
    sameDiag a b = isJust (subDiagnostic a) && subDiagnostic a == subDiagnostic b
    sameSource a b = normaliseSource (subSource a) == normaliseSource (subSource b)

adjacent :: [a] -> [(a, a)]
adjacent xs = zip xs (drop 1 xs)

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay [] = Nothing

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

tally :: (Ord a) => [a] -> M.Map a Int
tally xs = M.fromListWith (+) [(x, 1) | x <- xs]

toolOf :: Section -> Text
toolOf s = fromMaybe (secRole s) (secTool s)

isWriteTool :: Text -> Bool
isWriteTool = maybe False actsOnNotebook . parseToolName

writeCounts :: [Submission] -> WriteCounts
writeCounts subs =
    WriteCounts
        { wcAttempted = length subs
        , wcCommitted = length [() | s <- subs, isCommit (subOutcome s)]
        , wcRejected = length [() | s <- subs, subOutcome s == Rejected]
        , wcUnresolved = length [() | s <- subs, subOutcome s == Unresolved]
        }
  where
    isCommit (Committed _) = True
    isCommit _ = False

elisionCounts :: [Section] -> ElisionCounts
elisionCounts results =
    ElisionCounts
        { ecResults = length elisions
        , ecFullChars = sum [elFullChars e | (_, e) <- elisions]
        , ecStubChars = sum [T.length (secContent r) | (r, _) <- elisions]
        }
  where
    elisions = [(r, e) | r <- results, Just e <- [elisionOf (secContent r)]]

{- | Pairs each call with the result that answered it: the next result of the
same tool, or, failing that, the oldest call still waiting — a write the
harness rerouted answers under a tool name the model never called. A done
signal answers nothing, and a call the episode never answers pairs with
nothing.
-}
pairCalls :: [Section] -> [(EpisodeCall, Maybe Section)]
pairCalls secs = [(c, M.lookup i matched) | (i, c) <- indexed]
  where
    indexed = zip [0 :: Int ..] (concatMap secCalls secs)
    matched = M.fromList (go 0 [] secs)
    go _ _ [] = []
    go n pending (s : ss)
        | secRole s == "assistant" =
            go (n + length (secCalls s)) (pending ++ zip [n ..] (secCalls s)) ss
        | secRole s == "tool"
        , not (isDoneSignal (secContent s))
        , Just (i, rest) <- claim (secTool s) pending =
            (i, s) : go n rest ss
        | otherwise = go n pending ss
    claim tool pending = case break (nameIs tool . snd) pending of
        (before, (i, _) : after) -> Just (i, before ++ after)
        _ -> case pending of
            ((i, _) : after) -> Just (i, after)
            [] -> Nothing
    nameIs tool c = Just (ecTool c) == tool

{- | Results that came back under a tool name the call did not use, which is
what a rerouted write looks like from the transcript.
-}
reroutedResults :: [Section] -> [Int]
reroutedResults secs =
    [ secIndex r
    | (c, Just r) <- pairCalls secs
    , secTool r /= Just (ecTool c)
    ]

writeSubmissions :: [Section] -> [Submission]
writeSubmissions secs =
    [submissionOf c r | (c, r) <- pairCalls secs, isWriteTool (ecTool c)]

submissionOf :: EpisodeCall -> Maybe Section -> Submission
submissionOf c r =
    Submission
        { subSource = fromMaybe (ecArgs c) (jsonString "source" (ecArgs c))
        , subOutcome = maybe Unresolved (writeOutcome . secContent) r
        , subDiagnostic = jsonString "diagnostic" . visibleText . secContent =<< r
        }

{- | The turn number of every committed write, in assistant turns elapsed when
its result arrived.
-}
committedTurns :: [Section] -> [Int]
committedTurns = go 0
  where
    go _ [] = []
    go turns (s : ss)
        | secRole s == "assistant" = go (turns + 1) ss
        | isCommittedWrite s = turns : go turns ss
        | otherwise = go turns ss

isCommittedWrite :: Section -> Bool
isCommittedWrite s = case (secRole s, secTool s) of
    ("tool", Just t) | isWriteTool t -> committed (writeOutcome (secContent s))
    _ -> False
  where
    committed (Committed _) = True
    committed _ = False

{- | Done signals whose most recent write result was a refusal: the harness
declared the task done in the same breath as it refused the model's last
write.
-}
doneAfterRejection :: [Section] -> [Int]
doneAfterRejection = reverse . snd . foldl' step (Nothing, [])
  where
    step acc@(lastWrite, claims) s
        | secRole s /= "tool" = acc
        | isDoneSignal (secContent s) = (lastWrite, claimed)
        | Just t <- secTool s
        , isWriteTool t =
            (Just (writeOutcome (secContent s)), claims)
        | otherwise = acc
      where
        claimed
            | lastWrite == Just Rejected = secIndex s : claims
            | otherwise = claims

{- | Done signals that no committed cell stands behind at the moment they fire.
A cell counts only while the transcript still shows it: a delete retires it.
-}
falseSuccess :: [Section] -> [Int]
falseSuccess = reverse . snd . foldl' step (S.empty, [])
  where
    step acc@(live, claims) s
        | secRole s /= "tool" = acc
        | isDoneSignal (secContent s) =
            (live, if S.null live then secIndex s : claims else claims)
        | Just t <- secTool s
        , isWriteTool t
        , Committed (Just cid) <- writeOutcome (secContent s) =
            (retire t cid live, claims)
        | otherwise = acc
    retire t cid
        | parseToolName t == Just DeleteCell = S.delete cid
        | otherwise = S.insert cid
