{-# LANGUAGE OverloadedStrings #-}

{- | Transcript lint (R8.4): content-shape rules over an episode's observable
messages — no raw exception records, no serialisation-in-string, no internal
package-hash names, no sentence more than twice per payload, exactly one
result per tool call, and no injected note asserting unverified state
(section 9.2, M16). Arm- and task-agnostic by construction.
-}
module Eval.TranscriptLint (
    LintIssue (..),
    lintMessages,
    lintLine,
    harnessChannels,
    singleShapedCells,
    stopIssues,
    verdictIssues,
    verifierChannels,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.Foldable (toList)
import Data.List (nub, sort, (\\))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Eval.ElisionLint (elisionProblems)
import Eval.VerdictLint (verdictProblems, verifierChannels)
import Eval.VerifierLeakLint (verifierLeakProblems)
import Siza.Agent.NoteLedger (assertedLive)

-- | One violated rule, with the offending content snippet.
data LintIssue = LintIssue
    { liRule :: Text
    , liDetail :: Text
    }
    deriving (Eq, Show)

{- | Harness-injected tool channels: results that are not answers to a model
call, so the one-result-per-call rule exempts them.
-}
harnessChannels :: [Text]
harnessChannels = ["discover", "health_gate", "verify", "salvage", "scaffold"]

lintMessages :: [Value] -> [LintIssue]
lintMessages msgs =
    concatMap contentIssues msgs
        <> cardinalityIssues msgs
        <> noteIssues msgs
        <> postSuccessDumpIssues msgs
        <> versionQualifiedIssues msgs
        <> verdictIssues msgs
        <> map (uncurry LintIssue) (verifierLeakProblems msgs)
        <> map (uncurry LintIssue) (elisionProblems msgs)

-- | Section 5.3 verdict totality, rule logic in "Eval.VerdictLint".
verdictIssues :: [Value] -> [LintIssue]
verdictIssues = map (uncurry LintIssue) . verdictProblems

-- | The compact lint verdict recorded in the episode-config header.
lintLine :: [LintIssue] -> Text
lintLine [] = "ok"
lintLine issues = "FAIL " <> T.intercalate "," (nub (map liRule issues))

{- | R8.4 stop-shape rule: a cap-class stop (the harness, not the model, ended
the episode) must carry a non-empty final line — the run-20260720-085948
symbolicRegression-off 43.7k max_turns transcript ended with @final:@ empty.
-}
stopIssues :: Text -> Text -> [LintIssue]
stopIssues stopped final =
    [ LintIssue "empty-final" ("stopped: " <> stopped <> " with an empty final")
    | stopped `elem` capStops
    , T.null (T.strip final)
    ]
  where
    capStops = ["max_turns", "repair_budget", "deadline"]

contentIssues :: Value -> [LintIssue]
contentIssues m =
    concatMap
        ($ strAt "content" m)
        [rawException, serialisationInString, packageHashNames, repeatedSentence]

rawException :: Text -> [LintIssue]
rawException c =
    [ LintIssue "raw-exception" (snippet c marker)
    | marker <- ["HttpExceptionRequest", "InvalidUrlException"]
    , marker `T.isInfixOf` c
    ]

serialisationInString :: Text -> [LintIssue]
serialisationInString c =
    [ LintIssue "serialisation-in-string" (snippet c marker)
    | marker <- ["Trailing garbage", "Error in $"]
    , marker `T.isInfixOf` c
    ]

packageHashNames :: Text -> [LintIssue]
packageHashNames c =
    [LintIssue "package-hash" w | w <- take 3 (filter hashToken (T.words c))]

-- | @pkg-1.2.3-\<abihash\>@: a version segment followed by a long alnum hash.
hashToken :: Text -> Bool
hashToken w = go (T.splitOn "-" w)
  where
    go (a : b : rest) = (isVersion a && isHash b) || go (b : rest)
    go _ = False
    isVersion s =
        not (T.null s)
            && T.count "." s >= 1
            && T.all (\ch -> isDigit ch || ch == '.') s
    isHash s =
        let h = T.takeWhile isAlphaNum s
         in T.length h >= 16 && T.any isDigit h && T.any isLetter h

repeatedSentence :: Text -> [LintIssue]
repeatedSentence c =
    [ LintIssue "repeated-sentence" (T.take 80 s <> " (x" <> tshow n <> ")")
    | (s, n) <- Map.toList counts
    , n > 2
    ]
  where
    counts =
        Map.fromListWith (+) [(s, 1 :: Int) | s <- sentences c, T.length s >= 25]

sentences :: Text -> [Text]
sentences c = concatMap (map T.strip . T.splitOn ". ") (T.lines c)

{- | R6.10 x R8.4: module-API content on a harness channel is illegal while
the most recent write result Succeeded — a red write re-opens the seam, and
a card before any write is the legal proactive one.
-}
postSuccessDumpIssues :: [Value] -> [LintIssue]
postSuccessDumpIssues = go False
  where
    go _ [] = []
    go lastOk (m : rest)
        | isWriteResult m = go (okResult (strAt "content" m)) rest
        | lastOk && harnessDump m =
            LintIssue "post-success-module-dump" (T.take 80 (strAt "content" m))
                : go lastOk rest
        | otherwise = go lastOk rest
    harnessDump m =
        strAt "role" m == "tool"
            && strAt "tool_name" m `elem` harnessChannels
            && moduleApiShaped (strAt "content" m)
    isWriteResult m =
        strAt "role" m == "tool"
            && strAt "tool_name" m `elem` ["insert_cell", "replace_cell_source"]

-- | Module-dump shape: the banner header, the card header, or 3+ signatures.
moduleApiShaped :: Text -> Bool
moduleApiShaped c =
    "Discovered API of" `T.isInfixOf` c
        || "Live API grammar" `T.isInfixOf` c
        || length [l | l <- T.lines c, " :: " `T.isInfixOf` l] >= 3

{- | Version-qualified FQNs (@aeson-2.3.1.0:Data.Aeson...@) are GHC
internals; a harness-composed note or channel result must never carry one.
-}
versionQualifiedIssues :: [Value] -> [LintIssue]
versionQualifiedIssues msgs =
    [ LintIssue "version-qualified-name" w
    | m <- msgs
    , strAt "role" m == "tool"
    , strAt "tool_name" m `elem` harnessChannels
    , w <- take 3 (filter versionQualified (T.words (strAt "content" m)))
    ]

-- | A token with a @pkg-<version>:@ prefix before a nonempty remainder.
versionQualified :: Text -> Bool
versionQualified w = case T.breakOn ":" w of
    (pre, post) -> not (T.null (T.drop 1 post)) && versionSuffixed pre
  where
    versionSuffixed pre = case T.splitOn "-" pre of
        parts@(_ : _ : _) ->
            let v = last parts
             in T.count "." v >= 1
                    && not (T.null v)
                    && T.all (\c -> isDigit c || c == '.') v
        _ -> False

{- | One result per call: each assistant message's tool calls are answered by
exactly one following tool result each; a tool result no call asked for is a
phantom (P10). The harness-channel exemption applies only to UNREQUESTED
results — a model call to a tool that shares a harness channel's name (the
merged @discover@) is still owed exactly one answer.
-}
cardinalityIssues :: [Value] -> [LintIssue]
cardinalityIssues = go . map classify
  where
    classify m = (strAt "role" m, callNames m, strAt "tool_name" m)
    go [] = []
    go ((role, calls, name) : rest)
        | role == "assistant" =
            let (results, rest') = span (\(r, _, _) -> r == "tool") rest
                names = [n | (_, _, n) <- results]
             in pairIssues calls names <> go rest'
        | role == "tool" && name `notElem` harnessChannels =
            LintIssue "phantom-result" name : go rest
        | otherwise = go rest

pairIssues :: [Text] -> [Text] -> [LintIssue]
pairIssues calls names =
    [LintIssue "unanswered-call" n | n <- sort calls \\ sort names]
        <> [ LintIssue "phantom-result" n
           | n <- sort names \\ sort calls
           , n `notElem` harnessChannels
           ]

callNames :: Value -> [Text]
callNames m = case lookupField "tool_calls" m of
    Just (Array a) ->
        [ n
        | Object c <- toList a
        , Just (Object f) <- [KM.lookup "function" c]
        , Just (String n) <- [KM.lookup "name" f]
        ]
    _ -> []

{- | Note ledger (section 9.2, M16): a user-role note after the task prompt may
assert a name live only when a preceding successful tool result evidences that
name. The first user message is the task's own ground truth and is exempt.
-}
noteIssues :: [Value] -> [LintIssue]
noteIssues = go False Set.empty
  where
    go _ _ [] = []
    go seenPrompt evidence (m : rest)
        | strAt "role" m == "user" && not seenPrompt = go True evidence rest
        | strAt "role" m == "user" =
            [ LintIssue "unverified-note" n
            | n <- assertedLive (strAt "content" m)
            , not (n `Set.member` evidence)
            ]
                <> go seenPrompt evidence rest
        | otherwise = go seenPrompt (addEvidence m evidence) rest

-- | Names evidenced by a successful tool result's content.
addEvidence :: Value -> Set Text -> Set Text
addEvidence m evidence
    | strAt "role" m == "tool" && okResult c = evidence <> identTokens c
    | otherwise = evidence
  where
    c = strAt "content" m

{- | A successful result: a clean write ack, or a found discover envelope
(its named hits are evidence a note may cite).
-}
okResult :: Text -> Bool
okResult c =
    any
        (`T.isInfixOf` c)
        ["\"ok\":true", "\"ok\": true", "\"state\":\"found\"", "\"state\": \"found\""]

identTokens :: Text -> Set Text
identTokens =
    Set.fromList
        . filter (not . T.null)
        . T.split (\c -> not (isAlphaNum c || c == '_' || c == '\''))

-- | Every cell entry in a list_cells result decodes against ONE shape (R9.1).
singleShapedCells :: Value -> Bool
singleShapedCells v = case cellObjects v of
    Nothing -> False
    Just [] -> True
    Just (c : cs) -> all ((== KM.keys c) . KM.keys) cs

cellObjects :: Value -> Maybe [KM.KeyMap Value]
cellObjects (Array a) = traverse asObject (toList a)
  where
    asObject (Object o) = Just o
    asObject _ = Nothing
cellObjects (Object o) = KM.lookup "cells" o >>= cellObjects
cellObjects _ = Nothing

strAt :: Text -> Value -> Text
strAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
strAt _ _ = ""

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KM.lookup (K.fromText k) o
lookupField _ _ = Nothing

snippet :: Text -> Text -> Text
snippet c marker = T.take 80 (snd (T.breakOn marker c))

tshow :: (Show a) => a -> Text
tshow = T.pack . show
