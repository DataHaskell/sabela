{-# LANGUAGE OverloadedStrings #-}

{- | A repeated lexical miss under a standing goal is evidence the name is the
wrong axis, so the ladder re-asks by type instead. Everything the disclosure
states is read off the answer the type query returned; a query that never ran
is disclosed as one, and never as an answer that held nothing.
-}
module Siza.Agent.Discover.GoalEscalate (
    TypeAnswer (..),
    escalationNext,
    escalationQueries,
    producersOfType,
    spendEscalation,
) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..), toolWireName)
import Sabela.AI.FitSignature (Freeness (..), classifyRow)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Advice (setNext)
import Siza.Agent.Discover.Goal (goalClusterKey, producesGoal)
import Siza.Agent.Discover.Ledger (SearchLedger, ledgerRelease)
import Siza.Agent.Discover.Types (StandingGoal (..))

{- | A bare @:: T@ query answers with polymorphic bottoms, so the type query is
scoped by the packages the held facts name. Two is the cap: past that the
answers repeat and the call costs more than the lead is worth.
-}
maxEscalationPackages :: Int
maxEscalationPackages = 2

maxProducersShown :: Int
maxProducersShown = 2

maxProducerChars :: Int
maxProducerChars = 90

maxReasonChars :: Int
maxReasonChars = 60

{- | What the type queries came back with. A query that did not run is its own
outcome: it neither found producers nor established that there are none.
-}
data TypeAnswer
    = Found [(Text, Text, Text)]
    | NoneFound
    | NotRun Text
    deriving (Eq, Show)

{- | The type queries this escalation may spend, most-specific first. The bare
form is the fallback for a goal no held fact attributes to a package.
-}
escalationQueries :: StandingGoal -> [Text] -> [Text]
escalationQueries sg held
    | T.null ty = []
    | null pkgs = [":: " <> ty]
    | otherwise = ["+" <> p <> " :: " <> ty | p <- pkgs]
  where
    ty = T.strip (sgType sg)
    pkgs =
        take maxEscalationPackages
            . dedup
            . filter (not . T.null)
            $ sgPackage sg : held

{- | Issue the type queries until one answers, then disclose what was issued
and what came back. The dispatcher is the un-guarded one: a capability query is
not a discover call, so this cannot re-enter the ladder. The flag says whether
a query actually ran, so a caller that budgeted the spend can release it.
-}
escalateGoal ::
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    StandingGoal ->
    [Text] ->
    Text ->
    Value ->
    IO (Value, Bool)
escalateGoal dispatch sg held q v
    | null queries = pure (v, False)
    | otherwise = do
        (spent, answer) <- ask [] Nothing queries
        pure (setNext (escalationNext q (sgType sg) spent answer) v, ran answer)
  where
    queries = escalationQueries sg held
    ran (NotRun _) = False
    ran _ = True
    ask spent mFail [] = pure (reverse spent, maybe NoneFound NotRun mFail)
    ask spent mFail (tq : rest) = do
        r <- dispatch (typeQueryCall tq)
        let spent' = tq : spent
        case answerOf r of
            Left why -> ask spent' (mFail <|> Just why) rest
            Right ans -> case producersOfType (sgType sg) ans of
                [] -> ask spent' mFail rest
                ps -> pure (reverse spent', Found ps)

{- | Spend the goal's one type query and keep the budget honest: a query that
never ran bought nothing, so the cluster gets its query back.
-}
spendEscalation ::
    IORef SearchLedger ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    StandingGoal ->
    [Text] ->
    Text ->
    Value ->
    IO Value
spendEscalation ref dispatch sg held q v = do
    (v', ran) <- escalateGoal dispatch sg held q v
    if ran
        then pure v'
        else do
            atomicModifyIORef' ref $ \l ->
                (ledgerRelease (goalClusterKey (sgType sg)) l, ())
            pure v'

typeQueryCall :: Text -> ToolCall
typeQueryCall tq =
    ToolCall
        (toolWireName SearchCapability)
        (object ["query" .= tq, "semantic" .= False])

{- | The answer a dispatch produced, or why there is none. A refusal and a
transport failure are both failures to answer, and neither is an empty answer.
-}
answerOf :: Either Text ToolOutcome -> Either Text Value
answerOf (Right (ToolOk v)) = Right v
answerOf (Right (ToolErr v)) = Left (refusalReason v)
answerOf (Left e) = Left e

refusalReason :: Value -> Text
refusalReason v = case filter (not . T.null) (map (`textAt` v) reasonKeys) of
    (r : _) -> r
    [] -> "the type query was refused"
  where
    reasonKeys = ["error", "diagnostic", "summary", "state"]

{- | The producers of the goal a capability answer holds: named rows that
produce the goal and that the fit law does not call goal-free. Reads the bucket
shape (a package with an @api@ list) and the flat shape alike.
-}
producersOfType :: Text -> Value -> [(Text, Text, Text)]
producersOfType goal v =
    dedupOn fst3 [p | p <- bucketRows ++ flatRows, informativeProducer p]
  where
    hits = arrayAt "hits" v
    bucketRows =
        [ (textAt "name" e, textAt "type" e, textAt "module" e)
        | b <- hits
        , e <- arrayAt "api" b
        ]
    flatRows =
        [ (textAt "name" b, textAt "type" b, textAt "module" b)
        | b <- hits
        , null (arrayAt "api" b)
        ]
    informativeProducer (n, ty, _) =
        not (T.null n)
            && producesGoal goal ty
            && classifyRow n ty == Informative

{- | What the escalation did, in the caller's own terms: the query it spent and
what came back. With no producer it says so; with no answer it says that
instead, and names nothing either way.
-}
escalationNext :: Text -> Text -> [Text] -> TypeAnswer -> Text
escalationNext _ _ [] _ = ""
escalationNext q goal spent answer =
    "'"
        <> q
        <> "' repeated while goal "
        <> goal
        <> " stands — the name is the wrong axis; the type query "
        <> T.intercalate " then " spent
        <> outcome
  where
    outcome = case answer of
        Found found ->
            " returned: "
                <> T.intercalate
                    "; "
                    (map producerLine (take maxProducersShown found))
                <> "."
        NoneFound -> " returned no informative producer of " <> goal <> "."
        NotRun why ->
            " did not run ("
                <> clip maxReasonChars why
                <> "), so what produces "
                <> goal
                <> " is still unknown."

{- | One producer, clipped with the ellipsis that marks the clipping, so a
signature is never silently shortened into a different type.
-}
producerLine :: (Text, Text, Text) -> Text
producerLine (n, ty, m) = clip maxProducerChars (n <> " :: " <> ty <> moduleClause)
  where
    moduleClause = if T.null m then "" else " (" <> m <> ")"

clip :: Int -> Text -> Text
clip n t
    | T.length t <= n = t
    | otherwise = T.take n t <> "…"

arrayAt :: Text -> Value -> [Value]
arrayAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (Array a) -> toList a
    _ -> []
arrayAt _ _ = []

textAt :: Text -> Value -> Text
textAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
textAt _ _ = ""

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

dedup :: (Eq a) => [a] -> [a]
dedup = dedupOn id

dedupOn :: (Eq b) => (a -> b) -> [a] -> [a]
dedupOn f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs
