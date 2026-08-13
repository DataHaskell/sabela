{-# LANGUAGE OverloadedStrings #-}

{- |
Technique: dispatch-layer repeat guard [Gating/Repair].
Guarantee: a repeated call or repeated diagnostic is answered, never silently re-run.
Entry: 'guardDispatch'. Siblings: 'Siza.Agent.Owned.noProgressStep' (episode layer), Siza.Agent.Streak (cell layer).
-}
module Siza.Agent.Futility (
    FutilityGuard,
    newFutilityGuard,
    guardDispatch,
    futilityNote,
    sourceFaultNote,
    noteFor,
    normaliseDiagnostic,
    rejectionRepeats,
    unchangedState,
) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Sabela.AI.SelfHeal (sourceDelta)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))

{- | What a diagnostic class has already cost: how many calls it answered, the
submitted sources it answered them for, and the most recent of those.
-}
data RejectionRun = RejectionRun
    { rrCount :: !Int
    , rrSources :: !(Set Text)
    , rrLast :: !Text
    }

data GuardState = GuardState
    { gsCalls :: !(Map (Text, Text) Text)
    , gsRuns :: !(Map Text RejectionRun)
    }

newtype FutilityGuard = FutilityGuard (IORef GuardState)

newFutilityGuard :: IO FutilityGuard
newFutilityGuard = FutilityGuard <$> newIORef (GuardState Map.empty Map.empty)

futilityNote :: Text
futilityNote =
    "This call was byte-identical to an earlier call and failed with the \
    \identical error. Re-sending or re-phrasing the same payload will not \
    \change the outcome - the payload is not the fault. Change approach: \
    \check kernel_status / list_cells, use a different tool, or take a \
    \smaller step."

sourceFaultNote :: Text
sourceFaultNote =
    "This exact source was rejected before with the identical diagnostic. It \
    \is deterministic: the fault is in the source, not the kernel or the \
    \environment. Read the diagnostic above and change the source it names."

noteFor :: Either Text ToolOutcome -> Text
noteFor out
    | deterministicRejection out = sourceFaultNote
    | otherwise = futilityNote

deterministicRejection :: Either Text ToolOutcome -> Bool
deterministicRejection (Right (ToolErr (Object o))) =
    KM.member (K.fromText "notCommitted") o
        || KM.lookup (K.fromText "verdict") o == Just (String "diagnostic")
        || KM.member (K.fromText "diagnostic") o
deterministicRejection _ = False

guardDispatch ::
    FutilityGuard ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
guardDispatch (FutilityGuard ref) dispatch call = do
    out <- dispatch call
    let key = callKey call
        src = submittedSource call
        mClass = diagnosticClass out
    case failureText out of
        Nothing -> do
            atomicModifyIORef' ref (\s -> (forgetCall key s, ()))
            pure out
        Just ft -> do
            (prevFt, prevRun) <-
                atomicModifyIORef' ref (record key ft mClass src)
            let marked = maybe out (markUnchanged src out) prevRun
                annotated
                    | prevFt == Just ft = annotate (noteFor out) marked
                    | otherwise = marked
            pure annotated

forgetCall :: (Text, Text) -> GuardState -> GuardState
forgetCall key s = s{gsCalls = Map.delete key (gsCalls s)}

{- | Fold one failed call into the guard, returning what it already knew: the
failure this call's arguments produced before, and the run this call's
diagnostic class was already on.
-}
record ::
    (Text, Text) ->
    Text ->
    Maybe Text ->
    Text ->
    GuardState ->
    (GuardState, (Maybe Text, Maybe RejectionRun))
record key ft mClass src s = (s', (Map.lookup key (gsCalls s), prevRun))
  where
    s' =
        GuardState
            { gsCalls = Map.insert key ft (gsCalls s)
            , gsRuns = maybe (gsRuns s) bump mClass
            }
    prevRun = flip Map.lookup (gsRuns s) =<< mClass
    bump cls = Map.insert cls (extend prevRun) (gsRuns s)
    extend Nothing = RejectionRun 1 (Set.singleton src) src
    extend (Just r) =
        RejectionRun (rrCount r + 1) (Set.insert src (rrSources r)) src

{- | How often each diagnostic class recurred after the call that introduced
it. Zero for a class seen once, so an absent repeat is still a recorded fact.
-}
rejectionRepeats :: FutilityGuard -> IO (Map Text Int)
rejectionRepeats (FutilityGuard ref) =
    Map.map (subtract 1 . rrCount) . gsRuns <$> readIORef ref

{- | A diagnostic with its @\<interactive\>@ positions erased. Two rejections
that differ only in where the session happened to place the candidate are the
same diagnostic, and the model cannot act on the difference.
-}
normaliseDiagnostic :: Text -> Text
normaliseDiagnostic t = case T.breakOn interactiveMarker t of
    (_, rest) | T.null rest -> t
    (pre, rest) ->
        let body = T.drop (T.length interactiveMarker) rest
            (pos, after) = T.span positionChar body
         in pre
                <> interactiveMarker
                <> (if T.null pos then "" else "L:C")
                <> normaliseDiagnostic after
  where
    positionChar c = isDigit c || c == ':' || c == '-'

interactiveMarker :: Text
interactiveMarker = "<interactive>:"

{- | The diagnostic class a deterministic rejection belongs to. Only a
rejection carrying a diagnostic has one: an outcome with no diagnostic gives
the guard nothing to compare.
-}
diagnosticClass :: Either Text ToolOutcome -> Maybe Text
diagnosticClass out@(Right (ToolErr (Object o)))
    | deterministicRejection out
    , Just (String d) <- KM.lookup (K.fromText "diagnostic") o
    , not (T.null (T.strip d)) =
        Just (normaliseDiagnostic d)
diagnosticClass _ = Nothing

{- | Say that this diagnostic is the one an earlier call already produced, and
over how many distinct sources. Both numbers are counted here; nothing is
claimed about where the cause is.
-}
markUnchanged ::
    Text -> Either Text ToolOutcome -> RejectionRun -> Either Text ToolOutcome
markUnchanged src (Right (ToolErr (Object o))) prev =
    Right (ToolErr (Object (KM.insert (K.fromText "unchanged") detail marked)))
  where
    marked = KM.insert (K.fromText "state") (String unchangedState) o
    detail =
        object
            ( [ "priorCalls" .= rrCount prev
              , "distinctSources" .= Set.size sources
              , "sourceChanged" .= changed
              , "note" .= unchangedNote (rrCount prev) (Set.size sources)
              ]
                <> changedLinesPairs
            )
    sources = Set.filter (not . T.null) (Set.insert src (rrSources prev))
    changed = src /= rrLast prev
    (removed, added) = sourceDelta (rrLast prev) src
    dropped =
        length removed
            + length added
            - length (take deltaLines removed)
            - length (take deltaLines added)
    changedLinesPairs
        | not changed = []
        | otherwise =
            [ "changedLines"
                .= object
                    ( [ "removed" .= toJSON (take deltaLines removed)
                      , "added" .= toJSON (take deltaLines added)
                      ]
                        <> ["furtherLines" .= dropped | dropped > 0]
                    )
            ]
markUnchanged _ out _ = out

-- | How many changed lines either side of the delta is worth carrying.
deltaLines :: Int
deltaLines = 12

unchangedState :: Text
unchangedState = "unchanged"

{- | Only what was counted: how many calls this diagnostic answered, and how
many different non-empty sources those calls submitted.
-}
unchangedNote :: Int -> Int -> Text
unchangedNote priorCalls distinct =
    "This diagnostic is identical, ignoring <interactive> line and column \
    \numbers, to the one "
        <> tshow priorCalls
        <> " earlier call(s) in this session produced."
        <> sourceSentence
  where
    sourceSentence
        | distinct <= 0 = ""
        | otherwise =
            " Counting this call, "
                <> tshow distinct
                <> " distinct submitted source(s) have produced it."

tshow :: (Show a) => a -> Text
tshow = T.pack . show

callKey :: ToolCall -> (Text, Text)
callKey (ToolCall n a) = (n, encodeText a)

{- | The source text a write submitted, for the delta between two rejections.
A call carrying none contributes the empty source, which is still one value.
-}
submittedSource :: ToolCall -> Text
submittedSource (ToolCall _ (Object o)) =
    fromMaybe
        ""
        (listToMaybe [s | k <- sourceKeys, Just (String s) <- [KM.lookup k o]])
  where
    sourceKeys = map K.fromText ["source", "new_source", "code"]
submittedSource _ = ""

failureText :: Either Text ToolOutcome -> Maybe Text
failureText (Left e) = Just e
failureText (Right (ToolErr v)) = Just (encodeText v)
failureText (Right (ToolOk _)) = Nothing

annotate :: Text -> Either Text ToolOutcome -> Either Text ToolOutcome
annotate n (Left e) = Left (e <> " " <> n)
annotate n (Right (ToolErr (Object o))) =
    Right (ToolErr (Object (KM.insert (K.fromText "futility") (String n) o)))
annotate n (Right (ToolErr v)) =
    Right (ToolErr (object ["error" .= v, "futility" .= String n]))
annotate _ ok = ok

encodeText :: Value -> Text
encodeText = TE.decodeUtf8With TEE.lenientDecode . LBS.toStrict . encode
