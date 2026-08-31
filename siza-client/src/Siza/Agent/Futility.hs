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

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Futility.Rejection (
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
 )
import Siza.Agent.VerifyMemo (Seal, currentSeal)

data GuardState = GuardState
    { gsCalls :: !(Map (Text, Text) Text)
    , gsRuns :: !(Map Text RejectionRun)
    , gsEpochRuns :: !(Map Text RejectionRun)
    , gsCached :: !(Map (Text, Text) CachedRejection)
    , gsEpoch :: !Int
    }

newtype FutilityGuard = FutilityGuard (IORef GuardState)

newFutilityGuard :: IO FutilityGuard
newFutilityGuard =
    FutilityGuard
        <$> newIORef
            GuardState
                { gsCalls = Map.empty
                , gsRuns = Map.empty
                , gsEpochRuns = Map.empty
                , gsCached = Map.empty
                , gsEpoch = 0
                }

futilityNote :: Text
futilityNote =
    "This call's name and arguments match an earlier call, and the recorded \
    \error is identical."

sourceFaultNote :: Text
sourceFaultNote =
    "This exact source was rejected before with the identical compiler diagnostic."

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
    reused <- validatedCache ref dispatch call
    maybe fresh pure reused
  where
    fresh = do
        out <- dispatch call
        seal <- rejectionSeal dispatch call out
        _ <- atomicModifyIORef' ref (afterDispatch call out seal)
        observe False ref call out

validatedCache ::
    IORef GuardState ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Maybe (Either Text ToolOutcome))
validatedCache ref dispatch call = do
    cached <- Map.lookup (callKey call) . gsCached <$> readIORef ref
    case cached of
        Just c | completeWrite call -> do
            seal <- currentSeal dispatch
            if seal == Just (cachedSeal c)
                then Just <$> observe True ref call (Right (cachedOutcome c))
                else do
                    atomicModifyIORef' ref (\s -> (invalidateWorld s, ()))
                    pure Nothing
        _ -> pure Nothing

rejectionSeal ::
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO (Maybe Seal)
rejectionSeal dispatch call out
    | completeWrite call && cacheableRejection out = currentSeal dispatch
    | otherwise = pure Nothing

observe ::
    Bool ->
    IORef GuardState ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO (Either Text ToolOutcome)
observe reused ref call out = do
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
            epoch <- gsEpoch <$> readIORef ref
            pure (if reused then cachedFact epoch annotated else annotated)

afterDispatch ::
    ToolCall ->
    Either Text ToolOutcome ->
    Maybe Seal ->
    GuardState ->
    (GuardState, Int)
afterDispatch call out seal s
    | worldChanging call && not (settledNoMutation out) =
        let s' = invalidateWorld s
         in (s', gsEpoch s')
    | completeWrite call
    , cacheableRejection out
    , Right rejected <- out =
        case seal of
            Just observed ->
                ( s
                    { gsCached =
                        Map.insert
                            (callKey call)
                            (CachedRejection rejected observed)
                            (gsCached s)
                    }
                , gsEpoch s
                )
            Nothing -> (s, gsEpoch s)
    | otherwise = (s, gsEpoch s)

invalidateWorld :: GuardState -> GuardState
invalidateWorld s =
    s
        { gsEpochRuns = Map.empty
        , gsCached = Map.empty
        , gsEpoch = gsEpoch s + 1
        }

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
        s
            { gsCalls = Map.insert key ft (gsCalls s)
            , gsRuns = maybe (gsRuns s) (bump (gsRuns s)) mClass
            , gsEpochRuns = maybe (gsEpochRuns s) (bump (gsEpochRuns s)) mClass
            }
    prevRun = flip Map.lookup (gsEpochRuns s) =<< mClass
    bump runs cls = Map.insert cls (extend (Map.lookup cls runs)) runs
    extend Nothing = RejectionRun 1 (Set.singleton src) src
    extend (Just r) =
        RejectionRun (rrCount r + 1) (Set.insert src (rrSources r)) src

{- | How often each diagnostic class recurred after the call that introduced
it. Zero for a class seen once, so an absent repeat is still a recorded fact.
-}
rejectionRepeats :: FutilityGuard -> IO (Map Text Int)
rejectionRepeats (FutilityGuard ref) =
    Map.map (subtract 1 . rrCount) . gsRuns <$> readIORef ref

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
