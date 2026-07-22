{-# LANGUAGE OverloadedStrings #-}

{- | The declared write-ack envelope (R6.1/R6.2/R6.4, R3.6): ONE shape for
every mutation-write response — queued, executing, completed (ok or error),
duplicate — plus the own-write busy bounce. Producers encode through
'writeAckJson' / 'busyAckJson' only; consumers decode through
'parseAckEnvelope', which rejects serialisation-in-string execution fields.
-}
module Sabela.AI.WriteAck (
    AckStatus (..),
    ackStatusText,
    parseAckStatus,
    WriteAck (..),
    writeAckJson,
    BusyAck (..),
    busyAckJson,
    RefusalAck (..),
    refusalAck,
    pendingErrorAck,
    AckEnvelope (..),
    parseAckEnvelope,
    executingAckCell,
    landedNote,
    dupRunningNote,
    dupSettledNote,
    ownWriteHint,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Api (errorJsonWith)

-- | Where a landed write's execution stands when the response is built.
data AckStatus = AckQueued | AckExecuting | AckCompleted
    deriving (Bounded, Enum, Eq, Show)

ackStatusText :: AckStatus -> Text
ackStatusText AckQueued = "queued"
ackStatusText AckExecuting = "executing"
ackStatusText AckCompleted = "completed"

parseAckStatus :: Text -> Maybe AckStatus
parseAckStatus "queued" = Just AckQueued
parseAckStatus "executing" = Just AckExecuting
parseAckStatus "completed" = Just AckCompleted
parseAckStatus _ = Nothing

{- | The one write-ack envelope: the durable write's cell plus its execution
status. @waExecution@ is @Just@ only once the run settled (@Just Null@ for a
cell class that never executes); @waDuplicate@ marks an idempotent-retry reply.
-}
data WriteAck = WriteAck
    { waCellId :: Int
    , waStatus :: AckStatus
    , waHash :: Maybe Text
    , waExecution :: Maybe Value
    , waDuplicate :: Bool
    , waNote :: Maybe Text
    }
    deriving (Eq, Show)

writeAckJson :: WriteAck -> Value
writeAckJson wa =
    object $
        [ "cellId" .= waCellId wa
        , "status" .= ackStatusText (waStatus wa)
        ]
            <> ["hash" .= h | Just h <- [waHash wa]]
            <> ["execution" .= e | Just e <- [waExecution wa]]
            <> ["duplicate" .= True | waDuplicate wa]
            <> ["note" .= n | Just n <- [waNote wa]]

{- | The kernel-needing-call bounce while the caller's own write is executing
(R6.4): names the writing cell, the elapsed time, and the reconcile path.
-}
data BusyAck = BusyAck
    { baCellId :: Int
    , baElapsedMs :: Int
    }
    deriving (Eq, Show)

busyAckJson :: BusyAck -> Value
busyAckJson ba =
    errorJsonWith
        ( "The kernel is still executing your own write (cell "
            <> tshow (baCellId ba)
            <> ", "
            <> tshow (baElapsedMs ba)
            <> "ms elapsed). Do NOT re-send it."
        )
        [ "busy" .= True
        , "cause" .= ("own-write" :: Text)
        , "cellId" .= baCellId ba
        , "elapsedMs" .= baElapsedMs ba
        , "hint" .= ownWriteHint
        ]

{- | A mutation the server declined to commit as written (R3.6): a
signature-without-body proposal, a pending-error dam, a duplicate-def conflict.
It never damaged the session, so it is not an @error@ the caller relays raw —
it names its class ('raKind'), the cell it points at when it names one, and its
one-line message, and the caller routes on the class.
-}
data RefusalAck = RefusalAck
    { raKind :: Text
    , raCell :: Maybe Int
    , raMessage :: Text
    }
    deriving (Eq, Show)

{- | Tag an existing 'Sabela.Api.errorJsonWith' refusal object as a decodable
refusal-class ack: insert the @refusal@ discriminant (and @cellId@, when the
refusal names a cell) so the one 'parseAckEnvelope' validator decodes it — no
new envelope shape, the same @error@-plus-fields object every refusal already
emits. A non-object is returned untouched.
-}
refusalAck :: Text -> Maybe Int -> Value -> Value
refusalAck kind mCell (Object o) =
    Object (KM.union o (KM.fromList extras))
  where
    extras =
        (Key.fromText "refusal", String kind)
            : [(Key.fromText "cellId", jsonInt c) | Just c <- [mCell]]
    jsonInt = Number . fromIntegral
refusalAck _ _ v = v

{- | The pending-error refusal envelope (R10-T2): a new cell cannot be added
atop a red one. It names the ONE class of legal move — replace_cell_source on
the blocking cell, or delete_cell — and, when an unchecked mechanical proposal
for that cell exists, returns it as @suggestedSource@ with an explicit status.
It NEVER
re-ships the raw multi-line GHC error: the compiler text lives on the red cell,
not in every downstream insert refusal. Decodes on the one 'parseAckEnvelope'
refusal arm; @suggestedSource@ mirrors the sig-body proposal's own field.
-}
pendingErrorAck :: Int -> Maybe Text -> Value
pendingErrorAck cid mCand =
    refusalAck "pending-error" (Just cid) (errorJsonWith msg extras)
  where
    moves =
        "replace_cell_source(cell_id="
            <> tshow cid
            <> ", new_source=…) to fix cell "
            <> tshow cid
            <> " in place, or delete_cell("
            <> tshow cid
            <> ") to remove it."
    (msg, extras) = case mCand of
        Just cand ->
            ( "Cell "
                <> tshow cid
                <> " must go green before another cell can run. A repair proposal is \
                   \ready — apply it with "
                <> moves
                <> " The compiler will check the proposed source when it is applied."
            ,
                [ "pendingErrorCell" .= cid
                , "suggestedSource" .= cand
                , "suggestedSourceStatus" .= ("unchecked" :: Text)
                ]
            )
        Nothing ->
            ( "Cell "
                <> tshow cid
                <> " has an unresolved error, so a new cell cannot be added. Use "
                <> moves
            , ["pendingErrorCell" .= cid]
            )

-- | The shapes a caller may decode a mutation-path response into.
data AckEnvelope = EnvWrite WriteAck | EnvBusy BusyAck | EnvRefusal RefusalAck
    deriving (Eq, Show)

{- | Decode a response against the declared shape. An @execution@ field that
is a JSON string is serialisation-in-string and fails the decode outright.
-}
parseAckEnvelope :: Value -> Maybe AckEnvelope
parseAckEnvelope (Object o)
    | KM.lookup "busy" o == Just (Bool True)
    , Just (String "own-write") <- KM.lookup "cause" o =
        EnvBusy <$> (BusyAck <$> intF "cellId" <*> intF "elapsedMs")
    | Just (String kind) <- KM.lookup "refusal" o =
        Just (EnvRefusal (RefusalAck kind (intF "cellId") (msgField)))
    | otherwise = do
        cid <- intF "cellId"
        st <- parseAckStatus =<< txtF "status"
        ex <- execField
        pure $
            EnvWrite
                WriteAck
                    { waCellId = cid
                    , waStatus = st
                    , waHash = txtF "hash"
                    , waExecution = ex
                    , waDuplicate = KM.lookup "duplicate" o == Just (Bool True)
                    , waNote = txtF "note"
                    }
  where
    intF k = case KM.lookup (Key.fromText k) o of
        Just (Number n) -> Just (round n)
        _ -> Nothing
    txtF k = case KM.lookup (Key.fromText k) o of
        Just (String s) -> Just s
        _ -> Nothing
    execField = case KM.lookup "execution" o of
        Nothing -> Just Nothing
        Just (String _) -> Nothing
        Just v -> Just (Just v)
    msgField = case KM.lookup "error" o of
        Just (String s) -> s
        _ -> ""
parseAckEnvelope _ = Nothing

-- | The cell id of an executing (not yet settled) write ack, if that is what @v@ is.
executingAckCell :: Value -> Maybe Int
executingAckCell v = case parseAckEnvelope v of
    Just (EnvWrite wa) | waStatus wa == AckExecuting -> Just (waCellId wa)
    _ -> Nothing

landedNote :: Int -> Text
landedNote cid =
    "The write landed (cell "
        <> tshow cid
        <> ") and is still executing. Call await_idle to collect the result; \
           \do NOT re-send this write."

dupRunningNote :: Int -> Text
dupRunningNote cid =
    "This exact write already landed (cell "
        <> tshow cid
        <> ") and is still executing — not re-inserted. Call await_idle for \
           \the result."

dupSettledNote :: Int -> Text
dupSettledNote cid =
    "This exact write already landed (cell "
        <> tshow cid
        <> ") — not re-inserted. Its settled outcome is in `execution`."

ownWriteHint :: Text
ownWriteHint =
    "Call await_idle to wait for your write to finish, then continue."

tshow :: (Show a) => a -> Text
tshow = T.pack . show
