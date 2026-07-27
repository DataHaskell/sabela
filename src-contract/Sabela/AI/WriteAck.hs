{-# LANGUAGE OverloadedStrings #-}

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

data RefusalAck = RefusalAck
    { raKind :: Text
    , raCell :: Maybe Int
    , raMessage :: Text
    }
    deriving (Eq, Show)

refusalAck :: Text -> Maybe Int -> Value -> Value
refusalAck kind mCell (Object o) =
    Object (KM.union o (KM.fromList extras))
  where
    extras =
        (Key.fromText "notCommitted", String kind)
            : [(Key.fromText "cellId", jsonInt c) | Just c <- [mCell]]
    jsonInt = Number . fromIntegral
refusalAck _ _ v = v

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

data AckEnvelope = EnvWrite WriteAck | EnvBusy BusyAck | EnvRefusal RefusalAck
    deriving (Eq, Show)

parseAckEnvelope :: Value -> Maybe AckEnvelope
parseAckEnvelope (Object o)
    | KM.lookup "busy" o == Just (Bool True)
    , Just (String "own-write") <- KM.lookup "cause" o =
        EnvBusy <$> (BusyAck <$> intF "cellId" <*> intF "elapsedMs")
    | Just (String kind) <- KM.lookup "notCommitted" o =
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
