{- | The 'SessionEvent' record and its JSON wire encoding (redesign 7.1).

A 'SessionEvent' is not a new schema: it serialises the R2 contract types
('ToolName', 'ToolOutcome', 'KernelState', 'Diagnostic'), one JSON object per
tool call. This module owns the type and its instances; 'Siza.Provenance.Log'
owns the append-only file and the hash chain around it.
-}
module Siza.Provenance.Event (
    SessionEvent (..),
    Actor (..),
    Preflight (..),
    actorWire,
    parseActor,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Sabela.AI.Capabilities.ToolName (ToolName, parseToolName, toolWireName)
import Sabela.AI.KernelState (KernelState)
import Sabela.AI.Provenance (
    Actor (Agent, Human, InBrowserChat),
    actorTag,
    kernelStateProvJSON,
    outcomeJSON,
    parseActor,
    parseKernelState,
    parseOutcome,
 )
import Sabela.AI.Types (ToolOutcome)
import Siza.Language (Diagnostic (..), Severity (Error, Warning))

{- | The pre-flight verdict, present at the CLIENT seam only — the parse
result, the security findings, and whether a mutation was vetted. The server
never sees this (redesign 7.2), so it is the client log's reason to exist.
-}
data Preflight = Preflight
    { pfParsed :: Bool
    , pfFindings :: [Diagnostic]
    , pfVetted :: Bool
    }
    deriving (Eq, Show)

{- | One provenance record. Reuses the contract types verbatim: it is the
typed events serialised, not a parallel schema. @seGen@ together with
@seSession@ is the correlation key against the server log.
-}
data SessionEvent = SessionEvent
    { seAt :: UTCTime
    , seSession :: Text
    , seNotebook :: Text
    , seActor :: Actor
    , seCall :: ToolName
    , seInput :: Value
    , sePreflight :: Maybe Preflight
    , seOutcome :: ToolOutcome
    , seKernelBefore :: KernelState
    , seGen :: Int
    , sePrev :: Maybe Text
    }
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Wire encoding
-- ---------------------------------------------------------------------------

-- | The actor tag, sharing the canonical casing from 'Sabela.AI.Provenance'.
actorWire :: Actor -> Text
actorWire = actorTag

severityWire :: Severity -> Text
severityWire Error = "error"
severityWire Warning = "warning"

diagToJSON :: Diagnostic -> Value
diagToJSON d =
    object
        [ "severity" .= severityWire (dgSeverity d)
        , "line" .= dgLine d
        , "col" .= dgCol d
        , "message" .= dgMessage d
        ]

diagFromJSON :: Value -> Parser Diagnostic
diagFromJSON = withObject "Diagnostic" $ \o ->
    Diagnostic . sevFromText
        <$> o .: "severity"
        <*> o .:? "line"
        <*> o .:? "col"
        <*> o .: "message"
  where
    sevFromText :: Text -> Severity
    sevFromText "warning" = Warning
    sevFromText _ = Error

instance ToJSON Preflight where
    toJSON p =
        object
            [ "parsed" .= pfParsed p
            , "findings" .= map diagToJSON (pfFindings p)
            , "vetted" .= pfVetted p
            ]

instance FromJSON Preflight where
    parseJSON = withObject "Preflight" $ \o ->
        Preflight
            <$> o .: "parsed"
            <*> (o .: "findings" >>= mapM diagFromJSON)
            <*> o .: "vetted"

instance ToJSON SessionEvent where
    toJSON e =
        object
            [ "at" .= seAt e
            , "session" .= seSession e
            , "notebook" .= seNotebook e
            , "actor" .= actorTag (seActor e)
            , "tool" .= toolWireName (seCall e)
            , "input" .= seInput e
            , "preflight" .= sePreflight e
            , "outcome" .= outcomeJSON (seOutcome e)
            , "kernelBefore" .= kernelStateProvJSON (seKernelBefore e)
            , "gen" .= seGen e
            , "prev" .= sePrev e
            ]

instance FromJSON SessionEvent where
    parseJSON = withObject "SessionEvent" $ \o ->
        SessionEvent
            <$> o .: "at"
            <*> o .: "session"
            <*> o .: "notebook"
            <*> (actorFromText <$> o .: "actor")
            <*> (o .: "tool" >>= toolFromText)
            <*> o .: "input"
            <*> o .:? "preflight"
            <*> (o .: "outcome" >>= parseOutcome)
            <*> (o .: "kernelBefore" >>= parseKernelState)
            <*> o .: "gen"
            <*> o .:? "prev"
      where
        actorFromText t = fromMaybe Agent (parseActor t)
        toolFromText :: Text -> Parser ToolName
        toolFromText t =
            maybe (fail ("unknown tool in log: " <> T.unpack t)) pure (parseToolName t)
