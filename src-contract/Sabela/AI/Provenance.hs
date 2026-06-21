{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | The server-side provenance log (design Part 7): one append-only JSONL
record per @/api/ai/tool@ call. It is NOT a new schema — a 'SessionEvent'
serialises the same contract values the agent acted on ('ToolName',
'ToolOutcome', 'KernelState'), so the audit record and the wire are one
definition and cannot drift.

The server seam ('Sabela.Server.Ai.aiToolH') is the authoritative trust
anchor; 'recordEvent' is best-effort, so a log-write failure can never fail
the tool call (the caller guards it with 'try').
-}
module Sabela.AI.Provenance (
    SessionEvent (..),
    Actor (..),
    actorTag,
    parseActor,
    sessionEventJSON,
    outcomeJSON,
    parseOutcome,
    kernelStateProvJSON,
    parseKernelState,
    sessionLogPath,
    stateBase,
    recordEvent,
    recordToolCall,
) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))

import Sabela.AI.Capabilities.ToolName (
    ToolName,
    parseToolName,
    toolWireName,
 )
import Sabela.AI.KernelState (
    Activity (..),
    KernelState (..),
 )
import Sabela.AI.Types (ToolOutcome (..))

{- | Who drove a tool call. The server seam is the only place that sees all
three: the agent (the REST bridge), the human clicking Run, and the
in-browser chat. This run records 'Agent' only; the enum is in place so the
human / in-browser-chat actors are an additive later extension.
-}
data Actor = Agent | Human | InBrowserChat
    deriving (Show, Eq)

actorTag :: Actor -> Text
actorTag Agent = "agent"
actorTag Human = "human"
actorTag InBrowserChat = "in_browser_chat"

parseActor :: Text -> Maybe Actor
parseActor "agent" = Just Agent
parseActor "human" = Just Human
parseActor "in_browser_chat" = Just InBrowserChat
parseActor _ = Nothing

{- | One provenance record. Reuses the contract types verbatim: 'seCall' is
the typed 'ToolName', 'seOutcome' the typed 'ToolOutcome', 'seKernelBefore'
the typed 'KernelState'. @(seSession, seGen)@ is the correlation key the
client and server logs share. 'sePrev' is the SHA-256 of the prior record
for an opt-in hash-chain; 'Nothing' for the default plain append-only log.
-}
data SessionEvent = SessionEvent
    { seAt :: UTCTime
    , seSession :: Text
    , seNotebook :: Text
    , seActor :: Actor
    , seCall :: ToolName
    , seInput :: Value
    , seOutcome :: ToolOutcome
    , seKernelBefore :: KernelState
    , seGen :: Int
    , sePrev :: Maybe Text
    }

instance ToJSON SessionEvent where
    toJSON = sessionEventJSON

instance FromJSON SessionEvent where
    parseJSON = withObject "SessionEvent" $ \o -> do
        seAt <- o .: "at"
        seSession <- o .: "session"
        seNotebook <- o .: "notebook"
        actorTxt <- o .: "actor"
        seActor <-
            maybe (fail "unknown actor") pure (parseActor actorTxt)
        callTxt <- o .: "tool"
        seCall <-
            maybe (fail "unknown tool name") pure (parseToolName callTxt)
        seInput <- o .: "input"
        _ <- o .:? "preflight" :: Parser (Maybe Value)
        seOutcome <- (o .: "outcome") >>= parseOutcome
        seKernelBefore <- (o .: "kernelBefore") >>= parseKernelState
        seGen <- o .: "gen"
        sePrev <- o .: "prev"
        pure SessionEvent{..}

-- | The JSONL line shape. The two contract values nest under tagged objects.
sessionEventJSON :: SessionEvent -> Value
sessionEventJSON ev =
    object
        [ "at" .= seAt ev
        , "session" .= seSession ev
        , "notebook" .= seNotebook ev
        , "actor" .= actorTag (seActor ev)
        , "tool" .= toolWireName (seCall ev)
        , "input" .= seInput ev
        , "preflight" .= (Nothing :: Maybe Value)
        , "outcome" .= outcomeJSON (seOutcome ev)
        , "kernelBefore" .= kernelStateProvJSON (seKernelBefore ev)
        , "gen" .= seGen ev
        , "prev" .= sePrev ev
        ]

{- | 'ToolOutcome' tagged so the @isError@ axis survives the round-trip. The
inner payload key is @result@, matching the client log codec
('Siza.Provenance.Event') so a server-written line decodes there.
-}
outcomeJSON :: ToolOutcome -> Value
outcomeJSON (ToolOk v) = object ["isError" .= False, "result" .= v]
outcomeJSON (ToolErr v) = object ["isError" .= True, "result" .= v]

parseOutcome :: Value -> Parser ToolOutcome
parseOutcome = withObject "ToolOutcome" $ \o -> do
    isErr <- o .: "isError"
    v <- o .: "result"
    pure (if isErr then ToolErr v else ToolOk v)

{- | A reversible 'KernelState' encoding for the log, matching the client codec
('Siza.Provenance.Event'): the activity rides as a string @activity@ tag, not
a boolean, so the two seams share one shape.
-}
kernelStateProvJSON :: KernelState -> Value
kernelStateProvJSON Cold = object ["state" .= ("cold" :: Text)]
kernelStateProvJSON (Alive gen activity building) =
    object
        [ "state" .= ("alive" :: Text)
        , "ksGen" .= gen
        , "activity" .= activityWire activity
        , "building" .= building
        ]

activityWire :: Activity -> Text
activityWire Executing = "executing"
activityWire Idle = "idle"

parseKernelState :: Value -> Parser KernelState
parseKernelState = withObject "KernelState" $ \o -> do
    st <- o .: "state" :: Parser Text
    case st of
        "cold" -> pure Cold
        _ -> do
            gen <- o .: "ksGen"
            activity <- o .: "activity" :: Parser Text
            building <- o .: "building"
            pure
                Alive
                    { ksGen = gen
                    , ksActivity = if activity == "executing" then Executing else Idle
                    , ksBuilding = building
                    }

{- | The per-session log path:
@<state>/sabela/sessions/<notebook-id>/<session-id>.jsonl@, honouring
@$XDG_STATE_HOME@ (else @$HOME/.local/state@) — the same base the discovery
registry uses. @notebook-id@ is the work-dir basename; @session-id@ is the
@X-Sabela-Session@ value (sanitised to a path-safe leaf).
-}
sessionLogPath :: FilePath -> Text -> IO FilePath
sessionLogPath workDir session = do
    base <- stateBase
    let notebookId = safeLeaf (takeFileName workDir)
        sessionId = safeLeaf (T.unpack session)
        dir = base </> "sabela" </> "sessions" </> notebookId
    pure (dir </> (sessionId ++ ".jsonl"))

stateBase :: IO FilePath
stateBase = do
    mxdg <- lookupEnv "XDG_STATE_HOME"
    case mxdg of
        Just x | not (null x) -> pure x
        _ -> do
            home <- getHomeDirectory
            pure (home </> ".local" </> "state")

-- | Keep a log path component to a single safe leaf (no separators / empties).
safeLeaf :: String -> String
safeLeaf s =
    let cleaned = map (\c -> if c `elem` ("/\\" :: String) then '_' else c) s
     in if null cleaned then "unknown" else cleaned

{- | Append one JSONL record to the session log, creating the directory tree
on demand. Best-effort: any IO failure is swallowed so a log write can never
fail the tool call (the server seam additionally wraps the call in 'try').
-}
recordEvent :: FilePath -> SessionEvent -> IO ()
recordEvent path ev = void (try go :: IO (Either SomeException ()))
  where
    go = do
        createDirectoryIfMissing True (takeDirectory path)
        appendFile path (LBS.unpack (encode ev) ++ "\n")

{- | The server-seam recording glue: resolve the per-session path, stamp the
time, and append one record for a tool call. Best-effort and additive — an
unparseable tool name is not logged, the @X-Sabela-Session@ default is the
@browser@ sentinel, and 'recordEvent' swallows any write failure so logging
can never fail the call. The default 'sePrev' is 'Nothing' (plain append-only;
the hash-chain is opt-in for the hub).
-}
recordToolCall ::
    -- | the server work dir (notebook-id is its basename)
    FilePath ->
    -- | the @X-Sabela-Session@ header; 'Nothing' => the browser path
    Maybe Text ->
    Actor ->
    -- | the boundary tool-name string
    Text ->
    Value ->
    ToolOutcome ->
    KernelState ->
    Int ->
    IO ()
recordToolCall workDir mSession actor name input outcome kBefore gen =
    case parseToolName name of
        Nothing -> pure ()
        Just call -> do
            now <- getCurrentTime
            path <- sessionLogPath workDir session
            recordEvent
                path
                SessionEvent
                    { seAt = now
                    , seSession = session
                    , seNotebook = T.pack (takeFileName workDir)
                    , seActor = actor
                    , seCall = call
                    , seInput = input
                    , seOutcome = outcome
                    , seKernelBefore = kBefore
                    , seGen = gen
                    , sePrev = Nothing
                    }
  where
    session = fromMaybe "browser" mSession
