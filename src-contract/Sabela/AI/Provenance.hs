{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

data Actor = Agent | Human | InBrowserChat
    deriving (Eq, Show)

actorTag :: Actor -> Text
actorTag Agent = "agent"
actorTag Human = "human"
actorTag InBrowserChat = "in_browser_chat"

parseActor :: Text -> Maybe Actor
parseActor "agent" = Just Agent
parseActor "human" = Just Human
parseActor "in_browser_chat" = Just InBrowserChat
parseActor _ = Nothing

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

outcomeJSON :: ToolOutcome -> Value
outcomeJSON (ToolOk v) = object ["isError" .= False, "result" .= v]
outcomeJSON (ToolErr v) = object ["isError" .= True, "result" .= v]

parseOutcome :: Value -> Parser ToolOutcome
parseOutcome = withObject "ToolOutcome" $ \o -> do
    isErr <- o .: "isError"
    v <- o .: "result"
    pure (if isErr then ToolErr v else ToolOk v)

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

safeLeaf :: String -> String
safeLeaf s =
    let cleaned = map (\c -> if c `elem` ("/\\" :: String) then '_' else c) s
     in if null cleaned then "unknown" else cleaned

recordEvent :: FilePath -> SessionEvent -> IO ()
recordEvent path ev = void (try go :: IO (Either SomeException ()))
  where
    go = do
        createDirectoryIfMissing True (takeDirectory path)
        appendFile path (LBS.unpack (encode ev) ++ "\n")

recordToolCall ::
    FilePath ->
    Maybe Text ->
    Actor ->
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
