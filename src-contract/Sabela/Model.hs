{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Sabela.Model (
    OutputItem (..),
    MimeType (..),
    mimeIndicator,
    textToMime,
    Notebook (..),
    Cell (..),
    CellType (..),
    RunMode (..),
    runModeTag,
    parseRunMode,
    lookupCell,
    cellLangOf,
    NotebookEvent (..),
    SessionStatus (..),
    statusTag,
    statusMessage,
    KernelPhase (..),
    kernelPhaseTag,
    CellError (..),
    bareCellError,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    object,
    withObject,
    (.:),
    (.=),
 )
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Sabela.Ids (EditId (..), ToolCallId (..), TurnId (..))
import Sabela.Model.Status (
    KernelPhase (..),
    SessionStatus (..),
    kernelPhaseTag,
    statusMessage,
    statusTag,
 )
import Sabela.SessionTypes (CellLang (..))
import ScriptHs.Markdown (MimeType (..))

data OutputItem = OutputItem
    { oiMime :: MimeType
    , oiOutput :: Text
    }
    deriving (Eq, Generic, Show)

mimeIndicator :: MimeType -> Text
mimeIndicator m = case m of
    MimeHtml -> "text/html"
    MimeMarkdown -> "text/markdown"
    MimeSvg -> "image/svg+xml"
    MimeLatex -> "text/latex"
    MimeJson -> "application/json"
    MimeImage t -> t <> ";base64"
    MimePlain -> "text/plain"

textToMime :: Text -> MimeType
textToMime m
    | Just t <- T.stripSuffix ";base64" m = MimeImage t
    | otherwise = case m of
        "text/html" -> MimeHtml
        "text/markdown" -> MimeMarkdown
        "image/svg+xml" -> MimeSvg
        "text/latex" -> MimeLatex
        "application/json" -> MimeJson
        _ -> MimePlain

instance ToJSON OutputItem where
    toJSON oi =
        object
            [ "oiMime" .= mimeIndicator (oiMime oi)
            , "oiOutput" .= oiOutput oi
            ]

instance FromJSON OutputItem where
    parseJSON = withObject "OutputItem" $ \o -> do
        mime <- o .: "oiMime"
        output <- o .: "oiOutput"
        pure (OutputItem (textToMime mime) output)

data Notebook = Notebook
    { nbTitle :: Text
    , nbCells :: [Cell]
    }
    deriving (Eq, Generic, Show)

instance ToJSON Notebook
instance FromJSON Notebook

data Cell = Cell
    { cellId :: Int
    , cellType :: CellType
    , cellLang :: CellLang
    , cellSource :: Text
    , cellOutputs :: [OutputItem]
    , cellError :: Maybe Text
    , cellDirty :: Bool
    }
    deriving (Eq, Generic, Show)

data CellType = CodeCell | ProseCell
    deriving (Eq, Generic, Show)

{- | Whether an edit runs its affected cells now or only marks them stale,
leaving one explicit drain (run-all) to execute the accumulated set.
-}
data RunMode = RunReactive | RunDeferred
    deriving (Eq, Generic, Show)

runModeTag :: RunMode -> Text
runModeTag RunReactive = "reactive"
runModeTag RunDeferred = "deferred"

parseRunMode :: Text -> Maybe RunMode
parseRunMode "reactive" = Just RunReactive
parseRunMode "deferred" = Just RunDeferred
parseRunMode _ = Nothing

instance ToJSON Cell
instance FromJSON Cell
instance ToJSON CellType
instance FromJSON CellType

data NotebookEvent
    = EvCellUpdating Int
    | EvCellCompiling Int
    | EvCellPartialOutput Int Text
    | EvCellResult Int [OutputItem] (Maybe Text) [CellError] [CellError]
    | EvWidget Int Text Text
    | EvExecutionDone
    | EvSessionStatus SessionStatus
    | EvInstallLog Text
    | EvChatTextDelta TurnId Text
    | EvChatToolCall TurnId ToolCallId Text Value
    | EvChatToolResult TurnId ToolCallId Value
    | EvChatEditProposed (Maybe TurnId) EditId Int Text Text
    | EvChatDone TurnId
    | EvChatCancelled TurnId
    | EvChatError (Maybe TurnId) Text
    | EvNotebookChanged Notebook
    | EvNotebookState Int [Int]
    | EvRunMode RunMode
    | EvKernelError KernelPhase Text [Int]
    | EvChatUsageUpdate TurnId Value
    deriving (Show)

instance ToJSON NotebookEvent where
    toJSON (EvCellUpdating cid) =
        object ["type" .= ("cellUpdating" :: Text), "cellId" .= cid]
    toJSON (EvCellCompiling cid) =
        object ["type" .= ("cellCompiling" :: Text), "cellId" .= cid]
    toJSON (EvCellPartialOutput cid line) =
        object
            ["type" .= ("cellPartialOutput" :: Text), "cellId" .= cid, "line" .= line]
    toJSON (EvCellResult cid outputs err errs warns) =
        object
            [ "type" .= ("cellResult" :: Text)
            , "cellId" .= cid
            , "outputs" .= outputs
            , "error" .= err
            , "errors" .= errs
            , "warnings" .= warns
            ]
    toJSON (EvWidget cid name value) =
        object
            [ "type" .= ("widget" :: Text)
            , "cellId" .= cid
            , "name" .= name
            , "value" .= value
            ]
    toJSON EvExecutionDone =
        object ["type" .= ("executionDone" :: Text)]
    toJSON (EvSessionStatus msg) =
        object
            [ "type" .= ("sessionStatus" :: Text)
            , "state" .= statusTag msg
            , "message" .= statusMessage msg
            , "deps" .= case msg of
                SUpdateDeps deps -> deps
                _ -> []
            ]
    toJSON (EvKernelError phase message cellIds) =
        object
            [ "type" .= ("kernelError" :: Text)
            , "phase" .= kernelPhaseTag phase
            , "message" .= message
            , "cellIds" .= cellIds
            ]
    toJSON (EvInstallLog line) =
        object ["type" .= ("installLog" :: Text), "line" .= line]
    toJSON (EvChatTextDelta tid text) =
        object
            [ "type" .= ("chatTextDelta" :: Text)
            , "turnId" .= tid
            , "text" .= text
            ]
    toJSON (EvChatToolCall tid tcId toolName input) =
        object
            [ "type" .= ("chatToolCall" :: Text)
            , "turnId" .= tid
            , "toolCallId" .= tcId
            , "tool" .= toolName
            , "input" .= input
            ]
    toJSON (EvChatToolResult tid tcId result) =
        object
            [ "type" .= ("chatToolResult" :: Text)
            , "turnId" .= tid
            , "toolCallId" .= tcId
            , "result" .= result
            ]
    toJSON (EvChatEditProposed tid eid cid oldSrc newSrc) =
        object
            [ "type" .= ("chatEditProposed" :: Text)
            , "turnId" .= tid
            , "editId" .= eid
            , "cellId" .= cid
            , "oldSource" .= oldSrc
            , "newSource" .= newSrc
            ]
    toJSON (EvChatDone tid) =
        object ["type" .= ("chatDone" :: Text), "turnId" .= tid]
    toJSON (EvChatCancelled tid) =
        object ["type" .= ("chatCancelled" :: Text), "turnId" .= tid]
    toJSON (EvChatError tid msg) =
        object
            [ "type" .= ("chatError" :: Text)
            , "turnId" .= tid
            , "message" .= msg
            ]
    toJSON (EvNotebookChanged nb) =
        object
            [ "type" .= ("notebookChanged" :: Text)
            , "notebook" .= nb
            ]
    toJSON (EvNotebookState epoch staleIds) =
        object
            [ "type" .= ("notebookState" :: Text)
            , "epoch" .= epoch
            , "staleIds" .= staleIds
            ]
    toJSON (EvRunMode mode) =
        object
            [ "type" .= ("runMode" :: Text)
            , "mode" .= runModeTag mode
            ]
    toJSON (EvChatUsageUpdate tid payload) =
        object
            [ "type" .= ("chatUsageUpdate" :: Text)
            , "turnId" .= tid
            , "usage" .= payload
            ]

data CellError = CellError
    { ceLine :: Maybe Int
    , ceCol :: Maybe Int
    , ceMessage :: Text
    , ceCode :: Maybe Int
    }
    deriving (Eq, Generic, Show)

instance ToJSON CellError
instance FromJSON CellError

bareCellError :: Maybe Int -> Maybe Int -> Text -> CellError
bareCellError l c m = CellError l c m Nothing

lookupCell :: Int -> Notebook -> Maybe Cell
lookupCell cid nb = find (\c -> cellId c == cid) (nbCells nb)

cellLangOf :: Int -> Notebook -> CellLang
cellLangOf cid nb = maybe Haskell cellLang (lookupCell cid nb)
