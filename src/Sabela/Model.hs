{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Model (
    -- * Output
    OutputItem (..),

    -- * Notebook and cells
    Notebook (..),
    Cell (..),
    CellType (..),
    lookupCell,
    cellLangOf,

    -- * Events
    NotebookEvent (..),
    SessionStatus (..),

    -- * Errors
    CellError (..),
) where

import Data.Aeson (FromJSON, ToJSON (..), Value, object, (.=))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Sabela.SessionTypes (CellLang (..))

data OutputItem = OutputItem
    { oiMime :: Text
    , oiOutput :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON OutputItem
instance FromJSON OutputItem

data Notebook = Notebook
    { nbTitle :: Text
    , nbCells :: [Cell]
    }
    deriving (Show, Eq, Generic)

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
    deriving (Show, Eq, Generic)

data CellType = CodeCell | ProseCell
    deriving (Show, Eq, Generic)

instance ToJSON Cell
instance FromJSON Cell
instance ToJSON CellType
instance FromJSON CellType

data NotebookEvent
    = EvCellUpdating Int
    | EvCellPartialOutput Int Text
    | EvCellResult Int [OutputItem] (Maybe Text) [CellError]
    | EvExecutionDone
    | EvSessionStatus SessionStatus
    | EvInstallLog Text
    | -- | turnId, text token
      EvChatTextDelta Int Text
    | -- | turnId, toolCallId, toolName, input
      EvChatToolCall Int Text Text Value
    | -- | turnId, toolCallId, result
      EvChatToolResult Int Text Value
    | -- | turnId, editId, cellId, oldSource, newSource
      EvChatEditProposed Int Int Int Text Text
    | -- | turnId
      EvChatDone Int
    | -- | turnId
      EvChatCancelled Int
    | -- | turnId, error message
      EvChatError Int Text
    deriving (Show)

data SessionStatus
    = SReset
    | SCrashed
    | SUpdateDeps [Text]
    | SDepsUpToDate
    | SStarting
    | SReady
    deriving (Eq)

instance Show SessionStatus where
    show :: SessionStatus -> String
    show SReady = "ready"
    show SReset = "reset"
    show SCrashed = "crashed"
    show (SUpdateDeps deps) = T.unpack ("installing: " <> T.intercalate ", " deps)
    show SStarting = "starting session"
    show SDepsUpToDate = "dependencies up to date"

instance ToJSON NotebookEvent where
    toJSON (EvCellUpdating cid) =
        object ["type" .= ("cellUpdating" :: Text), "cellId" .= cid]
    toJSON (EvCellPartialOutput cid line) =
        object
            ["type" .= ("cellPartialOutput" :: Text), "cellId" .= cid, "line" .= line]
    toJSON (EvCellResult cid outputs err errs) =
        object
            [ "type" .= ("cellResult" :: Text)
            , "cellId" .= cid
            , "outputs" .= outputs
            , "error" .= err
            , "errors" .= errs
            ]
    toJSON EvExecutionDone =
        object ["type" .= ("executionDone" :: Text)]
    toJSON (EvSessionStatus msg) =
        object ["type" .= ("sessionStatus" :: Text), "message" .= show msg]
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

data CellError = CellError
    { ceLine :: Maybe Int
    -- ^ 1-based line within cell
    , ceCol :: Maybe Int
    , ceMessage :: Text
    }
    deriving (Show, Generic, Eq)

instance ToJSON CellError
instance FromJSON CellError

lookupCell :: Int -> Notebook -> Maybe Cell
lookupCell cid nb = find (\c -> cellId c == cid) (nbCells nb)

cellLangOf :: Int -> Notebook -> CellLang
cellLangOf cid nb = maybe Haskell cellLang (lookupCell cid nb)
