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

import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
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
    deriving (Show, Eq)

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
