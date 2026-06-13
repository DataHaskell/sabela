{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Sabela.Model (
    -- * Output
    OutputItem (..),
    MimeType (..),
    mimeIndicator,
    textToMime,

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
import Sabela.SessionTypes (CellLang (..))
import ScriptHs.Markdown (MimeType (..))

data OutputItem = OutputItem
    { oiMime :: MimeType
    , oiOutput :: Text
    }
    deriving (Show, Eq, Generic)

-- | Wire-format MIME label. Round-trips with 'textToMime'.
mimeIndicator :: MimeType -> Text
mimeIndicator m = case m of
    MimeHtml -> "text/html"
    MimeMarkdown -> "text/markdown"
    MimeSvg -> "image/svg+xml"
    MimeLatex -> "text/latex"
    MimeJson -> "application/json"
    MimeImage t -> t <> ";base64"
    MimePlain -> "text/plain"

-- | Inverse of 'mimeIndicator'. Anything unrecognised maps to 'MimePlain'.
textToMime :: Text -> MimeType
textToMime m
    -- @"<type>;base64"@ round-trips back to 'MimeImage' so an image
    -- output isn't silently saved as plain text.
    | Just t <- T.stripSuffix ";base64" m = MimeImage t
    | otherwise = case m of
        "text/html" -> MimeHtml
        "text/markdown" -> MimeMarkdown
        "image/svg+xml" -> MimeSvg
        "text/latex" -> MimeLatex
        "application/json" -> MimeJson
        _ -> MimePlain

-- Hand-rolled instances preserve the wire shape: @oiMime@ goes out as
-- the indicator string (e.g. @"text/html"@) regardless of which 'MimeType'
-- constructor it came from, so the frontend continues to see strings.
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
    | -- | A compiled cell's module is being (re)compiled.
      EvCellCompiling Int
    | EvCellPartialOutput Int Text
    | EvCellResult Int [OutputItem] (Maybe Text) [CellError]
    | EvExecutionDone
    | EvSessionStatus SessionStatus
    | EvInstallLog Text
    | -- | turnId, text token
      EvChatTextDelta TurnId Text
    | -- | turnId, toolCallId, toolName, input
      EvChatToolCall TurnId ToolCallId Text Value
    | -- | turnId, toolCallId, result
      EvChatToolResult TurnId ToolCallId Value
    | {- | 'Nothing' is an edit proposed outside any turn (e.g. via the
      REST tool bridge); previously this was the magic sentinel @TurnId 0@.
      -}
      EvChatEditProposed (Maybe TurnId) EditId Int Text Text
    | -- | turnId
      EvChatDone TurnId
    | -- | turnId
      EvChatCancelled TurnId
    | {- | 'Nothing' is a global error not bound to any turn (e.g. "AI not
      configured" or "A turn is already in progress"); previously this
      was the magic sentinel @0@.
      -}
      EvChatError (Maybe TurnId) Text
    | {- | Full notebook snapshot — fired whenever cells are inserted, deleted,
      reordered, or edited outside the reactive execute path, so the frontend
      can refresh its view without re-fetching.
      -}
      EvNotebookChanged Notebook
    | {- | Per-turn token accounting: {turnId, inputTokens, outputTokens,
      cacheCreationInputTokens, cacheReadInputTokens, wallTimeMs,
      iterations}. Emitted once when a turn completes.
      -}
      EvChatUsageUpdate TurnId Value
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
    toJSON (EvCellCompiling cid) =
        object ["type" .= ("cellCompiling" :: Text), "cellId" .= cid]
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
    toJSON (EvNotebookChanged nb) =
        object
            [ "type" .= ("notebookChanged" :: Text)
            , "notebook" .= nb
            ]
    toJSON (EvChatUsageUpdate tid payload) =
        object
            [ "type" .= ("chatUsageUpdate" :: Text)
            , "turnId" .= tid
            , "usage" .= payload
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
