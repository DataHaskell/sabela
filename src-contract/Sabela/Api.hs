{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Api where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Types as A
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Sabela.Model (CellError, CellType, OutputItem)
import Sabela.SessionTypes (CellLang)

-- ---------------------------------------------------------------------
-- Error-response helpers
-- ---------------------------------------------------------------------

{- | Build a tool/HTTP error payload: @{"error": <msg>}@. Used wherever a
handler or tool returns a JSON object whose only field is @error@. One
shared helper keeps the wire shape consistent across the server and
'Sabela.AI.Capabilities'.
-}
errorJson :: Text -> Value
errorJson msg = object ["error" .= msg]

{- | Like 'errorJson' but with extra fields appended after @error@. The
helper preserves insertion order, so @errorJsonWith \"…\" [\"cellId\" .=
123]@ renders as @{"error":\"…\",\"cellId\":123}@.
-}
errorJsonWith :: Text -> [Pair] -> Value
errorJsonWith msg extras = object (("error" .= msg) : extras)

data WidgetUpdate = WidgetUpdate
    { wuCellId :: Int
    , wuName :: Text
    , wuValue :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON WidgetUpdate
instance FromJSON WidgetUpdate

newtype UpdateCell = UpdateCell
    {ucSource :: Text}
    deriving (Eq, Generic, Show)

instance ToJSON UpdateCell
instance FromJSON UpdateCell

{- | Where to insert a new cell. The wire format is a single integer:
@-1@ means "at the beginning of the notebook"; any non-negative value
identifies the cell to insert after. Negative values other than @-1@ are
rejected at parse time — the previous @Int@-typed field silently treated
them as "append at end", which is a clean illegal-states-unrepresentable
fix.
-}
data InsertAt = AtBeginning | After !Int
    deriving (Eq, Generic, Show)

instance ToJSON InsertAt where
    toJSON AtBeginning = toJSON (-1 :: Int)
    toJSON (After n) = toJSON n

instance FromJSON InsertAt where
    parseJSON v = do
        n <- parseJSON v :: A.Parser Int
        case n of
            (-1) -> pure AtBeginning
            k | k >= 0 -> pure (After k)
            _ ->
                fail
                    "after_cell_id must be -1 (at beginning) or a non-negative cell id"

data InsertCell = InsertCell
    { icAfter :: InsertAt
    , icType :: CellType
    , icLang :: CellLang
    , icSource :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON InsertCell
instance FromJSON InsertCell

newtype LoadRequest = LoadRequest
    { lrPath :: FilePath
    }
    deriving (Eq, Generic, Show)

instance ToJSON LoadRequest
instance FromJSON LoadRequest

newtype SaveRequest = SaveRequest
    { srPath :: Maybe FilePath
    -- ^ Nothing = save to nbTitle
    }
    deriving (Eq, Generic, Show)

instance ToJSON SaveRequest
instance FromJSON SaveRequest

{- | Sum type for the @POST /api/files/create@ payload. Directories
don't carry content; making the two cases distinct constructors removes
the @{isDir: true, content: "hello"}@ silent-discard footgun the old
record shape allowed.
-}
data CreateFileRequest
    = CreateDir !Text
    | CreateFile !Text !Text
    deriving (Eq, Generic, Show)

instance ToJSON CreateFileRequest where
    toJSON (CreateDir p) =
        object ["cfPath" .= p, "cfContent" .= ("" :: Text), "cfIsDir" .= True]
    toJSON (CreateFile p c) =
        object ["cfPath" .= p, "cfContent" .= c, "cfIsDir" .= False]

instance FromJSON CreateFileRequest where
    parseJSON = A.withObject "CreateFileRequest" $ \o -> do
        p <- o A..: "cfPath"
        isDir <- o A..:? "cfIsDir" A..!= False
        content <- o A..:? "cfContent" A..!= ("" :: Text)
        if isDir
            then
                if T.null content
                    then pure (CreateDir p)
                    else fail "cfIsDir=true forbids non-empty cfContent"
            else pure (CreateFile p content)

data WriteFileRequest = WriteFileRequest
    { wfPath :: Text
    , wfContent :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON WriteFileRequest
instance FromJSON WriteFileRequest

newtype DeleteFileRequest = DeleteFileRequest
    { dfPath :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON DeleteFileRequest
instance FromJSON DeleteFileRequest

data RenameFileRequest = RenameFileRequest
    { rfOldPath :: Text
    , rfNewPath :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON RenameFileRequest
instance FromJSON RenameFileRequest

{- | A bounded window of a file's bytes for @GET /api/file/preview@. The
frontend pulls one window at a time so a multi-gigabyte file never loads
whole; @fpEof@ says whether @fpOffset + fpReturned@ reached the end.
-}
data FilePreview = FilePreview
    { fpContent :: Text
    , fpOffset :: Int
    , fpReturned :: Int
    , fpTotalBytes :: Int
    , fpEof :: Bool
    }
    deriving (Eq, Generic, Show)

instance ToJSON FilePreview
instance FromJSON FilePreview

newtype CompleteRequest = CompleteRequest
    { crPrefix :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON CompleteRequest
instance FromJSON CompleteRequest

newtype InfoRequest = InfoRequest
    { irName :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON InfoRequest
instance FromJSON InfoRequest

data RunResult = RunResult
    { rrCellId :: Int
    , rrOutputs :: [OutputItem]
    , rrError :: Maybe Text
    , rrWarnings :: [CellError]
    }
    deriving (Generic, Show)

instance ToJSON RunResult
instance FromJSON RunResult

newtype RunAllResult = RunAllResult
    { rarResults :: [RunResult]
    }
    deriving (Generic, Show)

instance ToJSON RunAllResult
instance FromJSON RunAllResult

newtype CompleteResult = CompleteResult
    { crCompletions :: [Text]
    }
    deriving (Generic, Show)

instance ToJSON CompleteResult
instance FromJSON CompleteResult

newtype InfoResult = InfoResult
    { irText :: Text
    }
    deriving (Generic, Show)

instance ToJSON InfoResult
instance FromJSON InfoResult

data FileEntry = FileEntry
    { feName :: Text
    , fePath :: Text
    , feIsDir :: Bool
    }
    deriving (Generic, Show)

instance ToJSON FileEntry
instance FromJSON FileEntry

data Example = Example
    { exTitle :: Text
    , exDesc :: Text
    , exCategory :: Text
    , exCode :: Text
    }
    deriving (Generic, Show)

instance ToJSON Example
instance FromJSON Example

newtype ChatRequest = ChatRequest
    { crMessage :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON ChatRequest
instance FromJSON ChatRequest
