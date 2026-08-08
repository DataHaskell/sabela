{-# LANGUAGE OverloadedStrings #-}

{- | Serves a sampled dataset to the Data panel. The sniffing, header
detection and column typing are the ones the AI @peek_data@ tool already
uses; only the wire shape is this module's own.
-}
module Sabela.Server.Dataset (
    datasetPreviewH,
    defaultPreviewRows,
    maxPreviewRows,
) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Servant (Handler)
import System.Directory (getFileSize)

import Sabela.AI.Artefact (Artefact (..), atRowLimit)
import Sabela.AI.Files (ReadError (..), readLocal, resolveInWorkDir)
import Sabela.AI.PeekData (
    DelimitedView (..),
    PeekColumn (..),
    PeekResult (..),
    PeekVerdict (..),
    colTypeName,
 )
import Sabela.Api
import Sabela.Parquet (ParquetColumn (..), ParquetSchema (..))
import Sabela.Parquet.Read (isParquetPath, readParquetSchema)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))

defaultPreviewRows :: Int
defaultPreviewRows = 20

maxPreviewRows :: Int
maxPreviewRows = 200

datasetPreviewH :: App -> Maybe Text -> Maybe Int -> Handler DatasetPreview
datasetPreviewH app mPath mRows = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        path = fromMaybe "" mPath
        rows = min maxPreviewRows (max 1 (fromMaybe defaultPreviewRows mRows))
    resolved <- resolveInWorkDir workDir path
    case resolved of
        Left e -> pure (refusal path e)
        Right abs'
            | isParquetPath abs' -> parquetPreview path abs'
            | otherwise ->
                either (refusal path) (preview path . atRowLimit rows)
                    <$> readLocal workDir path

{- | Parquet answers from its own footer, which is uncompressed Thrift at the
end of the file. The byte sniffer cannot read a columnar format and would
only report that it is not text.
-}
parquetPreview :: Text -> FilePath -> IO DatasetPreview
parquetPreview path abs' = do
    size <- fileSizeOf abs'
    got <- readParquetSchema abs'
    pure $ case got of
        Left e -> (emptyPreview path){dpReason = Just e, dpBytes = size}
        Right s ->
            (emptyPreview path)
                { dpDelimited = True
                , dpHasHeader = True
                , dpColumns = zipWith schemaColumn [0 ..] (pqColumns s)
                , dpRowCount = pqRowCount s
                , dpBytes = size
                }

schemaColumn :: Int -> ParquetColumn -> DatasetColumn
schemaColumn i c = DatasetColumn i (Just (pqName c)) (pqType c)

fileSizeOf :: FilePath -> IO Integer
fileSizeOf p = either (const 0) id <$> try' (getFileSize p)
  where
    try' :: IO a -> IO (Either IOException a)
    try' = try

{- | The wording a refused read is told in, matching how the @read_file@ tool
refuses so the panel and the model say the same thing about the same path.
-}
readErrorReason :: ReadError -> Text
readErrorReason OutsideWorkDir = "outside the workspace"
readErrorReason NotFound = "no such file"
readErrorReason IsDirectory = "that is a directory"
readErrorReason NotReadable = "could not be read"

refusal :: Text -> ReadError -> DatasetPreview
refusal path e = (emptyPreview path){dpReason = Just (readErrorReason e)}

emptyPreview :: Text -> DatasetPreview
emptyPreview path =
    DatasetPreview
        { dpPath = path
        , dpDelimited = False
        , dpReason = Nothing
        , dpDelimiter = Nothing
        , dpHasHeader = False
        , dpColumns = []
        , dpRows = []
        , dpRowCount = 0
        , dpLineCount = 0
        , dpTruncated = False
        , dpBytes = 0
        }

preview :: Text -> Artefact -> DatasetPreview
preview path a =
    (verdictPreview path (peekVerdict peeked))
        { dpLineCount = peekLineCount peeked
        , dpTruncated = arTruncated a
        , dpBytes = arBytes a
        }
  where
    peeked = arPeek a

verdictPreview :: Text -> PeekVerdict -> DatasetPreview
verdictPreview path (NotDelimited reason) =
    (emptyPreview path){dpReason = Just reason}
verdictPreview path (Delimited v) =
    (emptyPreview path)
        { dpDelimited = True
        , dpDelimiter = Just (dvDelimiter v)
        , dpHasHeader = dvHasHeader v
        , dpColumns = map column (dvColumns v)
        , dpRows = dvRows v
        , dpRowCount = dvRowCount v
        }

column :: PeekColumn -> DatasetColumn
column c = DatasetColumn (pcIndex c) (pcName c) (colTypeName (pcType c))
