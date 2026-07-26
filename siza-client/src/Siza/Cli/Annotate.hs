module Siza.Cli.Annotate (
    runAnnotate,
) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sabela.AI.Capabilities.ToolName (ToolName (CheckType, ReadCell))
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Siza.Annotate (
    AnnotateReport (AnnParseError, AnnReport),
    TypeQuery,
    annotateCell,
    annotatedSource,
    renderReport,
 )
import Siza.Transport (Conn, callTool)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

runAnnotate :: Conn -> Text -> Int -> Bool -> IO ()
runAnnotate conn base cellId asSource = do
    msrc <- readCellSource conn base cellId
    case msrc of
        Left e -> fatal e
        Right src -> do
            report <- annotateCell (sessionTypeQuery conn base) src
            TIO.putStr $
                if asSource
                    then annotatedSource report src
                    else renderReport report
            reportExit report

readCellSource :: Conn -> Text -> Int -> IO (Either Text Text)
readCellSource conn base cellId = do
    res <- callTool conn base ReadCell (object ["cell_id" .= cellId])
    pure $ case res of
        Left e -> Left e
        Right o
            | toolOutcomeIsError o ->
                Left ("read_cell: " <> T.pack (show (toolOutcomeValue o)))
            | otherwise -> case stringField "source" (toolOutcomeValue o) of
                Just s -> Right s
                Nothing -> Left "read_cell returned no source"

sessionTypeQuery :: Conn -> Text -> TypeQuery
sessionTypeQuery conn base name = do
    res <-
        callTool
            conn
            base
            CheckType
            (object ["expr" .= name])
    pure $ case res of
        Left e -> Left e
        Right o
            | toolOutcomeIsError o ->
                Left (errorText (toolOutcomeValue o))
            | otherwise -> case stringField "result" (toolOutcomeValue o) of
                Just r -> Right (stripSig name r)
                Nothing -> Left "no inferred type"

stripSig :: Text -> Text -> Text
stripSig name r =
    let r' = T.strip r
        prefix = name <> " :: "
     in if prefix `T.isPrefixOf` r'
            then T.strip (T.drop (T.length prefix) r')
            else r'

errorText :: Value -> Text
errorText v =
    fromMaybe
        (fromMaybe (T.pack (show v)) (stringField "error" v))
        (stringField "message" v)

stringField :: Text -> Value -> Maybe Text
stringField k = \case
    A.Object o -> case KM.lookup (AK.fromText k) o of
        Just (A.String s) -> Just s
        _ -> Nothing
    _ -> Nothing

reportExit :: AnnotateReport -> IO ()
reportExit = \case
    AnnParseError _ -> exitFailure
    AnnReport _ -> exitSuccess

fatal :: Text -> IO ()
fatal e = hPutStrLn stderr ("siza: " <> T.unpack e) >> exitFailure
