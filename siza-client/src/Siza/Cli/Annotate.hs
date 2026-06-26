{- | The @siza annotate@ runtime: drive 'Siza.Annotate' against a live
session over the transport.

'runAnnotate' reads the cell's source (@read_cell@), then runs the annotate
pipeline with a 'TypeQuery' backed by @check_type {expr}@. The pure
extraction + assembly lives in 'Siza.Annotate'; this module only wires it to
the transport and renders the result, degrading gracefully on a cold session.
-}
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

{- | @siza annotate CELL_ID@: read the cell's source, infer types for its
unsigned top-level binds from the live session, and print the report (or the
annotated source under @--source@). Degrades gracefully — a cold session or
an uninferable bind is surfaced as a comment line, not a failure.
-}
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

{- | Read a cell's @source@ via the @read_cell@ tool. @Left@ on transport
failure, a tool error, or a missing/non-string @source@ field.
-}
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

{- | A 'TypeQuery' backed by the live session: @check_type {expr}@.
@Left@ — a cold session, a transport error, or a name GHCi cannot type —
degrades the bind to an 'AnnFailed' line rather than failing the whole run.
-}
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

{- | GHCi renders @:type@ as @name :: ty@; keep just @ty@ so the report's
own @name ::@ prefix is not doubled. A reply without the prefix is kept
verbatim.
-}
stripSig :: Text -> Text -> Text
stripSig name r =
    let r' = T.strip r
        prefix = name <> " :: "
     in if prefix `T.isPrefixOf` r'
            then T.strip (T.drop (T.length prefix) r')
            else r'

-- | The @message@/@error@ field of an error JSON, or a compact fallback.
errorText :: Value -> Text
errorText v =
    fromMaybe
        (fromMaybe (T.pack (show v)) (stringField "error" v))
        (stringField "message" v)

-- | A string field of a JSON object, if present and a string.
stringField :: Text -> Value -> Maybe Text
stringField k = \case
    A.Object o -> case KM.lookup (AK.fromText k) o of
        Just (A.String s) -> Just s
        _ -> Nothing
    _ -> Nothing

-- | Exit non-zero on a parse error so a broken cell fails the command.
reportExit :: AnnotateReport -> IO ()
reportExit = \case
    AnnParseError _ -> exitFailure
    AnnReport _ -> exitSuccess

fatal :: Text -> IO ()
fatal e = hPutStrLn stderr ("siza: " <> T.unpack e) >> exitFailure
