module Siza.Cli.Retro (
    RetroTarget (..),
    retroTargetParser,
    runRetro,
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative
import Siza.Provenance (sessionLogPath)
import Siza.Retro (computeMetrics, decodeSession, metricsValue)
import Siza.Retro.Metrics (metricsFromText)
import Siza.Retro.Report (transcriptMetricsValue)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

{- | What to read the metrics from: a server-side session log, the notebook
and session that names one, or a rendered episode from either surface.
-}
data RetroTarget
    = FromLog FilePath
    | FromSession Text Text
    | FromTranscript FilePath
    deriving (Show)

retroTargetParser :: Parser RetroTarget
retroTargetParser = byTranscript <|> byFile <|> byPair
  where
    byTranscript =
        FromTranscript
            <$> strOption
                ( long "transcript"
                    <> metavar "FILE"
                    <> help "Path to a rendered episode markdown"
                )
    byFile =
        FromLog
            <$> argument str (metavar "FILE" <> help "Path to a session .jsonl log")
    byPair =
        FromSession
            <$> strOption (long "notebook" <> metavar "NB" <> help "Notebook id")
            <*> strOption (long "session" <> metavar "SID" <> help "Session id")

runRetro :: RetroTarget -> IO ()
runRetro (FromTranscript path) = withExisting path transcriptReport
runRetro (FromLog path) = withExisting path sessionReport
runRetro (FromSession nb sid) = do
    path <- sessionLogPath nb sid
    withExisting path sessionReport

transcriptReport :: FilePath -> IO ()
transcriptReport path = do
    body <- TIO.readFile path
    LBS8.putStrLn (A.encode (transcriptMetricsValue (metricsFromText body)))

sessionReport :: FilePath -> IO ()
sessionReport path = do
    raw <- LBS8.readFile path
    LBS8.putStrLn (A.encode (metricsValue (computeMetrics (decodeSession raw))))

withExisting :: FilePath -> (FilePath -> IO ()) -> IO ()
withExisting path act = do
    ok <- doesFileExist path
    if ok
        then act path
        else do
            hPutStrLn stderr ("siza: retro: no such file: " <> path)
            exitFailure
