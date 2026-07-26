module Siza.Cli.Retro (
    RetroTarget,
    retroTargetParser,
    runRetro,
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import Options.Applicative
import Siza.Provenance (sessionLogPath)
import Siza.Retro (computeMetrics, decodeSession, metricsValue)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

type RetroTarget = Either FilePath (Text, Text)

retroTargetParser :: Parser RetroTarget
retroTargetParser = byFile <|> byPair
  where
    byFile =
        Left
            <$> argument str (metavar "FILE" <> help "Path to a session .jsonl log")
    byPair =
        fmap Right $
            (,)
                <$> strOption (long "notebook" <> metavar "NB" <> help "Notebook id")
                <*> strOption (long "session" <> metavar "SID" <> help "Session id")

runRetro :: RetroTarget -> IO ()
runRetro target = do
    path <- either pure (uncurry sessionLogPath) target
    ok <- doesFileExist path
    if not ok
        then do
            hPutStrLn stderr ("siza: retro: no such session log: " <> path)
            exitFailure
        else do
            raw <- LBS8.readFile path
            LBS8.putStrLn (A.encode (metricsValue (computeMetrics (decodeSession raw))))
