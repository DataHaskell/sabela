{- | The CLI adapter for @siza retro@: read a session JSONL log, fold it with
'Siza.Retro', and print the metrics as JSON (redesign 7.5).

The log argument is either an explicit file path, or a @notebook/session@ pair
resolved under the same @sessions/@ tree 'Siza.Provenance' writes.
-}
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

{- | What @siza retro@ reads: an explicit log path, or a @(notebook, session)@
pair resolved under the @sessions/@ tree.
-}
type RetroTarget = Either FilePath (Text, Text)

{- | @siza retro FILE@, or @siza retro --notebook N --session S@. An explicit
path wins; otherwise the notebook/session pair resolves under the tree.
-}
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

{- | Resolve the log (an explicit path, or a notebook/session pair under the
@sessions/@ tree), decode its events, and print the metrics. Exits non-zero
when the log is missing.
-}
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
