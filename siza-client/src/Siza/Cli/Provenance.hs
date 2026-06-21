{- | The client seam's tie-in to the provenance log: build a 'SessionEvent'
for a tool call and append it (redesign Part 7).

This is the CLI's adapter onto 'Siza.Provenance'. It is the only seam that
records the pre-flight verdict the server never sees. @seKernelBefore@ and
@seGen@ are best-effort here ('Cold'/0) — the server seam is authoritative for
the real kernel state and the @ebGeneration@ fence (redesign 7.2).
-}
module Siza.Cli.Provenance (
    logToolCall,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Sabela.AI.Capabilities.ToolName (ToolName)
import Sabela.AI.KernelState (KernelState (Cold))
import Sabela.AI.Types (ToolOutcome (ToolErr))
import Siza.Discover (Server (..))
import Siza.Provenance (
    Actor (Agent),
    Preflight,
    SessionEvent (..),
    recordEvent,
 )
import Siza.Transport (Conn (..), Env (envSession))
import System.FilePath (takeFileName)

{- | Append the client-seam provenance record for a tool call: the typed
'SessionEvent' with the pre-flight verdict. Best-effort via 'recordEvent', so
a log failure can never fail the call. A transport error is recorded as a
'ToolErr' so the log holds an outcome for every attempt.
-}
logToolCall ::
    Conn ->
    Server ->
    ToolName ->
    Value ->
    Maybe Preflight ->
    Either Text ToolOutcome ->
    IO ()
logToolCall conn srv name input mpf res = do
    now <- getCurrentTime
    let outcome = either (ToolErr . transportErr) id res
    recordEvent
        SessionEvent
            { seAt = now
            , seSession = envSession (connEnv conn)
            , seNotebook = notebookOf srv
            , seActor = Agent
            , seCall = name
            , seInput = input
            , sePreflight = mpf
            , seOutcome = outcome
            , seKernelBefore = Cold
            , seGen = 0
            , sePrev = Nothing
            }
  where
    transportErr e = object ["error" .= e]

-- | The notebook id: the basename of the chosen server's work dir.
notebookOf :: Server -> Text
notebookOf srv =
    case srvWorkDir srv of
        Just wd | not (T.null wd) -> T.pack (takeFileName (T.unpack wd))
        _ -> "unknown"
