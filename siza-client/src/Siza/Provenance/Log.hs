{- | The write side of the client-seam provenance log: the append-only JSONL
file, the path resolution, and the opt-in hash chain (redesign 7.3/7.4).

'Siza.Provenance' owns the 'SessionEvent' record and its wire encoding; this
module is the IO around it. The default log is plain append-only; the hash
chain is opt-in for a security-sensitive deployment such as the hub.
-}
module Siza.Provenance.Log (
    sessionLogPath,
    recordEvent,
    appendEvent,
    eventHash,
    chainEvents,
    verifyChain,
) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Provenance (stateBase)
import Siza.Provenance.Event (SessionEvent (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (
    BufferMode (LineBuffering),
    IOMode (AppendMode),
    hSetBuffering,
    withFile,
 )

-- ---------------------------------------------------------------------------
-- Tamper-evident hash chain (opt-in; redesign 7.3)
-- ---------------------------------------------------------------------------

{- | The SHA-256 of an event's canonical JSON encoding, lower-case hex. Because
the encoding includes @sePrev@, hashing a record commits to its predecessor's
hash too, so the digest covers the whole prefix of the chain.
-}
eventHash :: SessionEvent -> Text
eventHash ev =
    T.pack (show (hash (LBS.toStrict (A.encode ev)) :: Digest SHA256))

{- | Link a sequence into a hash chain: each event's @sePrev@ becomes the
'eventHash' of the one before it; the first stays 'Nothing'. The default log is
plain append-only ('sePrev' = 'Nothing'); a security-sensitive deployment opts
in by chaining before writing.
-}
chainEvents :: [SessionEvent] -> [SessionEvent]
chainEvents = go Nothing
  where
    go _ [] = []
    go prev (e : es) =
        let linked = e{sePrev = prev}
         in linked : go (Just (eventHash linked)) es

{- | Verify a hash chain: every @sePrev@ must equal the predecessor's
'eventHash', and the first must be 'Nothing'. A deleted or edited record breaks
the link and fails. An empty or single-event chain trivially holds.
-}
verifyChain :: [SessionEvent] -> Bool
verifyChain = go Nothing
  where
    go _ [] = True
    go prev (e : es) = sePrev e == prev && go (Just (eventHash e)) es

-- ---------------------------------------------------------------------------
-- Append-only JSONL log
-- ---------------------------------------------------------------------------

{- | The per-session log path under
@${XDG_STATE_HOME:-~/.local/state}/sabela/sessions/<notebook>/<session>.jsonl@,
the exact base-dir resolution 'Siza.Discover' uses for the server registry.
-}
sessionLogPath :: Text -> Text -> IO FilePath
sessionLogPath notebook session = do
    base <- stateBase
    pure
        ( base
            </> "sabela"
            </> "sessions"
            </> sanitise notebook
            </> (sanitise session <> ".jsonl")
        )

-- | Keep a notebook/session id a single safe path segment.
sanitise :: Text -> FilePath
sanitise = T.unpack . T.map repl . T.dropWhile (== '.')
  where
    repl c = if c `elem` ("/\\\NUL" :: String) then '_' else c

{- | Append one event to the resolved per-session log, creating the directory
tree as needed. Best-effort: any exception is swallowed via 'try' so a log
write can never fail the tool call.
-}
recordEvent :: SessionEvent -> IO ()
recordEvent ev = void (try go :: IO (Either SomeException ()))
  where
    go = do
        path <- sessionLogPath (seNotebook ev) (seSession ev)
        appendEvent path ev

-- | Append a single JSONL line (the encoded event + newline) to @path@.
appendEvent :: FilePath -> SessionEvent -> IO ()
appendEvent path ev = do
    createDirectoryIfMissing True (takeDirectory path)
    withFile path AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        LBS.hPut h (A.encode ev <> "\n")
