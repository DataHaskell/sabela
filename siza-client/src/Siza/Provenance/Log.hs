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

eventHash :: SessionEvent -> Text
eventHash ev =
    T.pack (show (hash (LBS.toStrict (A.encode ev)) :: Digest SHA256))

chainEvents :: [SessionEvent] -> [SessionEvent]
chainEvents = go Nothing
  where
    go _ [] = []
    go prev (e : es) =
        let linked = e{sePrev = prev}
         in linked : go (Just (eventHash linked)) es

verifyChain :: [SessionEvent] -> Bool
verifyChain = go Nothing
  where
    go _ [] = True
    go prev (e : es) = sePrev e == prev && go (Just (eventHash e)) es

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

sanitise :: Text -> FilePath
sanitise = T.unpack . T.map repl . T.dropWhile (== '.')
  where
    repl c = if c `elem` ("/\\\NUL" :: String) then '_' else c

recordEvent :: SessionEvent -> IO ()
recordEvent ev = void (try go :: IO (Either SomeException ()))
  where
    go = do
        path <- sessionLogPath (seNotebook ev) (seSession ev)
        appendEvent path ev

appendEvent :: FilePath -> SessionEvent -> IO ()
appendEvent path ev = do
    createDirectoryIfMissing True (takeDirectory path)
    withFile path AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        LBS.hPut h (A.encode ev <> "\n")
