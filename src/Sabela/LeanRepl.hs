{-# LANGUAGE OverloadedStrings #-}

module Sabela.LeanRepl (
    LeanSession (..),
    ReplResponse (..),
    ReplMessage (..),
    ReplPos (..),
    newLeanSession,
    closeLeanSession,
    sendCommand,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (
    FromJSON (..),
    Value,
    eitherDecode,
    encode,
    withObject,
    (.:),
    (.:?),
 )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode)
import System.IO (
    BufferMode (LineBuffering),
    Handle,
    hClose,
    hFlush,
    hSetBinaryMode,
    hSetBuffering,
 )
import System.Process (
    CreateProcess (cwd, std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
 )

data LeanSession = LeanSession
    { lsProcess :: ProcessHandle
    , lsStdin :: Handle
    , lsStdout :: Handle
    , lsStderr :: Handle
    , lsLock :: MVar ()
    , lsProjectDir :: FilePath
    , lsReplBin :: FilePath
    , lsCurrentEnv :: IORef (Maybe Int)
    }

data ReplResponse = ReplResponse
    { rrEnv :: Int
    , rrMessages :: [ReplMessage]
    , rrSorries :: [Value]
    }
    deriving (Show)

instance FromJSON ReplResponse where
    parseJSON = withObject "ReplResponse" $ \o ->
        ReplResponse
            <$> o .: "env"
            <*> ((o .:? "messages") <&> maybe [] id)
            <*> ((o .:? "sorries") <&> maybe [] id)

data ReplMessage = ReplMessage
    { rmSeverity :: Text
    , rmPos :: ReplPos
    , rmEndPos :: Maybe ReplPos
    , rmData :: Text
    }
    deriving (Show, Eq)

instance FromJSON ReplMessage where
    parseJSON = withObject "ReplMessage" $ \o ->
        ReplMessage
            <$> o .: "severity"
            <*> o .: "pos"
            <*> o .:? "endPos"
            <*> o .: "data"

data ReplPos = ReplPos
    { rpLine :: Int
    , rpColumn :: Int
    }
    deriving (Show, Eq)

instance FromJSON ReplPos where
    parseJSON = withObject "ReplPos" $ \o ->
        ReplPos
            <$> o .: "line"
            <*> o .: "column"

{- | Create a new Lean REPL session.
Spawns @lake env \<replBin\>@ in the given project directory.
-}
newLeanSession :: FilePath -> FilePath -> IO LeanSession
newLeanSession projDir replBin = do
    let cp =
            (proc "lake" ["env", replBin])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = Just projDir
                }
    (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
    hSetBinaryMode hIn False
    hSetBinaryMode hOut False
    hSetBuffering hIn LineBuffering
    hSetBuffering hOut LineBuffering
    hSetBuffering hErr LineBuffering
    lock <- newMVar ()
    envRef <- newIORef Nothing
    pure
        LeanSession
            { lsProcess = ph
            , lsStdin = hIn
            , lsStdout = hOut
            , lsStderr = hErr
            , lsLock = lock
            , lsProjectDir = projDir
            , lsReplBin = replBin
            , lsCurrentEnv = envRef
            }

closeLeanSession :: LeanSession -> IO ()
closeLeanSession sess = do
    _ <- try (hClose (lsStdin sess)) :: IO (Either SomeException ())
    _ <- try (terminateProcess (lsProcess sess)) :: IO (Either SomeException ())
    void
        (try (waitForProcess (lsProcess sess)) :: IO (Either SomeException ExitCode))

{- | Send a command to the REPL and read back the response.
The lock ensures only one command runs at a time.
-}
sendCommand :: LeanSession -> Text -> Maybe Int -> IO ReplResponse
sendCommand sess cmd mEnv = withMVar (lsLock sess) $ \_ -> do
    let jsonObj = case mEnv of
            Nothing -> "{\"cmd\": " <> encodeText cmd <> "}"
            Just env ->
                "{\"cmd\": "
                    <> encodeText cmd
                    <> ", \"env\": "
                    <> T.pack (show env)
                    <> "}"
    -- Write command followed by blank line
    let bs = LBS8.pack (T.unpack jsonObj)
    LBS.hPut (lsStdin sess) bs
    LBS.hPut (lsStdin sess) "\n\n"
    hFlush (lsStdin sess)
    -- Read response lines until blank line
    responseBytes <- readUntilBlank (lsStdout sess)
    case eitherDecode responseBytes of
        Left err ->
            error $
                "LeanRepl: failed to decode response: "
                    ++ err
                    ++ "\nResponse was: "
                    ++ show responseBytes
        Right resp -> pure resp

-- | Read lines from a handle until a blank line is encountered.
readUntilBlank :: Handle -> IO LBS.ByteString
readUntilBlank h = go []
  where
    go acc = do
        line <- readLineLBS h
        if LBS.null (LBS8.dropWhileEnd (\c -> c == '\r' || c == '\n') line)
            then pure (LBS8.unlines (reverse acc))
            else go (line : acc)

-- | Read a single line from a handle (up to newline).
readLineLBS :: Handle -> IO LBS.ByteString
readLineLBS h = go []
  where
    go acc = do
        b <- LBS.hGet h 1
        if LBS.null b || b == "\n"
            then pure (LBS8.pack (reverse acc))
            else go (LBS8.head b : acc)

-- | Encode a Text value as a JSON string (with escaping).
encodeText :: Text -> Text
encodeText t =
    let encoded = encode t -- produces a JSON string with quotes
     in T.pack (LBS8.unpack encoded)
