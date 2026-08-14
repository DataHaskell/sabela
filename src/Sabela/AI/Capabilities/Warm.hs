{- | Kernel pre-warming for tools that need a session before they answer:
a no-op when a session already exists, and skipped for try-alias tools.
-}
module Sabela.AI.Capabilities.Warm (warmKernel) where

import Control.Monad (void)
import Data.IORef (readIORef)

import Sabela.Handlers.Lifecycle (ensureSessionAlive)
import Sabela.State
import ScriptHs.Parser (CabalMeta (..))

warmKernel :: App -> IO ()
warmKernel app = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Just _ -> pure ()
        Nothing -> do
            gen <- readIORef (ebGeneration (appEvents app))
            void (ensureSessionAlive app gen emptyMeta)
  where
    emptyMeta =
        CabalMeta
            { metaDeps = []
            , metaExts = []
            , metaGhcOptions = []
            , metaExtraLibDirs = []
            , metaExtraIncludeDirs = []
            , metaPackages = []
            , metaSourceRepos = []
            , metaUnknownKeys = []
            }
