{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import qualified Data.Text as T
import Hub.Config (loadConfig)
import Hub.Docker (cliDockerOps, dockerBackend)
import Hub.Ecs (cliEcsBackend)
import Hub.Proxy (hubApp)
import Hub.Reaper (startReaper, sweepOrphans)
import Hub.Session (newSessionManager, reattachSessions)
import Hub.Share (newShareStore)
import Hub.Types
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.Wai.Handler.Warp as Warp
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    cfg <- loadConfig
    validateConfig cfg
    mgr <- HC.newManager TLS.tlsManagerSettings
    backend <- case hcBackend cfg of
        BackendDocker -> dockerBackend (hcDockerConfig cfg) cliDockerOps
        BackendEcs -> pure cliEcsBackend
    sm <- newSessionManager backend cfg
    case hcBackend cfg of
        BackendDocker -> reattachSessions sm
        BackendEcs -> sweepOrphans sm
    startReaper sm
    hPutStrLn stderr $
        "[hub] Starting on port "
            ++ show (hcPort cfg)
            ++ " with Google OAuth"
    store <- newShareStore (T.unpack (hcSharesDir cfg))
    app <- hubApp sm store mgr
    Warp.run (hcPort cfg) app

validateConfig :: HubConfig -> IO ()
validateConfig cfg = do
    when (T.null (hcGoogleClientId cfg)) $
        error "GOOGLE_CLIENT_ID is required"
    when (T.null (hcGoogleClientSecret cfg)) $
        error "GOOGLE_CLIENT_SECRET is required"
    case hcBackend cfg of
        BackendEcs -> do
            when (null (tcSubnets (hcTaskConfig cfg))) $
                error "HUB_ECS_SUBNETS is required"
            when (null (tcSecurityGroups (hcTaskConfig cfg))) $
                error "HUB_ECS_SECURITY_GROUPS is required"
        BackendDocker -> pure ()
    hPutStrLn stderr $ "[hub] Backend: " ++ show (hcBackend cfg)
    hPutStrLn stderr $
        "[hub] OAuth redirect: "
            ++ T.unpack (hcGoogleRedirectUri cfg)
