{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Hub.Config (loadConfig)
import Hub.Ecs (cliEcsBackend)
import Hub.Proxy (hubApp)
import Hub.Reaper (startReaper)
import Hub.Session (newSessionManager)
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
    sm <- newSessionManager cliEcsBackend cfg
    startReaper sm
    hPutStrLn stderr $
        "[hub] Starting on port "
            ++ show (hcPort cfg)
            ++ " with Google OAuth"
    app <- hubApp sm mgr
    Warp.run (hcPort cfg) app

validateConfig :: HubConfig -> IO ()
validateConfig cfg = do
    if T.null (hcGoogleClientId cfg)
        then error "GOOGLE_CLIENT_ID is required"
        else pure ()
    if T.null (hcGoogleClientSecret cfg)
        then error "GOOGLE_CLIENT_SECRET is required"
        else pure ()
    if null (tcSubnets (hcTaskConfig cfg))
        then error "HUB_ECS_SUBNETS is required"
        else pure ()
    if null (tcSecurityGroups (hcTaskConfig cfg))
        then error "HUB_ECS_SECURITY_GROUPS is required"
        else pure ()
    hPutStrLn stderr $
        "[hub] OAuth redirect: "
            ++ T.unpack (hcGoogleRedirectUri cfg)
