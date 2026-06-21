{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, when)
import qualified Data.Text as T
import Hub.Config (loadConfig)
import Hub.Docker (cliDockerOps, dockerBackend)
import Hub.Ecs (cliEcsBackend)
import Hub.Gallery (newGalleryStore)
import Hub.Proxy (hubApp)
import Hub.Reaper (startReaper, sweepOrphans)
import Hub.Republish (republishBanners, republishRunners)
import Hub.Session (newSessionManager, reattachSessions)
import Hub.Share (newShareStore)
import Hub.Types
import Hub.Users (isAnyAdmin, newUserStore)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("republish-banners" : rest) ->
            runRepublish "republish-banners" republishBanners rest
        ("republish-runners" : rest) ->
            runRepublish "republish-runners" republishRunners rest
        _ -> runServer

{- | @sabela-hub \<cmd\> [SHARES_DIR]@: backfill a spliced fragment into existing
snapshots (the fork banner or the WASM runner). The dir defaults to
@$HUB_SHARES_DIR@. Runs standalone — no OAuth config required — so it is safe to
invoke as a one-shot migration.
-}
runRepublish ::
    String -> (FilePath -> IO [(a, Bool)]) -> [String] -> IO ()
runRepublish cmd backfill rest = do
    mdir <- case rest of
        (d : _) -> pure (Just d)
        [] -> lookupEnv "HUB_SHARES_DIR"
    case mdir of
        Nothing -> do
            hPutStrLn
                stderr
                ("usage: sabela-hub " ++ cmd ++ " [SHARES_DIR] (or set HUB_SHARES_DIR)")
            exitFailure
        Just dir -> do
            results <- backfill dir
            let changed = length (filter snd results)
            hPutStrLn stderr $
                "[hub] "
                    ++ cmd
                    ++ ": "
                    ++ show changed
                    ++ " updated, "
                    ++ show (length results - changed)
                    ++ " already current ("
                    ++ show (length results)
                    ++ " shares in "
                    ++ dir
                    ++ ")"

runServer :: IO ()
runServer = do
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
    users <- newUserStore (T.unpack (hcUsersDir cfg)) (hcBootstrapAdmin cfg)
    gallery <- newGalleryStore (T.unpack (hcGalleryDir cfg))
    admins <- isAnyAdmin users
    unless admins $
        hPutStrLn stderr $
            "[hub] WARNING: no admin exists (set HUB_BOOTSTRAP_ADMIN) - "
                ++ "the gallery curation surface is inert"
    app <- hubApp sm store users gallery mgr
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
    case hcAllowlistFile cfg of
        Just path -> do
            readable <- doesFileExist path
            hPutStrLn stderr $ "[hub] Signup allowlist: " ++ path
            unless readable $
                hPutStrLn stderr $
                    "[hub] WARNING: allowlist file missing ("
                        ++ path
                        ++ ") - all logins will be DENIED until it exists"
        Nothing ->
            hPutStrLn stderr $
                "[hub] WARNING: HUB_ALLOWLIST_FILE unset - signup is OPEN; "
                    ++ "anyone with a Google account gets a container"
