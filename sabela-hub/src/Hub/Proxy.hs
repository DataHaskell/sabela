{-# LANGUAGE OverloadedStrings #-}

{- | Top-level WAI router for the hub: splits incoming requests between the
public-share static serve ('Hub.Shares.Api'), the authed share JSON endpoints
('Hub.Shares.Api'), the auth/OAuth flow ('Hub.Auth'), and the per-user reverse
proxy ('Hub.Proxy.Forward').
-}
module Hub.Proxy (
    hubApp,
) where

import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import Hub.Auth (
    PendingStates,
    extractSessionId,
    handleLogin,
    handleOAuthCallback,
    logoutResponse,
    requireSession,
 )
import Hub.Pages (loginPage, startingPage, textResponse)
import Hub.Proxy.Forward (proxyWithRetry)
import Hub.Session (
    SessionManager (..),
    lookupBySessionId,
 )
import Hub.Share (ShareStore)
import Hub.Shares.Api (
    handleDeleteShare,
    handleListShares,
    handlePublish,
    serveShare,
 )
import Hub.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai

-- | Create the WAI application. Call once at startup.
hubApp :: SessionManager -> ShareStore -> HC.Manager -> IO Application
hubApp sm store mgr = do
    states <- newTVarIO Map.empty
    pure $ hubApp' sm store mgr states

{- | Top-level routing. Phase 3a share routes match first on the split
'pathInfo'; everything else (auth, health, and the authed proxy) falls through
to 'hubDispatch' unchanged.
-}
hubApp' ::
    SessionManager -> ShareStore -> HC.Manager -> PendingStates -> Application
hubApp' sm store mgr states req respond =
    case pathInfo req of
        -- Public static share (Phase 3a): no auth, no backend — a file serve.
        ["s", slug] -> serveShare store slug respond
        -- Authed share management; the owner is taken from the session cookie,
        -- never from the request body.
        ["_hub", "publish"] ->
            requireSession sm req respond $ \sess ->
                handlePublish sm store mgr sess req respond
        ["_hub", "shares"] ->
            requireSession sm req respond $ \sess ->
                handleListShares store sess respond
        ["_hub", "shares", slug]
            | requestMethod req == methodDelete ->
                requireSession sm req respond $ \sess ->
                    handleDeleteShare store sess slug respond
        _ -> hubDispatch sm mgr states req respond

hubDispatch :: SessionManager -> HC.Manager -> PendingStates -> Application
hubDispatch sm mgr states req respond =
    let path = rawPathInfo req
        cfg = smConfig sm
     in case path of
            "/_hub/health" ->
                respond $ textResponse status200 "ok"
            "/_hub/login" ->
                handleLogin states cfg req respond
            "/_hub/oauth/callback" ->
                handleOAuthCallback sm mgr states cfg req respond
            "/_hub/logout" ->
                respond (logoutResponse req)
            _ ->
                case extractSessionId req of
                    Nothing ->
                        respond loginPage
                    Just sid -> do
                        mSess <- lookupBySessionId sm sid
                        case mSess of
                            Nothing ->
                                respond loginPage
                            Just sess ->
                                case sessionState sess of
                                    SReady ip ->
                                        proxyWithRetry mgr (hcBackendPort cfg) ip req respond
                                    SStarting ->
                                        respond startingPage
                                    SStopping ->
                                        respond loginPage
