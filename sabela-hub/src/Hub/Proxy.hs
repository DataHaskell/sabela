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
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hub.Admin.Api (adminDispatch, requireAdmin)
import Hub.Admin.Page (adminPage)
import Hub.Auth (
    PendingStates,
    extractSessionId,
    handleLogin,
    handleOAuthCallback,
    logoutResponse,
    requireSession,
 )
import Hub.Fork (serveFork)
import Hub.Gallery (GalleryStore)
import Hub.Gallery.Public (
    serveCollection,
    serveCollectionReader,
    serveFeed,
    serveGallery,
    serveSitemap,
    serveSource,
 )
import Hub.Pages (jsonError, loginPage, startingPage, textResponse)
import Hub.Proxy.Forward (proxyWithRetry)
import Hub.Session (
    SessionManager (..),
    lookupBySessionId,
 )
import Hub.Share (ShareStore, validSlug)
import Hub.Shares.Api (
    handleDeleteShare,
    handleListShares,
    handlePublish,
    serveAsset,
    serveShare,
 )
import Hub.Types
import Hub.Users (UserStore)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai

-- | Create the WAI application. Call once at startup.
hubApp ::
    SessionManager ->
    ShareStore ->
    UserStore ->
    GalleryStore ->
    HC.Manager ->
    IO Application
hubApp sm store users gallery mgr = do
    states <- newTVarIO Map.empty
    pure $ hubApp' sm store users gallery mgr states

{- | Top-level routing. Static-share, public-gallery, and admin routes match
first on the split 'pathInfo'; everything else (auth, health, the authed proxy,
and the anonymous gallery homepage) falls through to 'hubDispatch'.
-}
hubApp' ::
    SessionManager ->
    ShareStore ->
    UserStore ->
    GalleryStore ->
    HC.Manager ->
    PendingStates ->
    Application
hubApp' sm store users gallery mgr states req respond =
    case pathInfo req of
        ["s", slug] -> serveShare store slug respond
        -- Cacheable static assets (no auth): the in-browser WASM runtime.
        ["_hub", "assets", name] ->
            serveAsset (T.unpack (hcAssetsDir cfg)) name respond
        -- Public gallery (no auth, soft-resolved against the share cache).
        ["gallery"] -> serveGallery cfg gallery store req respond
        ["gallery", "feed.xml"] -> serveFeed cfg gallery store respond
        ["sitemap.xml"] -> serveSitemap cfg gallery store respond
        ["c", cid] -> serveCollection cfg gallery store cid respond
        ["c", cid, n] -> serveCollectionReader cfg gallery store cid n respond
        -- Download a share's source markdown (no auth; any stored source).
        ["_hub", "source", slug] -> serveSource store slug respond
        -- Fork into the caller's work dir (authed; the caller is allowlisted).
        ["_hub", "fork", slug]
            | requestMethod req == methodPost ->
                requireSessionOrForkLogin sm slug req respond $ \sess ->
                    let UserId email = sessionUserId sess
                     in serveFork cfg store email slug req respond
        -- Admin: the server-rendered page, then the JSON curation endpoints.
        ["_hub", "admin"] -> adminPageRoute
        ("_hub" : "admin" : _) -> adminDispatch sm users gallery store req respond
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
        _ -> hubDispatch sm store gallery mgr states req respond
  where
    cfg = smConfig sm
    -- The page must not be enumerable: an authed non-admin (or anyone) falls to
    -- the login page, never a 403 that confirms the route.
    adminPageRoute =
        requireAdminPage sm users req respond $
            respond (adminPage (hcAdminContact cfg))

{- | Session gate for the Fork POST: a browser form with no session is sent to
login (so the gallery Fork button degrades to "sign in"), stashing the slug in a
short-lived @sabela_fork@ cookie so the editor can finish the fork right after
sign-in. An API caller still gets JSON 401. The cookie is set only for a valid
slug, so it can't inject a forged @Set-Cookie@.
-}
requireSessionOrForkLogin ::
    SessionManager ->
    Text ->
    Request ->
    (Response -> IO ResponseReceived) ->
    (Session -> IO ResponseReceived) ->
    IO ResponseReceived
requireSessionOrForkLogin sm slug req respond k =
    case extractSessionId req of
        Nothing -> noAuth
        Just sid -> lookupBySessionId sm sid >>= maybe noAuth k
  where
    isBrowser =
        maybe False ("text/html" `BS.isInfixOf`) (lookup hAccept (requestHeaders req))
    loginLoc = ("Location", "/_hub/login")
    forkCookie =
        ( "Set-Cookie"
        , "sabela_fork=" <> TE.encodeUtf8 slug <> "; Path=/; Max-Age=600; SameSite=Lax"
        )
    noAuth
        | not isBrowser = respond (jsonError status401 "Not signed in.")
        | validSlug slug = respond (responseLBS status303 [loginLoc, forkCookie] "")
        | otherwise = respond (responseLBS status303 [loginLoc] "")

{- | Page-flavoured admin gate: 'requireAdmin' answers JSON 403, which would
make @\/_hub\/admin@ enumerable, so the page instead falls back to 'loginPage'.
-}
requireAdminPage ::
    SessionManager ->
    UserStore ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived ->
    IO ResponseReceived
requireAdminPage sm users req respond k =
    requireAdmin sm users req (const (respond loginPage)) (const k)

hubDispatch ::
    SessionManager ->
    ShareStore ->
    GalleryStore ->
    HC.Manager ->
    PendingStates ->
    Application
hubDispatch sm store gallery mgr states req respond =
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
                    Nothing -> anonymous
                    Just sid -> do
                        mSess <- lookupBySessionId sm sid
                        case mSess of
                            Nothing -> anonymous
                            Just sess ->
                                case sessionState sess of
                                    SReady ip ->
                                        proxyWithRetry mgr (hcBackendPort cfg) ip req respond
                                    SStarting ->
                                        respond startingPage
                                    SStopping -> anonymous
  where
    -- The gallery is the public homepage: an anonymous root request renders it;
    -- any other unmatched anonymous path keeps the login page (scope the gallery
    -- to '/' so it isn't served at infinite URLs).
    anonymous
        | rawPathInfo req == "/" =
            serveGallery (smConfig sm) gallery store req respond
        | otherwise = respond loginPage
