{-# LANGUAGE OverloadedStrings #-}

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
    requireSessionOrLogin,
 )
import Hub.CliAuth (
    CliAuth,
    cliAuthPage,
    handleCliApprove,
    handleCliPoll,
    handleCliRevoke,
    handleCliStart,
    newCliAuth,
    resolveCliToken,
    revokeSessionTokens,
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

hubApp ::
    SessionManager ->
    ShareStore ->
    UserStore ->
    GalleryStore ->
    HC.Manager ->
    IO Application
hubApp sm store users gallery mgr = do
    states <- newTVarIO Map.empty
    cliAuth <- newCliAuth (smConfig sm)
    pure $ hubApp' sm store users gallery mgr states cliAuth

hubApp' ::
    SessionManager ->
    ShareStore ->
    UserStore ->
    GalleryStore ->
    HC.Manager ->
    PendingStates ->
    CliAuth ->
    Application
hubApp' sm store users gallery mgr states cliAuth req respond =
    case pathInfo req of
        ["s", slug] -> serveShare store slug respond
        ["_hub", "cli-auth"]
            | requestMethod req == methodGet ->
                requireSessionOrLogin sm req respond $ \_ ->
                    cliAuthPage cliAuth req respond
            | otherwise -> notAllowed
        ["_hub", "cli-auth", "start"]
            | requestMethod req == methodPost -> handleCliStart cliAuth req respond
            | otherwise -> notAllowed
        ["_hub", "cli-auth", "poll"]
            | requestMethod req == methodPost -> handleCliPoll cliAuth req respond
            | otherwise -> notAllowed
        ["_hub", "cli-auth", "approve"]
            | requestMethod req == methodPost ->
                requireSession sm req respond $ \_ ->
                    handleCliApprove cliAuth req respond
            | otherwise -> notAllowed
        ["_hub", "cli-auth", "revoke"]
            | requestMethod req == methodPost -> handleCliRevoke cliAuth req respond
            | otherwise -> notAllowed
        ["_hub", "assets", name] ->
            serveAsset (T.unpack (hcAssetsDir cfg)) name respond
        ["gallery"] -> serveGallery cfg gallery store req respond
        ["gallery", "feed.xml"] -> serveFeed cfg gallery store respond
        ["sitemap.xml"] -> serveSitemap cfg gallery store respond
        ["c", cid] -> serveCollection cfg gallery store cid respond
        ["c", cid, n] -> serveCollectionReader cfg gallery store cid n respond
        ["_hub", "source", slug] -> serveSource store slug respond
        ["_hub", "fork", slug]
            | requestMethod req == methodPost ->
                requireSessionOrForkLogin sm slug req respond $ \sess ->
                    let UserId email = sessionUserId sess
                     in serveFork cfg store email slug req respond
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
        _ -> hubDispatch sm store gallery mgr states cliAuth req respond
  where
    cfg = smConfig sm
    notAllowed = respond (jsonError status405 "Method not allowed.")
    adminPageRoute =
        requireAdminPage sm users req respond $
            respond (adminPage (hcAdminContact cfg))

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

requireAdminPage ::
    SessionManager ->
    UserStore ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived ->
    IO ResponseReceived
requireAdminPage sm users req respond k =
    requireAdmin sm users req (const (respond loginPage)) (const k)

resolveSession :: SessionManager -> CliAuth -> Request -> IO (Maybe Session)
resolveSession sm cliAuth req =
    case extractSessionId req of
        Just sid -> lookupBySessionId sm sid
        Nothing -> do
            mSid <- resolveCliToken cliAuth req
            maybe (pure Nothing) (lookupBySessionId sm) mSid

hubDispatch ::
    SessionManager ->
    ShareStore ->
    GalleryStore ->
    HC.Manager ->
    PendingStates ->
    CliAuth ->
    Application
hubDispatch sm store gallery mgr states cliAuth req respond =
    let path = rawPathInfo req
        cfg = smConfig sm
     in case path of
            "/_hub/health" ->
                respond $ textResponse status200 "ok"
            "/_hub/login" ->
                handleLogin states cfg req respond
            "/_hub/oauth/callback" ->
                handleOAuthCallback sm mgr states cfg req respond
            "/_hub/logout" -> do
                maybe (pure ()) (revokeSessionTokens cliAuth) (extractSessionId req)
                respond (logoutResponse req)
            _ -> do
                mSess <- resolveSession sm cliAuth req
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
    anonymous
        | rawPathInfo req == "/" =
            serveGallery (smConfig sm) gallery store req respond
        | otherwise = respond loginPage
