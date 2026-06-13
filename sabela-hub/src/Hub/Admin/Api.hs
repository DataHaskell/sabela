{-# LANGUAGE OverloadedStrings #-}

{- | Admin curation API: @adminDispatch@ routes the @\/_hub\/admin\/*@ JSON
endpoints behind 'requireAdmin' (its own cookie→session→isAdmin lookup, since
'requireSession' is JSON-401-only) and, for mutations, a config-anchored
'Origin' check (fail closed). The server-rendered page lives in "Hub.Admin.Page".
-}
module Hub.Admin.Api (
    adminDispatch,
    requireAdmin,
    canonicalOrigin,
) where

import Control.Monad (forM)
import Data.Aeson (Value, decode, object, (.:), (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseMaybe, withObject)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Hub.Auth (extractSessionId)
import Hub.Gallery
import Hub.Pages (jsonError, jsonResponse)
import Hub.Session (SessionManager (..), lookupBySessionId)
import Hub.Share (Share (..), ShareStore, listAllShares, validSlug)
import Hub.Types
import Hub.Users (UserStore, isAdmin)
import Network.HTTP.Types
import Network.Wai

-- | scheme+host of @GOOGLE_REDIRECT_URI@ — the only Origin a mutation may carry.
canonicalOrigin :: SessionManager -> BS.ByteString
canonicalOrigin sm =
    TE.encodeUtf8 $
        T.intercalate "/" $
            take 3 $
                T.splitOn "/" (hcGoogleRedirectUri (smConfig sm))

{- | Run the continuation with the caller's email iff they are an admin; else
403 JSON (never enumerable). Does its own session lookup — it cannot wrap the
JSON-401-only 'Hub.Auth.requireSession'.
-}
requireAdmin ::
    SessionManager ->
    UserStore ->
    Request ->
    (Response -> IO ResponseReceived) ->
    (Text -> IO ResponseReceived) ->
    IO ResponseReceived
requireAdmin sm users req respond k =
    case extractSessionId req of
        Nothing -> deny
        Just sid -> do
            mSess <- lookupBySessionId sm sid
            case mSess of
                Nothing -> deny
                Just sess -> do
                    let UserId email = sessionUserId sess
                    ok <- isAdmin users email
                    if ok then k email else deny
  where
    deny = respond (jsonError status403 "Admin access required.")

-- | Mutations must carry the canonical 'Origin' (fail closed on a missing one).
originOk :: SessionManager -> Request -> Bool
originOk sm req =
    lookup "Origin" (requestHeaders req) == Just (canonicalOrigin sm)

adminDispatch ::
    SessionManager -> UserStore -> GalleryStore -> ShareStore -> Application
adminDispatch sm users gallery shares req respond =
    case drop 2 (pathInfo req) of
        ["shares"] -> on "GET" $ gated $ \_ -> handleShares gallery shares respond
        ["feature"] -> on "POST" $ mutate $ \_ -> handleFeature gallery shares req respond
        ["feature", slug] -> on "DELETE" $ mutate $ \_ -> removeFeatured gallery slug >> ok respond
        ["collection"] -> on "POST" $ mutate $ \_ -> handleCreateCol gallery req respond
        ["collection", cid] -> case requestMethod req of
            "PUT" -> mutate $ \_ -> handleUpdateCol gallery cid req respond
            "DELETE" -> mutate $ \_ -> deleteCollection gallery cid >> ok respond
            _ -> notAllowed
        ["collection", cid, "members"] ->
            on "PUT" $ mutate $ \_ -> handleMembers gallery shares cid req respond
        ["order"] -> on "PUT" $ mutate $ \_ -> handleOrder gallery req respond
        ["tags", tid] -> on "PUT" $ mutate $ \_ -> handleTags gallery tid req respond
        _ -> respond (jsonError status404 "No such admin endpoint.")
  where
    gated = requireAdmin sm users req respond
    mutate k =
        if originOk sm req
            then gated k
            else respond (jsonError status403 "Cross-origin request rejected.")
    on method act = if requestMethod req == method then act else notAllowed
    notAllowed = respond (jsonError status405 "Method not allowed.")

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

ok :: (Response -> IO ResponseReceived) -> IO ResponseReceived
ok respond = respond $ jsonResponse status200 (object ["ok" .= True])

handleShares ::
    GalleryStore ->
    ShareStore ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleShares gallery shares respond = do
    allSh <- listAllShares shares
    feat <- featuredSlugs gallery
    membership <- collectionMembership gallery
    dangling <- danglingFeatured gallery shares
    shareObjs <- forM allSh $ \s -> do
        tags <- getTags gallery (shareSlug s)
        let inCols = [cid | (cid, ms) <- membership, shareSlug s `elem` ms]
        pure $
            object
                [ "slug" .= shareSlug s
                , "title" .= shareTitle s
                , "owner" .= shareOwner s
                , "mode" .= exportModeText (shareMode s)
                , "createdAt" .= shareCreatedAt s
                , "featured" .= (shareSlug s `elem` feat)
                , "inCollections" .= inCols
                , "tags" .= tags
                ]
    let danglingObjs = [object ["slug" .= sl, "tags" .= tg] | (sl, tg) <- dangling]
    respond $
        jsonResponse status200 $
            object ["shares" .= shareObjs, "dangling" .= danglingObjs]

handleFeature ::
    GalleryStore ->
    ShareStore ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleFeature gallery shares req respond =
    withField req "slug" respond $ \slug -> do
        allSh <- listAllShares shares
        if slug `elem` map shareSlug allSh
            then addFeatured gallery slug >> ok respond
            else respond $ jsonError status404 "No such share."

handleCreateCol ::
    GalleryStore ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleCreateCol gallery req respond = do
    body <- strictRequestBody req
    case decode body of
        Just v
            | Just t <- str "title" v ->
                do
                    cid <- createCollection gallery t (strDef "description" v)
                    respond $ jsonResponse status200 (object ["cid" .= cid])
        _ -> respond $ jsonError status400 "Expected {title, description}."

handleUpdateCol ::
    GalleryStore ->
    Text ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleUpdateCol gallery cid req respond = do
    body <- strictRequestBody req
    case decode body of
        Just v
            | Just t <- str "title" v -> do
                done <- updateCollection gallery cid t (strDef "description" v)
                if done then ok respond else respond (jsonError status404 "No such collection.")
        _ -> respond $ jsonError status400 "Expected {title, description}."

handleMembers ::
    GalleryStore ->
    ShareStore ->
    Text ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleMembers gallery shares cid req respond = do
    body <- strictRequestBody req
    case decode body >>= strList "members" of
        Nothing -> respond $ jsonError status400 "Expected {members:[slug]}."
        Just members -> do
            res <- setCollectionMembers gallery shares cid members
            case res of
                MembersOk -> ok respond
                MembersUnknownCollection -> respond $ jsonError status404 "No such collection."
                MembersInvalid bad ->
                    respond $ jsonResponse status400 (object ["invalid" .= bad])

handleOrder ::
    GalleryStore ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleOrder gallery req respond = do
    body <- strictRequestBody req
    case decode body >>= parseOrder of
        Nothing -> respond $ jsonError status400 "Expected {index:[{kind,id}]}."
        Just entries -> setIndexOrder gallery entries >> ok respond

parseOrder :: Value -> Maybe [(Text, Text)]
parseOrder = parseMaybe $ withObject "order" $ \o -> do
    arr <- o .: "index"
    forM arr $ \e ->
        flip (withObject "entry") e $ \obj ->
            (,) <$> obj .: "kind" <*> obj .: "id"

handleTags ::
    GalleryStore ->
    Text ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleTags gallery tid req respond = do
    body <- strictRequestBody req
    case decode body >>= strList "tags" of
        Nothing -> respond $ jsonError status400 "Expected {tags:[tag]}."
        Just tags -> do
            res <- setTags gallery tid tags
            case res of
                TagsOk -> ok respond
                TagsUnknownId -> respond $ jsonError status404 "No such gallery entry."
                TagsTooMany ->
                    respond $
                        jsonResponse status400 (object ["invalid" .= (["too many tags"] :: [Text])])
                TagsInvalid bad -> respond $ jsonResponse status400 (object ["invalid" .= bad])

-- ---------------------------------------------------------------------------
-- Tiny JSON-body helpers (hand-rolled, per the repo convention)
-- ---------------------------------------------------------------------------

withField ::
    Request ->
    Text ->
    (Response -> IO ResponseReceived) ->
    (Text -> IO ResponseReceived) ->
    IO ResponseReceived
withField req k respond f = do
    body <- strictRequestBody req
    case decode body >>= str k of
        Just t -> f t
        Nothing -> respond $ jsonError status400 ("Expected {" <> k <> "}.")

str :: Text -> Value -> Maybe Text
str k = parseMaybe (withObject "o" (.: fromText k))

strDef :: Text -> Value -> Text
strDef k v = fromMaybe "" (str k v)

strList :: Text -> Value -> Maybe [Text]
strList k = parseMaybe (withObject "o" (.: fromText k))
