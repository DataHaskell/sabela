{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Server.Import (
    importUrlApp,
    isSafeRemoteUrl,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (
    Manager,
    Request,
    parseRequest,
    requestHeaders,
    responseBody,
    responseStatus,
    withResponse,
 )
import Network.HTTP.Types (
    hContentType,
    status200,
    status400,
    status502,
    statusCode,
 )
import Network.Wai (Application, queryString, responseLBS)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (makeRelative, (</>))

import Sabela.Api (errorJson)
import Sabela.Server.Files (isWithinPath)
import Sabela.Server.Static (safeUploadName)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.Url (rewriteGitHubUrl)

maxImportBytes :: Int
maxImportBytes = 25 * 1024 * 1024

importUrlApp :: App -> Application
importUrlApp app req resp = do
    let q = queryString req
        mUrl = TE.decodeUtf8 <$> (lookupQ "url" q >>= snd)
        mDir = TE.decodeUtf8 <$> (lookupQ "dir" q >>= snd)
        mName = TE.decodeUtf8 <$> (lookupQ "name" q >>= snd)
    case (mUrl, mName >>= safeUploadName) of
        (Nothing, _) -> bad status400 "missing url"
        (_, Nothing) -> bad status400 "missing or invalid file name"
        (Just rawUrl, Just name) -> do
            let url = rewriteGitHubUrl rawUrl
            case appHttpMgr app of
                Nothing -> bad status400 "URL import unavailable (no HTTP manager)"
                Just mgr
                    | not (isSafeRemoteUrl url) ->
                        bad status400 "only http(s) URLs to public hosts can be imported"
                    | otherwise -> do
                        eBytes <- fetchUrl mgr (T.unpack url)
                        case eBytes of
                            Left msg -> bad status502 msg
                            Right bytes ->
                                writeImport app mDir name bytes
                                    >>= either (bad status400) (ok name)
  where
    lookupQ k qq = case [p | p@(key, _) <- qq, key == k] of
        (x : _) -> Just x
        [] -> Nothing
    bad st msg =
        resp $
            responseLBS st [(hContentType, "application/json")] (encode (errorJson msg))
    ok name rel =
        resp $
            responseLBS
                status200
                [(hContentType, "application/json")]
                (encode (object ["path" .= rel, "name" .= name]))

isSafeRemoteUrl :: Text -> Bool
isSafeRemoteUrl url = case schemeHost url of
    Just (scheme, host) ->
        (scheme == "http" || scheme == "https")
            && host `notElem` blockedHosts
            && not ("127." `T.isPrefixOf` host)
    Nothing -> False
  where
    blockedHosts =
        ["localhost", "0.0.0.0", "169.254.169.254", "::1", "[::1]"]

schemeHost :: Text -> Maybe (Text, Text)
schemeHost url =
    let (scheme, rest) = T.breakOn "://" url
     in if T.null rest
            then Nothing
            else
                let afterScheme = T.drop 3 rest
                    authority =
                        T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') afterScheme
                    hostPort = case T.breakOnEnd "@" authority of
                        ("", h) -> h
                        (_, h) -> h
                    host = T.toLower (T.takeWhile (/= ':') hostPort)
                 in if T.null host
                        then Nothing
                        else Just (T.toLower scheme, host)

fetchUrl :: Manager -> String -> IO (Either Text BS.ByteString)
fetchUrl mgr url = do
    eReq <- try (parseRequest url) :: IO (Either SomeException Request)
    case eReq of
        Left _ -> pure (Left "could not parse the URL")
        Right req0 -> do
            let req = req0{requestHeaders = [("User-Agent", "sabela-import")]}
            eRes <-
                try (withResponse req mgr readCapped) ::
                    IO (Either SomeException (Either Text BS.ByteString))
            pure $ case eRes of
                Left e -> Left ("fetch failed: " <> firstLine (T.pack (show e)))
                Right r -> r
  where
    readCapped resp =
        let sc = statusCode (responseStatus resp)
         in if sc < 200 || sc >= 300
                then pure (Left ("remote returned HTTP " <> T.pack (show sc)))
                else do
                    mBytes <- drainCapped (responseBody resp) maxImportBytes
                    pure $ case mBytes of
                        Nothing ->
                            Left
                                ( "file exceeds the "
                                    <> T.pack (show (maxImportBytes `div` (1024 * 1024)))
                                    <> " MB import limit"
                                )
                        Just bs -> Right bs

drainCapped :: IO BS.ByteString -> Int -> IO (Maybe BS.ByteString)
drainCapped readChunk capBytes = go [] 0
  where
    go acc n = do
        chunk <- readChunk
        if BS.null chunk
            then pure (Just (BS.concat (reverse acc)))
            else
                let n' = n + BS.length chunk
                 in if n' > capBytes
                        then pure Nothing
                        else go (chunk : acc) n'

writeImport ::
    App -> Maybe Text -> Text -> BS.ByteString -> IO (Either Text Text)
writeImport app mDir name bytes = do
    let workDir = envWorkDir (appEnv app)
        destDir = workDir </> maybe "" T.unpack mDir
    rootCanon <- canonicalizePath workDir
    destCanon <- canonicalizePath destDir
    if not (isWithinPath rootCanon destCanon)
        then pure (Left "destination directory is outside the workspace")
        else do
            createDirectoryIfMissing True destCanon
            let dest = destCanon </> T.unpack name
            BS.writeFile dest bytes
            putStrLn $ "[sabela] Imported: " ++ dest
            pure (Right (T.pack (makeRelative rootCanon dest)))

firstLine :: Text -> Text
firstLine = T.takeWhile (/= '\n')
