{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Static asset serving (embedded @index.html@ + the dashboard / slideshow
shells), the safe-canonicalised @/api/asset@ pass-through for files in the
work directory, and the single-file @/api/upload@ endpoint. Export
endpoints live in "Sabela.Server.Export"; both modules share the embedded
HTML shells re-exported here.
-}
module Sabela.Server.Static (
    -- * Embedded HTML shells
    indexHtml,
    dashboardHtml,
    slideshowHtml,

    -- * Handlers
    staticApp,
    dashboardApp,
    slideshowApp,
    assetApp,
    uploadApp,

    -- * Pieces (exposed for testing)
    safeUploadName,
    assetContentType,
) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (
    hContentType,
    status200,
    status400,
    status404,
 )
import Network.Wai (
    Application,
    queryString,
    responseLBS,
    strictRequestBody,
 )
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesFileExist,
 )
import System.FilePath (makeRelative, takeExtension, takeFileName, (</>))

import Sabela.Api (errorJson)
import Sabela.Server.Files (isWithinPath)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))

indexHtml :: BS.ByteString
indexHtml = $(makeRelativeToProject "static/index.html" >>= embedFile)

dashboardHtml :: BS.ByteString
dashboardHtml = $(makeRelativeToProject "static/dashboard.html" >>= embedFile)

slideshowHtml :: BS.ByteString
slideshowHtml = $(makeRelativeToProject "static/slideshow.html" >>= embedFile)

staticApp :: Application
staticApp _req resp =
    resp $
        responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (LBS.fromStrict indexHtml)

dashboardApp :: Application
dashboardApp _req resp =
    resp $
        responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (LBS.fromStrict dashboardHtml)

slideshowApp :: Application
slideshowApp _req resp =
    resp $
        responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (LBS.fromStrict slideshowHtml)

{- | Serve a raw file (images, etc.) from the work directory, addressed by
the @?path=@ query param relative to the work-dir root. The path is
canonicalized and confined to the work directory.
-}
assetApp :: App -> Application
assetApp app req resp = do
    let workDir = envWorkDir (appEnv app)
        mPath = do
            (_, v) <- lookupQuery "path" (queryString req)
            TE.decodeUtf8 <$> v
    case mPath of
        Nothing -> notFound
        Just relText -> do
            rootCanon <- canonicalizePath workDir
            fileCanon <- canonicalizePath (workDir </> T.unpack relText)
            exists <- doesFileExist fileCanon
            if not (isWithinPath rootCanon fileCanon) || not exists
                then notFound
                else do
                    bytes <- LBS.readFile fileCanon
                    resp $
                        responseLBS
                            status200
                            [ (hContentType, assetContentType fileCanon)
                            , ("Cache-Control", "no-cache")
                            ]
                            bytes
  where
    lookupQuery k q = case [p | p@(key, _) <- q, key == k] of
        (x : _) -> Just x
        [] -> Nothing
    notFound =
        resp $
            responseLBS
                status404
                [(hContentType, "text/plain; charset=utf-8")]
                "asset not found"

assetContentType :: FilePath -> BS.ByteString
assetContentType p = case map toLower (takeExtension p) of
    ".png" -> "image/png"
    ".jpg" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".gif" -> "image/gif"
    ".webp" -> "image/webp"
    ".avif" -> "image/avif"
    ".svg" -> "image/svg+xml"
    ".bmp" -> "image/bmp"
    ".ico" -> "image/x-icon"
    ".pdf" -> "application/pdf"
    _ -> "application/octet-stream"

{- | Reduce an uploaded file name to a safe basename within a single directory,
or 'Nothing' if it isn't usable (empty, @.@, @..@, or only a path). Pure so the
upload path guard is unit-testable.
-}
safeUploadName :: Text -> Maybe Text
safeUploadName raw =
    let n = takeFileName (T.unpack (T.strip raw))
     in if n `elem` ["", ".", ".."] then Nothing else Just (T.pack n)

{- | Receive one uploaded file: the raw request body is written verbatim
(binary-safe) to @\<workDir\>\/\<dir\>\/\<name\>@. @?dir=@ is an optional
sub-path (default root) and @?name=@ the file name; the directory is
canonicalized and confined to the work dir, and the name is reduced to a safe
basename, so neither can escape the workspace.
-}
uploadApp :: App -> Application
uploadApp app req resp = do
    let workDir = envWorkDir (appEnv app)
        q = queryString req
        mDir = TE.decodeUtf8 <$> (lookupQ "dir" q >>= snd)
        mName = TE.decodeUtf8 <$> (lookupQ "name" q >>= snd)
    case mName >>= safeUploadName of
        Nothing -> bad "missing or invalid file name"
        Just name -> do
            let destDir = workDir </> maybe "" T.unpack mDir
            rootCanon <- canonicalizePath workDir
            destCanon <- canonicalizePath destDir
            if not (isWithinPath rootCanon destCanon)
                then bad "directory is outside the workspace"
                else do
                    body <- strictRequestBody req
                    createDirectoryIfMissing True destCanon
                    let dest = destCanon </> T.unpack name
                    LBS.writeFile dest body
                    putStrLn $ "[sabela] Uploaded: " ++ dest
                    resp $
                        responseLBS
                            status200
                            [(hContentType, "application/json")]
                            ( encode
                                ( object
                                    [ "path" .= T.pack (makeRelative rootCanon dest)
                                    , "name" .= name
                                    ]
                                )
                            )
  where
    lookupQ k qq = case [p | p@(key, _) <- qq, key == k] of
        (x : _) -> Just x
        [] -> Nothing
    bad msg =
        resp $
            responseLBS
                status400
                [(hContentType, "application/json")]
                (encode (errorJson msg))
