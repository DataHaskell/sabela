{-# LANGUAGE OverloadedStrings #-}

module Hub.Fork (
    serveFork,
) where

import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Network.Wai
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Hub.Docker (sanitize)
import Hub.OAuth (generateRandomToken)
import Hub.Pages (jsonError, jsonResponse)
import Hub.Share (
    ShareStore,
    lookupShareSource,
    validSlug,
 )
import Hub.Types

serveFork ::
    HubConfig ->
    ShareStore ->
    Text ->
    Text ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveFork cfg shares forker slug req respond
    | not (originOk cfg req) =
        respond (jsonError status403 "Cross-origin request rejected.")
    | not (validSlug slug) = respond notForkable
    | otherwise = do
        msrc <- lookupShareSource shares slug
        case msrc of
            Nothing -> respond notForkable
            Just src -> do
                name <- ("forked-" <>) . (<> ".md") <$> generateRandomToken
                let dir =
                        T.unpack (dcDataRoot (hcDockerConfig cfg))
                            </> "users"
                            </> T.unpack (sanitize forker)
                createDirectoryIfMissing True dir
                BS.writeFile (dir </> T.unpack name) src
                respond $
                    if wantsHtml req
                        then
                            responseLBS
                                status303
                                [("Location", TE.encodeUtf8 ("/?open=" <> name))]
                                ""
                        else jsonResponse status200 (object ["notebook" .= name])
  where
    notForkable = jsonError status404 "That notebook can't be forked."

wantsHtml :: Request -> Bool
wantsHtml req =
    maybe False ("text/html" `BS.isInfixOf`) (lookup hAccept (requestHeaders req))

originOk :: HubConfig -> Request -> Bool
originOk cfg req =
    lookup "Origin" (requestHeaders req) == Just (TE.encodeUtf8 origin)
  where
    origin = T.intercalate "/" (take 3 (T.splitOn "/" (hcGoogleRedirectUri cfg)))
