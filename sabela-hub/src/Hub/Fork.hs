{-# LANGUAGE OverloadedStrings #-}

{- | Fork: copy a shared notebook's source into the forking (allowlisted) user's
work dir as a new notebook. The destination filename is server-generated
(@forked-<token>.md@) and the email is sanitized, so the path can't traverse out
of the user's dir (F1); any share with a stored source is forkable, since it is
already public-by-URL at @\/s\/<slug>@ (F2); the forked notebook is plain
markdown that opens inert (F4).
-}
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

{- | @POST \/_hub\/fork\/<slug>@. The caller already passed 'requireSession', so
they are allowlisted; this adds the Origin CSRF check, the public-or-owned slug
gate, and the server-generated destination path.
-}
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
                -- A browser form lands the user in their editor with the
                -- fork opened (?open=); the SPA / post-login resume fetch
                -- (Accept: json) just gets the new notebook name to load.
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

-- | Whether the caller is a browser navigation (so we redirect, not JSON).
wantsHtml :: Request -> Bool
wantsHtml req =
    maybe False ("text/html" `BS.isInfixOf`) (lookup hAccept (requestHeaders req))

-- | Mutations must carry the canonical 'Origin' (fail closed on a missing one).
originOk :: HubConfig -> Request -> Bool
originOk cfg req =
    lookup "Origin" (requestHeaders req) == Just (TE.encodeUtf8 origin)
  where
    origin = T.intercalate "/" (take 3 (T.splitOn "/" (hcGoogleRedirectUri cfg)))
