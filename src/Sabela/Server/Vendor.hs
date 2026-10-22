{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Third-party assets a page may load from this server rather than the
network: the Vega trio behind charts and the Nordwand Mono faces. Embedded
at build time and served with a long cache life, since a build never changes them.
-}
module Sabela.Server.Vendor (
    vendorApp,
    vendorFiles,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, pathInfo, responseLBS)

-- | The files by the path they are served at under @/vendor@.
vendorFiles :: [(Text, BS.ByteString)]
vendorFiles =
    [
        ( "vega/vega.min.js"
        , $(makeRelativeToProject "static/src/vendor/vega/vega.min.js" >>= embedFile)
        )
    ,
        ( "vega/vega-lite.min.js"
        , $(makeRelativeToProject "static/src/vendor/vega/vega-lite.min.js" >>= embedFile)
        )
    ,
        ( "vega/vega-embed.min.js"
        , $(makeRelativeToProject "static/src/vendor/vega/vega-embed.min.js" >>= embedFile)
        )
    ,
        ( "fonts/NordwandMono-Regular.woff2"
        , $( makeRelativeToProject "static/src/vendor/fonts/NordwandMono-Regular.woff2"
                >>= embedFile
           )
        )
    ,
        ( "fonts/NordwandMono-Medium.woff2"
        , $( makeRelativeToProject "static/src/vendor/fonts/NordwandMono-Medium.woff2"
                >>= embedFile
           )
        )
    ,
        ( "fonts/NordwandMono-SemiBold.woff2"
        , $( makeRelativeToProject "static/src/vendor/fonts/NordwandMono-SemiBold.woff2"
                >>= embedFile
           )
        )
    ,
        ( "fonts/NordwandMono-Italic.woff2"
        , $( makeRelativeToProject "static/src/vendor/fonts/NordwandMono-Italic.woff2"
                >>= embedFile
           )
        )
    ]

-- | The media type a vendored path is served with, by its extension.
vendorMime :: Text -> BS.ByteString
vendorMime path
    | ".woff2" `T.isSuffixOf` path = "font/woff2"
    | otherwise = "application/javascript; charset=utf-8"

vendorApp :: Application
vendorApp req resp =
    case lookup path vendorFiles of
        Just bytes ->
            resp $
                responseLBS
                    status200
                    [ (hContentType, vendorMime path)
                    , ("Cache-Control", "public, max-age=86400, immutable")
                    ]
                    (LBS.fromStrict bytes)
        Nothing ->
            resp $
                responseLBS
                    status404
                    [(hContentType, "text/plain; charset=utf-8")]
                    "not a vendored file"
  where
    path = T.intercalate "/" (pathInfo req)
