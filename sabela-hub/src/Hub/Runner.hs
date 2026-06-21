{-# LANGUAGE OverloadedStrings #-}

{- | The "run in your browser" client-runtime splice for WASM mode. Like
'Hub.Banner.spliceBanner', one mechanism serves both the publish path (new
shares) and the @republish-runners@ backfill (existing shares): 'spliceRunner'
inserts the in-browser MicroHs runtime loader plus the notebook's scrubbed
source as an inert data island, right after the opening @\<body\>@ tag. It is
idempotent via a marker comment and is a no-op on HTML with no @\<body\>@. Every
byte outside the inserted block is preserved.
-}
module Hub.Runner (
    spliceRunner,
    runnerMarker,
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Hub.Gallery.Chrome (htmlEscape)

-- | Idempotency sentinel: a share whose HTML already contains it is left as-is.
runnerMarker :: BS.ByteString
runnerMarker = "<!--sabela-wasm-runner-->"

{- | Insert the WASM runner loader + source island after the first
@\<body ...\>@ tag. Returns the HTML unchanged when the marker is already
present (idempotent) or when there is no @\<body\>@ (e.g. a fragment), so callers
can apply it blindly. The slug is lowercase hex ('Hub.Share.validSlug'); the
source is HTML-escaped before it enters the @\<script\>@ island, so a notebook
containing @\</script\>@ or @\<!--@ cannot break out of the data island.
-}
spliceRunner :: Text -> Text -> BS.ByteString -> BS.ByteString
spliceRunner slug source html
    | runnerMarker `BS.isInfixOf` html = html
    | otherwise =
        case BS.breakSubstring "<body" html of
            (_, rest) | BS.null rest -> html
            (before, rest) ->
                case BS.elemIndex gt rest of
                    Nothing -> html
                    Just i ->
                        let (bodyOpen, afterBody) = BS.splitAt (i + 1) rest
                         in before <> bodyOpen <> runnerHtml slug source <> afterBody
  where
    gt = 0x3e -- '>'

{- | The runner block: the marker, the inert source data island, the runtime
loader @\<script src\>@, and a one-line bootstrap. The source is HTML-escaped
(via 'htmlEscape', which neutralizes @\<@ and @&@) so it stays inert text inside
the @application/notebook+markdown@ island regardless of its contents.
-}
runnerHtml :: Text -> Text -> BS.ByteString
runnerHtml slug source =
    TE.encodeUtf8 . T.concat $
        [ TE.decodeUtf8 runnerMarker
        , "<script type=\"application/notebook+markdown\""
        , " id=\"sabela-nb-source\" data-slug=\""
        , slug
        , "\">"
        , htmlEscape source
        , "</script>"
        , "<script src=\"/_hub/assets/sabela-wasm-run.js\" defer></script>"
        , "<script>window.SABELA_WASM_SLUG="
        , jsString slug
        , ";</script>"
        ]

{- | A JSON/JS string literal for the slug. The slug is lowercase hex, so only
the quoting is needed; no characters require escaping.
-}
jsString :: Text -> Text
jsString s = "\"" <> s <> "\""
