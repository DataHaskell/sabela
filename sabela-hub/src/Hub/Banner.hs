{-# LANGUAGE OverloadedStrings #-}

{- | The "non-interactive preview — fork to run" banner spliced into a published
share's stored HTML. One mechanism for both the publish path (new shares) and
the @republish-banners@ backfill (existing shares): 'spliceBanner' inserts the
banner right after the opening @\<body\>@ tag, is idempotent via a marker
comment, and is a no-op on HTML that has no @\<body\>@. Every byte outside the
inserted banner is preserved.
-}
module Hub.Banner (
    spliceBanner,
    bannerMarker,
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Idempotency sentinel: a share whose HTML already contains it is left as-is.
bannerMarker :: BS.ByteString
bannerMarker = "<!--sabela-fork-banner-->"

{- | Insert the fork banner after the first @\<body ...\>@ tag. Returns the HTML
unchanged when the marker is already present (idempotent) or when there is no
@\<body\>@ (e.g. a fragment), so callers can apply it blindly.
-}
spliceBanner :: Text -> BS.ByteString -> BS.ByteString
spliceBanner slug html
    | bannerMarker `BS.isInfixOf` html = html
    | otherwise =
        case BS.breakSubstring "<body" html of
            (_, rest) | BS.null rest -> html
            (before, rest) ->
                case BS.elemIndex gt rest of
                    Nothing -> html
                    Just i ->
                        let (bodyOpen, afterBody) = BS.splitAt (i + 1) rest
                         in before <> bodyOpen <> bannerHtml slug <> afterBody
  where
    gt = 0x3e -- '>'

{- | The self-contained banner: inline styles only (so it renders in dashboard,
slideshow, and notebook exports alike), a fork @POST@ form opened in a new tab,
and an inline-JS dismiss. The slug is lowercase hex ('Hub.Share.validSlug'), so
it is safe to interpolate into the action URL.
-}
bannerHtml :: Text -> BS.ByteString
bannerHtml slug =
    TE.encodeUtf8 . T.concat $
        [ TE.decodeUtf8 bannerMarker
        , "<div role=\"note\" style=\""
        , wrapStyle
        , "\"><span style=\"flex:1;min-width:0\">This is a read-only preview"
        , " \8212 sliders, inputs and other interactive controls do not run"
        , " here. <strong>Fork it to run the notebook live in Sabela.</strong>"
        , "</span><form method=\"post\" action=\"/_hub/fork/"
        , slug
        , "\" target=\"_blank\" style=\"margin:0\">"
        , "<button type=\"submit\" style=\""
        , buttonStyle
        , "\">Fork &amp; run \9656</button></form>"
        , "<button type=\"button\" aria-label=\"Dismiss\""
        , " onclick=\"this.parentNode.remove()\" style=\""
        , closeStyle
        , "\">\215</button></div>"
        ]
  where
    wrapStyle =
        T.concat
            [ "display:flex;align-items:center;gap:14px;"
            , "padding:11px 20px;background:#fff4d6;color:#5b4a26;"
            , "border-bottom:1px solid #e6cf93;"
            , "font:14px/1.45 -apple-system,BlinkMacSystemFont,'Segoe UI',system-ui,sans-serif;"
            ]
    buttonStyle =
        T.concat
            [ "white-space:nowrap;cursor:pointer;border:0;border-radius:6px;"
            , "padding:7px 14px;background:#0066ff;color:#fff;font:inherit;"
            , "font-weight:600;"
            ]
    closeStyle =
        T.concat
            [ "cursor:pointer;border:0;background:none;color:#8a7a52;"
            , "font-size:20px;line-height:1;padding:0 2px;"
            ]
