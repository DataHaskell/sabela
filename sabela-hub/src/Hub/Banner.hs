{-# LANGUAGE OverloadedStrings #-}

module Hub.Banner (
    spliceBanner,
    bannerMarker,
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

bannerMarker :: BS.ByteString
bannerMarker = "<!--sabela-fork-banner-->"

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
    gt = 0x3e

bannerHtml :: Text -> BS.ByteString
bannerHtml slug =
    TE.encodeUtf8 . T.concat $
        [ TE.decodeUtf8 bannerMarker
        , "<div role=\"note\" style=\""
        , wrapStyle
        , "\"><span style=\"flex:1;min-width:0\">Running in your browser"
        , " (MicroHs \8212 a Haskell subset)."
        , " <strong>Fork for the full toolchain &amp; packages.</strong>"
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
