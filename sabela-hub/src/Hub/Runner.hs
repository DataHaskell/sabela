{-# LANGUAGE OverloadedStrings #-}

module Hub.Runner (
    spliceRunner,
    runnerMarker,
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Hub.Gallery.Chrome (htmlEscape)

runnerMarker :: BS.ByteString
runnerMarker = "<!--sabela-wasm-runner-->"

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
    gt = 0x3e

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

jsString :: Text -> Text
jsString s = "\"" <> s <> "\""
