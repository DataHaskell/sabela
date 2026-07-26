{-# LANGUAGE OverloadedStrings #-}

module Sabela.Dashboard (
    renderStaticDashboard,
    renderStaticNotebook,
) where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.Model (Notebook)

renderStaticDashboard :: BS.ByteString -> Notebook -> LBS.ByteString
renderStaticDashboard template nb =
    injectStatic template (assign "__SABELA_STATIC__" nb)

renderStaticNotebook :: BS.ByteString -> Notebook -> Text -> LBS.ByteString
renderStaticNotebook template nb md =
    injectStatic template $
        assign "__SABELA_STATIC__" nb
            <> "\nwindow.__SABELA_RENDER_MODE__ = \"notebook\";"
            <> "\nwindow.__SABELA_MARKDOWN__ = "
            <> jsLiteral md
            <> ";"

injectStatic :: BS.ByteString -> Text -> LBS.ByteString
injectStatic template js =
    LBS.fromStrict . TE.encodeUtf8 $
        T.replace "/*__SABELA_INJECT__*/" js (TE.decodeUtf8 template)

assign :: (ToJSON a) => Text -> a -> Text
assign name v = "window." <> name <> " = " <> jsLiteral v <> ";"

jsLiteral :: (ToJSON a) => a -> Text
jsLiteral =
    T.replace "</" "<\\/" . TE.decodeUtf8 . LBS.toStrict . encode
