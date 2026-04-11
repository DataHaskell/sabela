{-# LANGUAGE OverloadedStrings #-}

module Sabela.Dashboard (
    renderStaticDashboard,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Aeson (encode)
import Sabela.Model (Notebook)

{- | Render a standalone dashboard HTML by injecting notebook JSON
into the dashboard template. The template contains a placeholder
@/*__SABELA_INJECT__*\/@ which is replaced with a JSON assignment.

The JSON is embedded as a base64-encoded string to avoid any issues
with @\<\/script\>@ sequences in the notebook content breaking the
enclosing script tag.
-}
renderStaticDashboard :: BS.ByteString -> Notebook -> LBS.ByteString
renderStaticDashboard template nb =
    LBS.fromStrict . TE.encodeUtf8 $ T.replace placeholder injection tmpl
  where
    tmpl :: Text
    tmpl = TE.decodeUtf8 template
    placeholder :: Text
    placeholder = "/*__SABELA_INJECT__*/"
    -- Escape </script> sequences that would prematurely close the tag.
    safeJson :: Text
    safeJson =
        T.replace "</script>" "<\\/script>"
            . T.replace "</Script>" "<\\/Script>"
            . T.replace "</SCRIPT>" "<\\/SCRIPT>"
            $ TE.decodeUtf8 (LBS.toStrict (encode nb))
    injection :: Text
    injection =
        "window.__SABELA_STATIC__ = " <> safeJson <> ";"
