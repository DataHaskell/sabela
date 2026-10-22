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
import Sabela.Model (Cell (..), Notebook (..), OutputItem (..))
import Sabela.Server.Vendor (vendorFiles)

renderStaticDashboard :: BS.ByteString -> Notebook -> LBS.ByteString
renderStaticDashboard template nb =
    injectStatic template (assign "__SABELA_STATIC__" nb <> vegaBundle nb)

{- | The chart runtime's dependency, carried by an export whose outputs hold a
chart, so the page draws them with nothing to fetch.
-}
vegaBundle :: Notebook -> Text
vegaBundle nb
    | any hasChart (nbCells nb) =
        "\nwindow.__SABELA_VEGA__ = "
            <> jsLiteral (T.intercalate "\n;" (map (TE.decodeUtf8 . snd) vendorFiles))
            <> ";"
    | otherwise = ""
  where
    hasChart c = any (T.isInfixOf "class=\"sabela-viz\"" . oiOutput) (cellOutputs c)

renderStaticNotebook :: BS.ByteString -> Notebook -> Text -> LBS.ByteString
renderStaticNotebook template nb md =
    injectStatic template $
        assign "__SABELA_STATIC__" nb
            <> vegaBundle nb
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
