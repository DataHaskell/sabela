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

{- | Render a standalone dashboard HTML by injecting notebook JSON into the
template's @/*__SABELA_INJECT__*\/@ placeholder. Outputs only (the dashboard
template never renders code cells' source).
-}
renderStaticDashboard :: BS.ByteString -> Notebook -> LBS.ByteString
renderStaticDashboard template nb =
    injectStatic template (assign "__SABELA_STATIC__" nb)

{- | Render a standalone, read-only "notebook" (tutorial) page. Reuses the same
template + notebook data as the dashboard, then adds a render-mode flag that
makes the page show code cells (prose + code + outputs, in order), and embeds the
reassembled markdown so the page can offer a self-contained "Download .md" — no
backend at view time. @md@ is the canonical notebook markdown (see
'Sabela.Server.exportMarkdownApp').
-}
renderStaticNotebook :: BS.ByteString -> Notebook -> Text -> LBS.ByteString
renderStaticNotebook template nb md =
    injectStatic template $
        assign "__SABELA_STATIC__" nb
            <> "\nwindow.__SABELA_RENDER_MODE__ = \"notebook\";"
            <> "\nwindow.__SABELA_MARKDOWN__ = "
            <> jsLiteral md
            <> ";"

-- | Replace the template's injection placeholder with a block of JS.
injectStatic :: BS.ByteString -> Text -> LBS.ByteString
injectStatic template js =
    LBS.fromStrict . TE.encodeUtf8 $
        T.replace "/*__SABELA_INJECT__*/" js (TE.decodeUtf8 template)

-- | @window.<name> = <json literal>;@
assign :: (ToJSON a) => Text -> a -> Text
assign name v = "window." <> name <> " = " <> jsLiteral v <> ";"

{- | A JS literal from any JSON-encodable value, with every @</@ rewritten to
@<\\/@ so a @</script>@ (any case / trailing space) inside the data cannot close
the enclosing @<script>@. @<\\/@ is an ordinary @</@ once the JS string parses,
so the data round-trips unchanged.
-}
jsLiteral :: (ToJSON a) => a -> Text
jsLiteral =
    T.replace "</" "<\\/" . TE.decodeUtf8 . LBS.toStrict . encode
