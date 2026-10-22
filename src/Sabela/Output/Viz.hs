{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The chart runtime, embedded so a rendered chart carries what mounts it.
Vega itself is not embedded here: the runtime fetches it from the binary's
@/vendor@ route, which keeps every chart's markup small.
-}
module Sabela.Output.Viz (vizDefs, vizWidgetJs) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.Output.Fence (wrapRuntime)

vizWidgetJs :: Text
vizWidgetJs =
    TE.decodeUtf8 $(makeRelativeToProject "static/src/widgets/viz.js" >>= embedFile)

{- | The runtime as a binding, the function that wraps a chart's markup with
it, and the media helpers (@image@, @video@, @audio@), which show a workspace
file or a URL inline through the same asset route.
-}
vizDefs :: Text
vizDefs =
    T.unlines
        [ "_sabelaVizJs :: SabelaBase.String"
        , "_sabelaVizJs = " <> T.pack (show (T.unpack (wrapRuntime "viz" vizWidgetJs)))
        , "_sabelaVizHtml :: SabelaBase.String -> SabelaBase.String"
        , "_sabelaVizHtml markup = markup SabelaBase.++ \"<script>\" SabelaBase.++ _sabelaVizJs SabelaBase.++ \"\\nsabelaVizMountAll();</script>\""
        , "_sabelaAssetUrl :: SabelaBase.String -> SabelaBase.String"
        , "_sabelaAssetUrl p = if SabelaBase.any (`SabelaBase.isPrefixOf` p) [\"http://\", \"https://\", \"data:\", \"/\"] then p else \"/api/asset?path=\" SabelaBase.++ SabelaBase.concatMap _sabelaUrlChar p"
        , "_sabelaUrlChar :: SabelaBase.Char -> SabelaBase.String"
        , "_sabelaUrlChar c = if SabelaBase.isAlphaNum c SabelaBase.|| c `SabelaBase.elem` \"-._~/\" then [c] else '%' : SabelaBase.map (\"0123456789ABCDEF\" SabelaBase.!!) [SabelaBase.div (SabelaBase.ord c) 16, SabelaBase.mod (SabelaBase.ord c) 16]"
        , "_sabelaAttr :: SabelaBase.String -> SabelaBase.String"
        , "_sabelaAttr = SabelaBase.concatMap (\\c -> case c of { '\"' -> \"&quot;\"; '<' -> \"&lt;\"; '&' -> \"&amp;\"; _ -> [c] })"
        , "image, video, audio :: SabelaBase.String -> Rich"
        , "image src = html (\"<img src=\\\"\" SabelaBase.++ _sabelaAttr (_sabelaAssetUrl src) SabelaBase.++ \"\\\" style=\\\"max-width:100%\\\">\")"
        , "video src = html (\"<video controls style=\\\"max-width:100%\\\" src=\\\"\" SabelaBase.++ _sabelaAttr (_sabelaAssetUrl src) SabelaBase.++ \"\\\"></video>\")"
        , "audio src = html (\"<audio controls style=\\\"width:100%\\\" src=\\\"\" SabelaBase.++ _sabelaAttr (_sabelaAssetUrl src) SabelaBase.++ \"\\\"></audio>\")"
        ]
