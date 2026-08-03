{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sabela.Output.Scatter (scatterDefs, scatterWidgetJs) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

scatterWidgetJs :: Text
scatterWidgetJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/scatter.js" >>= embedFile)

scatterDefs :: Text
scatterDefs =
    T.unlines
        [ "data ScatterOpts = ScatterOpts { soWidth :: SabelaBase.Int, soHeight :: SabelaBase.Int, soColor :: SabelaBase.String, soAlpha :: SabelaBase.Double, soRadius :: SabelaBase.Double, soSelColor :: SabelaBase.String, soTitle :: SabelaBase.String, soXLabel :: SabelaBase.String, soYLabel :: SabelaBase.String, soXBounds :: SabelaBase.Maybe (SabelaBase.Double, SabelaBase.Double), soYBounds :: SabelaBase.Maybe (SabelaBase.Double, SabelaBase.Double), soColorBy :: [SabelaBase.Double] }"
        , "defScatter :: ScatterOpts"
        , "defScatter = ScatterOpts { soWidth = 560, soHeight = 360, soColor = \"#4a9eff\", soAlpha = 0.55, soRadius = 2, soSelColor = \"#e3116c\", soTitle = \"\", soXLabel = \"\", soYLabel = \"\", soXBounds = SabelaBase.Nothing, soYBounds = SabelaBase.Nothing, soColorBy = [] }"
        , "scatterSelect :: SabelaBase.String -> [(SabelaBase.Double, SabelaBase.Double)] -> Input [SabelaBase.Int]"
        , "scatterSelect name = scatterSelectWith name defScatter"
        , "scatterSelectWith :: SabelaBase.String -> ScatterOpts -> [(SabelaBase.Double, SabelaBase.Double)] -> Input [SabelaBase.Int]"
        , "scatterSelectWith name opts pts = Input { iValue = widgetRead name ([] :: [SabelaBase.Int]), iShow = scatterRender name opts pts }"
        , "scatterRender :: SabelaBase.String -> ScatterOpts -> [(SabelaBase.Double, SabelaBase.Double)] -> SabelaBase.IO ()"
        , "scatterRender name opts pts = do"
        , "  sel <- widgetRead name ([] :: [SabelaBase.Int])"
        , "  cid <- SabelaIORef.readIORef _sabelaCellIdRef"
        , "  displayMime_ \"text/html\" (scatterHtml name cid opts pts sel)"
        , "_sabelaScatterJs :: SabelaBase.String"
        , "_sabelaScatterJs = " <> T.pack (show (T.unpack scatterWidgetJs))
        , "scatterHtml :: SabelaBase.String -> SabelaBase.String -> ScatterOpts -> [(SabelaBase.Double, SabelaBase.Double)] -> [SabelaBase.Int] -> SabelaBase.String"
        , "scatterHtml name cid opts pts sel = SabelaBase.unlines"
        , "  [ \"<div style='font-family:sans-serif'>\""
        , "  , \"<canvas id='\" SabelaBase.++ elId SabelaBase.++ \"' width='\" SabelaBase.++ SabelaBase.show w SabelaBase.++ \"' height='\" SabelaBase.++ SabelaBase.show h SabelaBase.++ \"' style='border:1px solid #e2e2ea;border-radius:6px;cursor:crosshair;max-width:100%;touch-action:none'></canvas>\""
        , "  , \"<div style='color:#889;font-size:11px;margin-top:5px'>drag to lasso-select &middot; double-click to clear &middot; \" SabelaBase.++ SabelaBase.show (SabelaBase.length pts) SabelaBase.++ \" points\" SabelaBase.++ (if SabelaBase.null sel then \"\" else \", \" SabelaBase.++ SabelaBase.show (SabelaBase.length sel) SabelaBase.++ \" selected\") SabelaBase.++ \"</div>\""
        , "  , \"<script>\""
        , "  , _sabelaScatterJs"
        , "  , \"sabelaScatter({elId:'\" SabelaBase.++ elId SabelaBase.++ \"',name:'\" SabelaBase.++ name SabelaBase.++ \"',cid:\" SabelaBase.++ cid SabelaBase.++ \",pts:\" SabelaBase.++ ptsJs SabelaBase.++ \",sel:\" SabelaBase.++ SabelaBase.show sel SabelaBase.++ \",cval:\" SabelaBase.++ cvalJs SabelaBase.++ \",w:\" SabelaBase.++ SabelaBase.show w SabelaBase.++ \",h:\" SabelaBase.++ SabelaBase.show h SabelaBase.++ \",r:\" SabelaBase.++ SabelaBase.show (soRadius opts) SabelaBase.++ \",alpha:\" SabelaBase.++ SabelaBase.show (soAlpha opts) SabelaBase.++ \",color:'\" SabelaBase.++ sanitize (soColor opts) SabelaBase.++ \"',selColor:'\" SabelaBase.++ sanitize (soSelColor opts) SabelaBase.++ \"',title:'\" SabelaBase.++ sanitize (soTitle opts) SabelaBase.++ \"',xlab:'\" SabelaBase.++ sanitize (soXLabel opts) SabelaBase.++ \"',ylab:'\" SabelaBase.++ sanitize (soYLabel opts) SabelaBase.++ \"',xb:\" SabelaBase.++ boundsJs (soXBounds opts) SabelaBase.++ \",yb:\" SabelaBase.++ boundsJs (soYBounds opts) SabelaBase.++ \"});\""
        , "  , \"</script>\""
        , "  , \"</div>\""
        , "  ]"
        , "  where"
        , "    w = soWidth opts"
        , "    h = soHeight opts"
        , "    elId = \"sc_\" SabelaBase.++ cid SabelaBase.++ \"_\" SabelaBase.++ name"
        , "    sanitize = SabelaBase.filter (\\c -> c SabelaBase./= '\\'' SabelaBase.&& c SabelaBase./= '\\\\' SabelaBase.&& c SabelaBase./= '<')"
        , "    ptsJs = \"[\" SabelaBase.++ SabelaBase.concatMap (\\(x,y) -> \"[\" SabelaBase.++ SabelaBase.show x SabelaBase.++ \",\" SabelaBase.++ SabelaBase.show y SabelaBase.++ \"],\") pts SabelaBase.++ \"]\""
        , "    cvalJs = \"[\" SabelaBase.++ SabelaBase.concatMap (\\v -> SabelaBase.show v SabelaBase.++ \",\") (soColorBy opts) SabelaBase.++ \"]\""
        , "    boundsJs SabelaBase.Nothing = \"null\""
        , "    boundsJs (SabelaBase.Just (a,b)) = \"[\" SabelaBase.++ SabelaBase.show a SabelaBase.++ \",\" SabelaBase.++ SabelaBase.show b SabelaBase.++ \"]\""
        ]
