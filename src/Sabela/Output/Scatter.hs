{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The @scatterSelect@ widget's GHCi-source definitions, spliced into
'Sabela.Output.displayPrelude'. The render logic lives in the real
@static/src/widgets/scatter.js@ file (lintable, prettier-formatted) and is
embedded here verbatim; each render only emits a small @sabelaScatter(cfg)@
bootstrap carrying the per-render data.
-}
module Sabela.Output.Scatter (scatterDefs, scatterWidgetJs) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

{- | The scatter widget render library, embedded from
@static/src/widgets/scatter.js@ at compile time. Exposes a single
@sabelaScatter(cfg)@ global that draws into a canvas inside the output iframe.
-}
scatterWidgetJs :: Text
scatterWidgetJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/scatter.js" >>= embedFile)

scatterDefs :: Text
scatterDefs =
    T.unlines
        [ "data ScatterOpts = ScatterOpts { soWidth :: Int, soHeight :: Int, soColor :: String, soAlpha :: Double, soRadius :: Double, soSelColor :: String, soTitle :: String, soXLabel :: String, soYLabel :: String, soXBounds :: Maybe (Double, Double), soYBounds :: Maybe (Double, Double), soColorBy :: [Double] }"
        , "defScatter :: ScatterOpts"
        , "defScatter = ScatterOpts { soWidth = 560, soHeight = 360, soColor = \"#4a9eff\", soAlpha = 0.55, soRadius = 2, soSelColor = \"#e3116c\", soTitle = \"\", soXLabel = \"\", soYLabel = \"\", soXBounds = Nothing, soYBounds = Nothing, soColorBy = [] }"
        , "scatterSelect :: String -> [(Double, Double)] -> Behavior [Int]"
        , "scatterSelect name = scatterSelectWith name defScatter"
        , "scatterSelectWith :: String -> ScatterOpts -> [(Double, Double)] -> Behavior [Int]"
        , "scatterSelectWith name opts pts = Behavior { bSample = widgetRead name ([] :: [Int]), bRender = scatterRender name opts pts }"
        , "scatterRender :: String -> ScatterOpts -> [(Double, Double)] -> IO ()"
        , "scatterRender name opts pts = do"
        , "  sel <- widgetRead name ([] :: [Int])"
        , "  cid <- readIORef _sabelaCellIdRef"
        , "  displayMime_ \"text/html\" (scatterHtml name cid opts pts sel)"
        , "_sabelaScatterJs :: String"
        , "_sabelaScatterJs = " <> T.pack (show (T.unpack scatterWidgetJs))
        , "scatterHtml :: String -> String -> ScatterOpts -> [(Double, Double)] -> [Int] -> String"
        , "scatterHtml name cid opts pts sel = unlines"
        , "  [ \"<div style='font-family:sans-serif'>\""
        , "  , \"<canvas id='\" ++ elId ++ \"' width='\" ++ show w ++ \"' height='\" ++ show h ++ \"' style='border:1px solid #e2e2ea;border-radius:6px;cursor:crosshair;max-width:100%;touch-action:none'></canvas>\""
        , "  , \"<div style='color:#889;font-size:11px;margin-top:5px'>drag to lasso-select &middot; double-click to clear &middot; \" ++ show (length pts) ++ \" points\" ++ (if null sel then \"\" else \", \" ++ show (length sel) ++ \" selected\") ++ \"</div>\""
        , "  , \"<script>\""
        , "  , _sabelaScatterJs"
        , "  , \"sabelaScatter({elId:'\" ++ elId ++ \"',name:'\" ++ name ++ \"',cid:\" ++ cid ++ \",pts:\" ++ ptsJs ++ \",sel:\" ++ show sel ++ \",cval:\" ++ cvalJs ++ \",w:\" ++ show w ++ \",h:\" ++ show h ++ \",r:\" ++ show (soRadius opts) ++ \",alpha:\" ++ show (soAlpha opts) ++ \",color:'\" ++ sanitize (soColor opts) ++ \"',selColor:'\" ++ sanitize (soSelColor opts) ++ \"',title:'\" ++ sanitize (soTitle opts) ++ \"',xlab:'\" ++ sanitize (soXLabel opts) ++ \"',ylab:'\" ++ sanitize (soYLabel opts) ++ \"',xb:\" ++ boundsJs (soXBounds opts) ++ \",yb:\" ++ boundsJs (soYBounds opts) ++ \"});\""
        , "  , \"</script>\""
        , "  , \"</div>\""
        , "  ]"
        , "  where"
        , "    w = soWidth opts"
        , "    h = soHeight opts"
        , "    elId = \"sc_\" ++ cid ++ \"_\" ++ name"
        , "    sanitize = filter (\\c -> c /= '\\'' && c /= '\\\\' && c /= '<')"
        , "    ptsJs = \"[\" ++ concatMap (\\(x,y) -> \"[\" ++ show x ++ \",\" ++ show y ++ \"],\") pts ++ \"]\""
        , "    cvalJs = \"[\" ++ concatMap (\\v -> show v ++ \",\") (soColorBy opts) ++ \"]\""
        , "    boundsJs Nothing = \"null\""
        , "    boundsJs (Just (a,b)) = \"[\" ++ show a ++ \",\" ++ show b ++ \"]\""
        ]
