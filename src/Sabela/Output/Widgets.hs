{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The input-widget GHCi-source definitions, spliced into
'Sabela.Output.displayPrelude'. Mirrors 'Sabela.Output.Scatter': the render
logic lives in the real, linted @static/src/widgets/sabela-widgets.js@ file and
is embedded here verbatim; each widget render only emits a small @sabelaXxx(cfg)@
bootstrap carrying its typed configuration — never inline event handlers or
string-built HTML attributes (which the old prelude did, with an unescaped-value
bug). Constructor names and argument order are preserved so the export-time
widget freezing ('Sabela.Export.Widget') keeps working.
-}
module Sabela.Output.Widgets (widgetDefs, sabelaWidgetsJs) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

{- | The shared widget runtime, embedded from
@static/src/widgets/sabela-widgets.js@ at compile time. Exposes one
@sabelaSlider@\/@sabelaDropdown@\/… global per widget that builds the control in
the output iframe and wires the @parent.postMessage@ bridge.
-}
sabelaWidgetsJs :: Text
sabelaWidgetsJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/sabela-widgets.js" >>= embedFile)

{- | GHCi-source definitions of the input widgets, appended to the display
prelude. Depends on the prelude's @Behavior@\/@widgetGet@\/@widgetRead@\/
@_sabelaCellIdRef@\/@displayMime_@ (defined alongside, exactly as 'scatterDefs'
assumes them).
-}
widgetDefs :: Text
widgetDefs =
    T.unlines
        [ "_sabelaWidgetsJs :: String"
        , "_sabelaWidgetsJs = " <> T.pack (show (T.unpack sabelaWidgetsJs))
        , -- One bootstrap block: a placeholder div + the runtime + the call.
          "_sabelaWidgetBlock :: String -> String -> String"
        , "_sabelaWidgetBlock elId call = \"<div id='\" ++ elId ++ \"'></div><script>\" ++ _sabelaWidgetsJs ++ call ++ \"</script>\""
        , "_sabelaWidgetElId :: String -> String -> String"
        , "_sabelaWidgetElId cid name = \"sw_\" ++ cid ++ \"_\" ++ name"
        , -- slider name def lo hi
          "slider :: (Show a, Read a, Integral a) => String -> a -> a -> a -> Behavior a"
        , "slider name def lo hi = Behavior { bSample = widgetRead name def, bRender = widgetRead name def >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaSlider({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",min:\" ++ show lo ++ \",max:\" ++ show hi ++ \",value:\" ++ show val ++ \"});\")) }"
        , -- dropdown name opts def
          "dropdown :: String -> [String] -> String -> Behavior String"
        , "dropdown name opts def = Behavior { bSample = fmap (maybe def id) (widgetGet name), bRender = fmap (maybe def id) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaDropdown({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",options:\" ++ show opts ++ \",value:\" ++ show val ++ \"});\")) }"
        , -- checkbox name def
          "checkbox :: String -> Bool -> Behavior Bool"
        , "checkbox name def = Behavior { bSample = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name), bRender = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaCheckbox({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",checked:\" ++ (if val then \"true\" else \"false\") ++ \"});\")) }"
        , -- textInput name def
          "textInput :: String -> String -> Behavior String"
        , "textInput name def = Behavior { bSample = fmap (maybe def id) (widgetGet name), bRender = fmap (maybe def id) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaTextInput({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",value:\" ++ show val ++ \"});\")) }"
        , -- button label name
          "button :: String -> String -> Behavior (Maybe ())"
        , "button label name = Behavior { bSample = fmap (\\mv -> case mv of { Just \"clicked\" -> Just (); _ -> Nothing }) (widgetGet name), bRender = readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaButton({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",label:\" ++ show label ++ \"});\")) }"
        , -- displaySlider / displayButton / displaySelect (non-Behavior variants)
          "displaySlider :: (Show a, Integral a) => String -> a -> a -> a -> IO ()"
        , "displaySlider name lo hi val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaSlider({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",min:\" ++ show lo ++ \",max:\" ++ show hi ++ \",value:\" ++ show val ++ \"});\"))"
        , "displaySelect :: String -> [String] -> String -> IO ()"
        , "displaySelect name opts val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaDropdown({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",options:\" ++ show opts ++ \",value:\" ++ show val ++ \"});\"))"
        , "displayButton :: String -> String -> IO ()"
        , "displayButton label name = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaButton({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",label:\" ++ show label ++ \"});\"))"
        ]
