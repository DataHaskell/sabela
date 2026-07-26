{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sabela.Output.Widgets (widgetDefs, sabelaWidgetsJs) where

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

sabelaWidgetsJs :: Text
sabelaWidgetsJs =
    TE.decodeUtf8
        $(makeRelativeToProject "static/src/widgets/sabela-widgets.js" >>= embedFile)

widgetDefs :: Text
widgetDefs =
    T.unlines
        [ "_sabelaWidgetsJs :: String"
        , "_sabelaWidgetsJs = " <> T.pack (show (T.unpack sabelaWidgetsJs))
        , "_sabelaWidgetBlock :: String -> String -> String"
        , "_sabelaWidgetBlock elId call = \"<div id='\" ++ elId ++ \"'></div><script>\" ++ _sabelaWidgetsJs ++ call ++ \"</script>\""
        , "_sabelaWidgetElId :: String -> String -> String"
        , "_sabelaWidgetElId cid name = \"sw_\" ++ cid ++ \"_\" ++ name"
        , "slider :: (Show a, Read a, Integral a) => String -> a -> a -> a -> Input a"
        , "slider name def lo hi = Input { iValue = widgetRead name def, iShow = widgetRead name def >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaSlider({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",min:\" ++ show lo ++ \",max:\" ++ show hi ++ \",value:\" ++ show val ++ \"});\")) }"
        , "dropdown :: String -> [String] -> String -> Input String"
        , "dropdown name opts def = Input { iValue = fmap (maybe def id) (widgetGet name), iShow = fmap (maybe def id) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaDropdown({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",options:\" ++ show opts ++ \",value:\" ++ show val ++ \"});\")) }"
        , "checkbox :: String -> Bool -> Input Bool"
        , "checkbox name def = Input { iValue = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name), iShow = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaCheckbox({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",checked:\" ++ (if val then \"true\" else \"false\") ++ \"});\")) }"
        , "textInput :: String -> String -> Input String"
        , "textInput name def = Input { iValue = fmap (maybe def id) (widgetGet name), iShow = fmap (maybe def id) (widgetGet name) >>= \\val -> readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaTextInput({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",value:\" ++ show val ++ \"});\")) }"
        , "button :: String -> String -> Input (Maybe ())"
        , "button label name = Input { iValue = fmap (\\mv -> case mv of { Just \"clicked\" -> Just (); _ -> Nothing }) (widgetGet name), iShow = readIORef _sabelaCellIdRef >>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaButton({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",label:\" ++ show label ++ \"});\")) }"
        , "displaySlider :: (Show a, Integral a) => String -> a -> a -> a -> IO ()"
        , "displaySlider name lo hi val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaSlider({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",min:\" ++ show lo ++ \",max:\" ++ show hi ++ \",value:\" ++ show val ++ \"});\"))"
        , "displaySelect :: String -> [String] -> String -> IO ()"
        , "displaySelect name opts val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaDropdown({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",options:\" ++ show opts ++ \",value:\" ++ show val ++ \"});\"))"
        , "displayButton :: String -> String -> IO ()"
        , "displayButton label name = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaButton({elId:\" ++ show elId ++ \",cid:\" ++ cid ++ \",name:\" ++ show name ++ \",label:\" ++ show label ++ \"});\"))"
        ]
