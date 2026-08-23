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
        [ "_sabelaWidgetsJs :: SabelaBase.String"
        , "_sabelaWidgetsJs = " <> T.pack (show (T.unpack sabelaWidgetsJs))
        , "_sabelaWidgetBlock :: SabelaBase.String -> SabelaBase.String -> SabelaBase.String"
        , "_sabelaWidgetBlock elId call = \"<div id='\" SabelaBase.++ elId SabelaBase.++ \"'></div><script>\" SabelaBase.++ _sabelaWidgetsJs SabelaBase.++ call SabelaBase.++ \"</script>\""
        , "_sabelaWidgetElId :: SabelaBase.String -> SabelaBase.String -> SabelaBase.String"
        , "_sabelaWidgetElId cid name = \"sw_\" SabelaBase.++ cid SabelaBase.++ \"_\" SabelaBase.++ name"
        , "_sabelaWidgetSlots :: SabelaBase.IO (SabelaBase.String, [(SabelaBase.String, SabelaBase.String)])"
        , "_sabelaWidgetSlots = SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> SabelaIORef.readIORef _sabelaWidgetRef SabelaBase.>>= \\slots -> SabelaBase.return (cid, slots)"
        , "htmlWidget :: SabelaBase.String -> (SabelaBase.String -> SabelaBase.String -> [(SabelaBase.String, SabelaBase.String)] -> (SabelaBase.String, a)) -> Input a"
        , "htmlWidget name build = Input { iValue = _sabelaWidgetSlots SabelaBase.>>= \\(cid, slots) -> case build name cid slots of { (_, value) -> SabelaBase.return value }, iShow = _sabelaWidgetSlots SabelaBase.>>= \\(cid, slots) -> case build name cid slots of { (html, _) -> displayMime_ \"text/html\" html } }"
        , "slider :: (SabelaBase.Show a, SabelaBase.Read a, SabelaBase.Integral a) => SabelaBase.String -> a -> a -> a -> Input a"
        , "slider name def lo hi = Input { iValue = widgetRead name def, iShow = widgetRead name def SabelaBase.>>= \\val -> SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaSlider({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",min:\" SabelaBase.++ SabelaBase.show lo SabelaBase.++ \",max:\" SabelaBase.++ SabelaBase.show hi SabelaBase.++ \",value:\" SabelaBase.++ SabelaBase.show val SabelaBase.++ \"});\")) }"
        , "dropdown :: SabelaBase.String -> [SabelaBase.String] -> SabelaBase.String -> Input SabelaBase.String"
        , "dropdown name opts def = Input { iValue = SabelaBase.fmap (SabelaBase.maybe def SabelaBase.id) (widgetGet name), iShow = SabelaBase.fmap (SabelaBase.maybe def SabelaBase.id) (widgetGet name) SabelaBase.>>= \\val -> SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaDropdown({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",options:\" SabelaBase.++ SabelaBase.show opts SabelaBase.++ \",value:\" SabelaBase.++ SabelaBase.show val SabelaBase.++ \"});\")) }"
        , "checkbox :: SabelaBase.String -> SabelaBase.Bool -> Input SabelaBase.Bool"
        , "checkbox name def = Input { iValue = SabelaBase.fmap (\\mv -> case mv of { SabelaBase.Just \"true\" -> SabelaBase.True; SabelaBase.Just \"false\" -> SabelaBase.False; _ -> def }) (widgetGet name), iShow = SabelaBase.fmap (\\mv -> case mv of { SabelaBase.Just \"true\" -> SabelaBase.True; SabelaBase.Just \"false\" -> SabelaBase.False; _ -> def }) (widgetGet name) SabelaBase.>>= \\val -> SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaCheckbox({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",checked:\" SabelaBase.++ (if val then \"true\" else \"false\") SabelaBase.++ \"});\")) }"
        , "textInput :: SabelaBase.String -> SabelaBase.String -> Input SabelaBase.String"
        , "textInput name def = Input { iValue = SabelaBase.fmap (SabelaBase.maybe def SabelaBase.id) (widgetGet name), iShow = SabelaBase.fmap (SabelaBase.maybe def SabelaBase.id) (widgetGet name) SabelaBase.>>= \\val -> SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaTextInput({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",value:\" SabelaBase.++ SabelaBase.show val SabelaBase.++ \"});\")) }"
        , "button :: SabelaBase.String -> SabelaBase.String -> Input (SabelaBase.Maybe ())"
        , "button label name = Input { iValue = SabelaBase.fmap (\\mv -> case mv of { SabelaBase.Just \"clicked\" -> SabelaBase.Just (); _ -> SabelaBase.Nothing }) (widgetGet name), iShow = SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayMime_ \"text/html\" (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaButton({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",label:\" SabelaBase.++ SabelaBase.show label SabelaBase.++ \"});\")) }"
        , "displaySlider :: (SabelaBase.Show a, SabelaBase.Integral a) => SabelaBase.String -> a -> a -> a -> SabelaBase.IO ()"
        , "displaySlider name lo hi val = SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaSlider({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",min:\" SabelaBase.++ SabelaBase.show lo SabelaBase.++ \",max:\" SabelaBase.++ SabelaBase.show hi SabelaBase.++ \",value:\" SabelaBase.++ SabelaBase.show val SabelaBase.++ \"});\"))"
        , "displaySelect :: SabelaBase.String -> [SabelaBase.String] -> SabelaBase.String -> SabelaBase.IO ()"
        , "displaySelect name opts val = SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaDropdown({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",options:\" SabelaBase.++ SabelaBase.show opts SabelaBase.++ \",value:\" SabelaBase.++ SabelaBase.show val SabelaBase.++ \"});\"))"
        , "displayButton :: SabelaBase.String -> SabelaBase.String -> SabelaBase.IO ()"
        , "displayButton label name = SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid -> displayHtml (let elId = _sabelaWidgetElId cid name in _sabelaWidgetBlock elId (\"sabelaButton({elId:\" SabelaBase.++ SabelaBase.show elId SabelaBase.++ \",cid:\" SabelaBase.++ cid SabelaBase.++ \",name:\" SabelaBase.++ SabelaBase.show name SabelaBase.++ \",label:\" SabelaBase.++ SabelaBase.show label SabelaBase.++ \"});\"))"
        ]
