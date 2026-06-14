{-# LANGUAGE OverloadedStrings #-}

{- | Tests for @Sabela.Output.Widgets@: the shared @sabela-widgets.js@ runtime is
embedded, the prelude definitions emit @sabelaXxx(cfg)@ bootstraps, and — the
key regression — they no longer build inline event handlers in HTML.
-}
module Test.WidgetsSpec (spec) where

import qualified Data.Text as T
import Sabela.Output.Widgets (sabelaWidgetsJs, widgetDefs)
import Test.Hspec

spec :: Spec
spec = do
    describe "sabela-widgets.js is embedded" $ do
        let js = sabelaWidgetsJs
        it "defines a bootstrap per widget" $
            mapM_
                (\f -> js `shouldSatisfy` T.isInfixOf f)
                [ "function sabelaSlider"
                , "function sabelaDropdown"
                , "function sabelaCheckbox"
                , "function sabelaTextInput"
                , "function sabelaButton"
                ]
        it "wires the shared postMessage bridge" $ do
            js `shouldSatisfy` T.isInfixOf "function _sabelaPost"
            js `shouldSatisfy` T.isInfixOf "parent.postMessage"
        it "builds controls with the DOM, not HTML strings" $
            js `shouldSatisfy` T.isInfixOf "document.createElement"

    describe "widgetDefs (the prelude fragment)" $ do
        let defs = widgetDefs
        it "embeds the runtime once" $
            defs `shouldSatisfy` T.isInfixOf "_sabelaWidgetsJs ="
        it "emits a bootstrap call per widget" $
            mapM_
                (\c -> defs `shouldSatisfy` T.isInfixOf c)
                [ "sabelaSlider({"
                , "sabelaDropdown({"
                , "sabelaCheckbox({"
                , "sabelaTextInput({"
                , "sabelaButton({"
                ]
        it "keeps the friendly constructor signatures (export-freeze depends on them)" $
            mapM_
                (\s -> defs `shouldSatisfy` T.isInfixOf s)
                [ "slider name def lo hi"
                , "dropdown name opts def"
                , "checkbox name def"
                , "textInput name def"
                , "button label name"
                ]

    describe "no inline JS (the regression we are fixing)" $ do
        it "widgetDefs has no inline event-handler attributes" $ do
            widgetDefs `shouldNotSatisfy` T.isInfixOf "oninput="
            widgetDefs `shouldNotSatisfy` T.isInfixOf "onclick="
            widgetDefs `shouldNotSatisfy` T.isInfixOf "onchange="
        it "widgetDefs no longer concatenates a raw unescaped value into an attribute" $
            widgetDefs `shouldNotSatisfy` T.isInfixOf "value='\" ++"
