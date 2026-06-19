{-# LANGUAGE OverloadedStrings #-}

{- | The dashboard re-shell: lifting the @__SABELA_STATIC__@ object literal (and
the optional render-mode\/markdown strings) out of an old export and splicing
them into the current template's @\/*__SABELA_INJECT__*\/@ placeholder. Pins the
brace\/string scanner across nesting, in-string braces, and escapes.
-}
module Test.ReshellSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hub.Dashboard.Reshell (extractObject, extractString, reshell)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

spec :: Spec
spec = do
    describe "extractObject" $ do
        it "extracts a flat object literal" $
            extractObject "window.__SABELA_STATIC__ = {\"a\":1};" "__SABELA_STATIC__"
                `shouldBe` Just "{\"a\":1}"
        it "handles nested braces" $
            extractObject "x; window.V = {\"a\":{\"b\":2}}; y" "V"
                `shouldBe` Just "{\"a\":{\"b\":2}}"
        it "ignores braces inside strings" $
            extractObject "window.V = {\"s\":\"}{ \"};" "V"
                `shouldBe` Just "{\"s\":\"}{ \"}"
        it "respects escaped quotes inside strings" $
            extractObject "window.V = {\"s\":\"a\\\"}\"};" "V"
                `shouldBe` Just "{\"s\":\"a\\\"}\"}"
        it "returns Nothing when the anchor is absent" $
            extractObject "nothing here" "V" `shouldBe` Nothing

    describe "extractString" $ do
        it "extracts a string literal verbatim" $
            extractString "window.M = \"hello\";" "M" `shouldBe` Just "\"hello\""
        it "respects escaped quotes" $
            extractString "window.M = \"a\\\"b\";" "M" `shouldBe` Just "\"a\\\"b\""

    describe "reshell" $ do
        it "splices the injected data into the template placeholder" $ do
            let src = "<old>window.__SABELA_STATIC__ = {\"k\":1};</old>"
                tmpl = "<tpl>/*__SABELA_INJECT__*/</tpl>"
            reshell src tmpl
                `shouldBe` Right "<tpl>window.__SABELA_STATIC__ = {\"k\":1};</tpl>"

        it "carries render mode and markdown when present" $ do
            let src =
                    T.concat
                        [ "window.__SABELA_STATIC__ = {\"k\":1};"
                        , "window.__SABELA_RENDER_MODE__ = \"notebook\";"
                        , "window.__SABELA_MARKDOWN__ = \"# Hi\";"
                        ]
                Right out = reshell src "/*__SABELA_INJECT__*/"
            ("window.__SABELA_RENDER_MODE__ = \"notebook\";" `T.isInfixOf` out)
                `shouldBe` True
            ("window.__SABELA_MARKDOWN__ = \"# Hi\";" `T.isInfixOf` out)
                `shouldBe` True

        it "fails when the template lacks the placeholder" $
            reshell "window.__SABELA_STATIC__ = {};" "no placeholder here"
                `shouldSatisfy` isLeft

        it "fails when the source has no static payload" $
            reshell "nothing" "/*__SABELA_INJECT__*/" `shouldSatisfy` isLeft

        it "replaces only the first placeholder" $ do
            let tmpl = "/*__SABELA_INJECT__*/ AND /*__SABELA_INJECT__*/"
                Right out = reshell "window.__SABELA_STATIC__ = {};" tmpl
            T.count "/*__SABELA_INJECT__*/" out `shouldBe` 1
