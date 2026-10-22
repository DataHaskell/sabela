{-# LANGUAGE OverloadedStrings #-}

-- | A published component: its mode on the wire, and the page a share shows.
module Test.ComponentShareSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Test.Hspec

import Hub.Components (componentPage)
import Hub.Share (Share (..))
import Hub.Types

spec :: Spec
spec = describe "component shares" $ do
    it "round-trip the component mode" $ do
        parseExportMode "component" `shouldBe` Just ExpComponent
        exportModeText ExpComponent `shouldBe` "component"

    it "render a page with the module, its exports, the source and a use button" $ do
        let share = Share "abc123" "me@example.com" ExpComponent "2026-10-19" "Components.Shout"
            bundle =
                object
                    [ "cbModule" .= ("Components.Shout" :: T.Text)
                    , "cbPurpose" .= ("Shouting <strings>." :: T.Text)
                    , "cbKind" .= ("function" :: T.Text)
                    , "cbExports" .= (["shout", "greeting"] :: [T.Text])
                    , "cbDeps" .= (["text"] :: [T.Text])
                    , "cbSource" .= ("module Components.Shout where\nshout = id" :: T.Text)
                    , "cbVersion" .= (1 :: Int)
                    ]
            page = componentPage share bundle
        T.isInfixOf "<h1>Components.Shout</h1>" page `shouldBe` True
        T.isInfixOf "Shouting &lt;strings&gt;." page `shouldBe` True
        T.isInfixOf "<code>greeting</code>" page `shouldBe` True
        T.isInfixOf "action=\"/_hub/use/abc123\"" page `shouldBe` True
        T.isInfixOf "depends on text" page `shouldBe` True
        T.isInfixOf "module Components.Shout where" page `shouldBe` True
