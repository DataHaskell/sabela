{-# LANGUAGE OverloadedStrings #-}

module Test.RecordFieldUseSpec (recordFieldUseSpec) where

import Data.Aeson (Key, Value (..), object, (.=))
import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Discover.Classify (sessionAnswer)
import Siza.Agent.Discover.Types (
    DHit (..),
    Interpreted (..),
    SourceAnswer (..),
 )

interp :: Interpreted
interp = Interpreted "columnSeparator" "columnSeparator" Nothing "name" "" []

matchWith :: [(Key, Value)] -> Value
matchWith extra =
    object
        ( [ "module" .= ("DataFrame" :: Text)
          , "name" .= ("columnSeparator" :: Text)
          , "type" .= ("ReadOptions -> Char" :: Text)
          , "via" .= ("name" :: Text)
          ]
            <> extra
        )

hitsOf :: Value -> [DHit]
hitsOf card = saHits (sessionAnswer interp (Just (object ["matches" .= [card]])))

{- | live_test67's wine cell: the server tells the client this is a record
field (Sabela.AI.Capability.hs's `capField`), and that fact must survive the
JSON round-trip into `DHit` so the model sees it, not just the bare type. -}
recordFieldUseSpec :: Spec
recordFieldUseSpec =
    describe "a record field's update syntax survives into the client's hit" $ do
        it "carries the server's field syntax as the hit's use note" $ do
            let [h] = hitsOf (matchWith [("field", String "ReadOptions { columnSeparator = ... }")])
            dhUse h `shouldBe` Just "ReadOptions { columnSeparator = ... }"

        it "leaves an ordinary function's use note for the generic import fallback" $ do
            let [h] = hitsOf (matchWith [])
            dhUse h `shouldBe` Nothing
