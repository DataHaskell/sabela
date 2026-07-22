{-# LANGUAGE OverloadedStrings #-}

module Test.PostNudgeGateSpec (postNudgeGateSpec) where

import Data.Aeson (Value, object, (.=))
import Test.Hspec

import Siza.Agent.Discover.History (
    emptyLedger,
    ledgerClose,
    ledgerRecord,
    ledgerShortcut,
 )
import Test.DiscoverFixtures (stateOf)

found :: Value
found =
    object
        [ "state" .= ("found" :: String)
        , "hits"
            .= [ object
                    [ "name" .= ("gust" :: String)
                    , "type" .= ("Zephyr" :: String)
                    , "matchKind" .= ("exact" :: String)
                    ]
               ]
        ]

postNudgeGateSpec :: Spec
postNudgeGateSpec = describe "post-nudge same-cluster gate (R10-T4)" $
    it "short-circuits an equivalent spelling without calling a backend" $ do
        let (recorded, _) = ledgerRecord "gust" found emptyLedger
            closed = ledgerClose recorded
        ledgerShortcut closed "gust function"
            `shouldSatisfy` maybe False ((== "duplicate") . stateOf)
