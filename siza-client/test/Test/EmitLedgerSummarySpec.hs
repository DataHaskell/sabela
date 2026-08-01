{-# LANGUAGE OverloadedStrings #-}

module Test.EmitLedgerSummarySpec (emitLedgerSummarySpec) where

import Control.Monad (forM_, unless)
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.EmitLedger (blockFloor, loadBearingKeys)
import Siza.Agent.Recall (recallToolName)
import Test.EmitLedgerFixtures (encodeT, occursIn, runSeq)

{- | C2-6e: a duplicate envelope's `summary` is the only content it carries,
so eliding it leaves a reference to nothing. Every load-bearing key must
either transmit verbatim or be reachable through the handle its marker names.
-}
emitLedgerSummarySpec :: Spec
emitLedgerSummarySpec = describe "a duplicate's only content survives (C2-6e)" $ do
    it "summary is load-bearing" $
        loadBearingKeys `shouldSatisfy` elem "summary"

    it "a respelled duplicate carries its summary verbatim" $ do
        let dup q =
                encodeT
                    ( object
                        [ "query" .= q
                        , "state" .= ("duplicate" :: Text)
                        , "ref" .= ("call 3" :: Text)
                        , "summary" .= longSummary
                        ]
                    )
            outs = runSeq [dup "q1", dup "q2", dup "q3"]
        T.length longSummary `shouldSatisfy` (>= blockFloor)
        forM_ (drop 1 outs) $ \o -> occursIn longSummary o `shouldBe` True

    it "every load-bearing key transmits verbatim or names a recall handle" $
        forM_ loadBearingKeys $ \k -> do
            let msg q =
                    encodeT
                        ( object
                            [ "query" .= q
                            , "state" .= ("found" :: Text)
                            , K.fromText k .= longSummary
                            ]
                        )
                outs = runSeq [msg "q1", msg "q1", msg "q2"]
                recoverable o = occursIn longSummary o || hasRecallHandle o
            forM_ (zip [1 :: Int ..] (drop 1 outs)) $ \(i, o) ->
                unless (recoverable o) $
                    expectationFailure
                        ("key " <> T.unpack k <> " lost at emission " <> show i)

longSummary :: Text
longSummary =
    "already held: `render` :: Config -> [Row] -> Doc -> Either Text Doc — "
        <> "Report.Render.Core (reportkit 1.4.2, installed-not-loaded, "
        <> "-- cabal: build-depends: reportkit); same ranked answer as call 3 "
        <> "(query 'render report'), 4 hits, top module Report.Render.Core"

hasRecallHandle :: Text -> Bool
hasRecallHandle = T.isInfixOf recallToolName
