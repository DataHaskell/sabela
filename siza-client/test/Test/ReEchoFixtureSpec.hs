{-# LANGUAGE OverloadedStrings #-}

{- | Context accounting on the symbolicRegression-off re-echo fixture (the
board's ~15k duplicate load): the ledger eliminates every injected re-echo
(>= 13k per-request, >= 15k with emissions) with zero information loss.
-}
module Test.ReEchoFixtureSpec (reEchoFixtureSpec) where

import Control.Monad (forM_)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Siza.Agent.EmitLedger (dedupText, emptyEmitLedger, recordText)
import Test.ReEchoFixture (Role (..), fixtureMsgs, srcPostHeal)

esc :: Text -> Text
esc t = T.dropEnd 1 (T.drop 1 (TE.decodeUtf8 (LBS.toStrict (encode t))))

{- | Replay the fixture: assistant/system/user content is recorded only (the
model's own bytes are never rewritten); tool content is deduped. Returns the
emitted contents in order.
-}
replay :: [(Role, Text)] -> [(Role, Text)]
replay = go 1 emptyEmitLedger
  where
    go _ _ [] = []
    go i led ((Tool, c) : ms) =
        let (c', led') = dedupText i c led
         in (Tool, c') : go (i + 1) led' ms
    go i led ((r, c) : ms) = (r, c) : go (i + 1) (recordText i c led) ms

{- | Per-request injected cost: each tool message's content is re-sent in
every later chat request (one request per assistant turn after it).
-}
requestCost :: [(Role, Text)] -> Int
requestCost ms =
    sum
        [ T.length c * chatCallsAfter i
        | (i, (Tool, c)) <- zip [1 :: Int ..] ms
        ]
  where
    chatCallsAfter i =
        length [() | (j, (Assistant, _)) <- zip [1 :: Int ..] ms, j > i]

-- | 'requestCost' plus each tool message's own emission into the transcript.
emissionCost :: [(Role, Text)] -> Int
emissionCost ms = requestCost ms + sum [T.length c | (Tool, c) <- ms]

reEchoFixtureSpec :: Spec
reEchoFixtureSpec = describe "symbolicRegression-off re-echo accounting" $ do
    let emitted = replay fixtureMsgs
        savedPerRequest = requestCost fixtureMsgs - requestCost emitted
        savedWithEmission = emissionCost fixtureMsgs - emissionCost emitted

    it "eliminates >= 13k re-injected bytes per-request (of the ~15k load)" $
        savedPerRequest `shouldSatisfy` (>= 13000)

    it "counting each emission itself, the saving covers the measured ~15k" $
        savedWithEmission `shouldSatisfy` (>= 15000)

    it "no injected surface re-transmits the healed source verbatim" $ do
        let toolEchoes =
                [c | (Tool, c) <- emitted, esc srcPostHeal `T.isInfixOf` c]
        toolEchoes `shouldBe` []

    it
        "zero information loss: every rewritten block's lines are recoverable \
        \from earlier emitted content"
        $ do
            let stream = map snd emitted
                logical =
                    T.replace "\\\"" "\"" (T.replace "\\n" "\n" (T.concat stream))
            -- The healed source: its unchanged lines were established by the
            -- model's own last write; the changed line rides the emitted diff.
            forM_ (T.lines srcPostHeal) $ \l ->
                (l, l `T.isInfixOf` logical) `shouldBe` (l, True)

    it "the rewritten surfaces reference their sources (back-ref or diff)" $ do
        let injected = [c | (Tool, c) <- drop 17 emitted]
        length
            [ ()
            | c <- injected
            , "as established turn " `T.isInfixOf` c
                || "changed since turn " `T.isInfixOf` c
            ]
            `shouldSatisfy` (>= 3)
