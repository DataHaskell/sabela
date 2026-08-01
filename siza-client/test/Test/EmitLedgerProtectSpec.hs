{-# LANGUAGE OverloadedStrings #-}

module Test.EmitLedgerProtectSpec (emitLedgerProtectSpec) where

import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Envelope (
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
 )
import Siza.Agent.EmitLedger (
    blockFloor,
    dedupText,
    emptyEmitLedger,
    loadBearingKeys,
 )
import Siza.Agent.Messages (verifyMsgWith)
import Siza.Agent.Tools (renderOutcome)
import Test.DiscoverFixtures (hitsOf, textField)
import Test.EmitLedgerFixtures (
    classes,
    encodeT,
    foundE,
    hitJ,
    longAutofix,
    longDiagnostic,
    longSig,
    occursIn,
    runSeq,
 )

render :: Value -> Text
render = renderOutcome . Right . ToolOk

prose :: Text
prose =
    "The catalogue was consulted across every backend this session. "
        <> T.unwords
            (replicate 10 "This paragraph is summarisable prose and may dedup freely.")

protectedOf :: Value -> [Text]
protectedOf (Object o) =
    concat
        [ if K.toText k `elem` loadBearingKeys then strings v else protectedOf v
        | (k, v) <- KM.toList o
        ]
protectedOf (Array a) = concatMap protectedOf (toList a)
protectedOf _ = []

strings :: Value -> [Text]
strings (String s) = [s | not (T.null s)]
strings (Array a) = concatMap strings (toList a)
strings (Object o) = concatMap strings (KM.elems o)
strings _ = []

{- | A verdict as the harness actually emits it, so this pins the block the
loop puts on the wire rather than a spelling nothing produces.
-}
verdictMsg :: Value
verdictMsg = verifyMsgWith 0 [] Nothing

verifyBody :: Text
verifyBody = textField "content" verdictMsg

wholeReplacement :: Text -> Bool
wholeReplacement oc =
    "[as established turn " `T.isPrefixOf` oc
        || "[changed since turn " `T.isPrefixOf` oc

assertProtected :: [Text] -> Expectation
assertProtected cs = do
    let outs = runSeq cs
    forM_ (zip3 [1 :: Int ..] cs outs) $ \(i, orig, out) -> do
        let ics = T.splitOn "\n\n" orig
            ocs = T.splitOn "\n\n" out
        length ocs `shouldBe` length ics
        forM_ (zip ics ocs) $ \(ic, oc) ->
            unless (wholeReplacement oc) $
                forM_ (maybe [] protectedOf (decodeT ic)) $ \pv ->
                    unless (occursIn pv oc) $
                        expectationFailure
                            ( "turn "
                                <> show i
                                <> ": protected value elided: "
                                <> T.unpack (T.take 60 pv)
                            )
  where
    decodeT :: Text -> Maybe Value
    decodeT = decode . LBS.fromStrict . TE.encodeUtf8

bytesSaved :: [Text] -> Int
bytesSaved cs = sum (zipWith (\i o -> T.length i - T.length o) cs (runSeq cs))

emitLedgerProtectSpec :: Spec
emitLedgerProtectSpec = failureOutputSpec >> protectSpec

protectSpec :: Spec
protectSpec = describe "load-bearing fields are elision-exempt (R8-T1)" $ do
    it "the load-bearing key set is the section 10 contract" $
        forM_ ["type", "signature", "use", "cabal", "name", "next", "exports"] $
            \k -> loadBearingKeys `shouldSatisfy` elem k

    it "the actionable diagnostic and its resolution are exempt" $
        forM_ ["diagnostic", "error", "stderr", "autofix"] $
            \k -> loadBearingKeys `shouldSatisfy` elem k

    it "elided-diagnostic: a repeated rejection carries its diagnostic in full" $ do
        let rejection =
                object
                    [ "notCommitted" .= ("compile-gate" :: Text)
                    , "verdict" .= ("diagnostic" :: Text)
                    , "diagnostic" .= longDiagnostic
                    , "autofix" .= longAutofix
                    ]
            rendered = renderOutcome (Right (ToolErr rejection))
        assertProtected [rendered, rendered]

    it "elided-success: a successful try envelope keeps its autofix note" $ do
        let ok =
                object
                    [ "route" .= ("disposable_scratch" :: Text)
                    , "outcome" .= ("ok" :: Text)
                    , "autofix" .= longAutofix
                    ]
            rendered = renderOutcome (Right (ToolOk ok))
        assertProtected [rendered, rendered, rendered]

    it "elided-verify: a repeated verify verdict is never a back-reference" $ do
        let verdict = encodeT verdictMsg
            outs = runSeq (replicate 6 verdict)
        forM_ (zip [1 :: Int ..] outs) $ \(i, o) ->
            unless (verifyBody `T.isInfixOf` o) $
                expectationFailure
                    ("verify verdict elided on emission " <> show i <> ": " <> T.unpack o)

    describe "generated grid: protected values transmit byte-complete" $
        forM_ classes $ \(label, f) -> do
            it (label <> ": respelled repeats keep every protected value") $
                assertProtected [render (f "q1"), render (f "q2"), render (f "q3")]
            it (label <> ": identical repeat still dedups whole-envelope") $ do
                let cs = [render (f "q1"), render (f "q1")]
                assertProtected cs
                runSeq cs !! 1 `shouldSatisfy` wholeReplacement
                bytesSaved cs `shouldSatisfy` (> 0)

    describe "interleavings across classes" $
        forM_ (zip classes (drop 1 classes ++ take 1 classes)) $
            \((la, f), (lb, g)) ->
                it (la <> " x " <> lb) $
                    assertProtected
                        [ render (f "a")
                        , render (g "a")
                        , render (f "b")
                        , render (g "b")
                        ]

    it "prose blocks still dedup while the envelope's answer survives" $ do
        let cs =
                [ prose <> "\n\n" <> render (foundE "exact" "q1")
                , prose <> "\n\n" <> render (foundE "exact" "q2")
                ]
            second = runSeq cs !! 1
            [proseOut, envOut] = T.splitOn "\n\n" second
        wholeReplacement proseOut `shouldBe` True
        occursIn longSig envOut `shouldBe` True
        bytesSaved cs `shouldSatisfy` (> 0)

    describe "the run-20260720-181807 barChart fixture" $
        it "the Plot signature is verbatim at turns 6 and 11, never a stub" $ do
            T.length longSig `shouldSatisfy` (>= blockFloor)
            let outs =
                    runSeq
                        [ render (foundE "exact" "Plot")
                        , render (foundE "exact" "defaultPlot")
                        , render (foundE "exact" "default")
                        ]
            forM_ (drop 1 outs) $ \out -> do
                occursIn longSig out `shouldBe` True
                out
                    `shouldSatisfy` ( not
                                        . T.isInfixOf
                                            ("(unchanged): " <> T.take 40 longSig)
                                    )

    describe "the 2,500-char bound post-exemption (R3.9)" $
        it "sheds hits, never truncates a surviving protected field" $
            forM_ [1, 4, 12, 24 :: Int] $ \n -> do
                let v =
                        object
                            [ "query" .= ("bars" :: Text)
                            , "state" .= ("found" :: Text)
                            , "hits" .= [hitJ ("bars" <> tShow i) "exact" longSig | i <- [1 .. n]]
                            , "shown" .= n
                            , "omitted" .= (0 :: Int)
                            , "total" .= n
                            ]
                    b = boundEnvelope v
                envelopeChars b `shouldSatisfy` (<= envelopeCharBudget)
                hitsOf b `shouldSatisfy` (not . null)
                forM_ (hitsOf b) $ \h -> do
                    textField "type" h `shouldBe` longSig
                    textField "cabal" h
                        `shouldBe` "-- cabal: build-depends: cumulus"
  where
    tShow = T.pack . show

failureOutputSpec :: Spec
failureOutputSpec = describe "a cell's own output: errors full, info contracted" $ do
    let cellEcho out =
            "{\"cellId\":6,\"ok\":true,\"outcome\":{\"tag\":\"Succeeded\"},\"error\":null,\
            \\"outputs\":[{\"oiMime\":\"text/plain\",\"oiOutput\":\""
                <> out
                <> "\"}],\"warnings\":[],\"padding\":\""
                <> T.replicate 200 "x"
                <> "\"}"
        twice block =
            let (_, led) = dedupText 1 block emptyEmitLedger
                (out, _) = dedupText 2 block led
             in out
    it "sends a decode error in full on a repeat" $
        twice (cellEcho "Decode error: parse error (Failed reading builtin)")
            `shouldSatisfy` T.isInfixOf "Decode error: parse error"
    it "still contracts a squared error, which is a metric not a failure" $
        twice (cellEcho "Total squared error: 0.0")
            `shouldSatisfy` T.isInfixOf "as established turn"
    it "sends an uncaught exception in full" $
        twice (cellEcho "*** Exception: divide by zero")
            `shouldSatisfy` T.isInfixOf "divide by zero"
    it "contracts ordinary informational output" $
        twice (cellEcho "Loaded rows: 178")
            `shouldSatisfy` T.isInfixOf "as established turn"
