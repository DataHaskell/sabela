{-# LANGUAGE OverloadedStrings #-}

{- | R8-T1 (search-api.md 10): load-bearing fields are elision-exempt — the
generated envelope grid, the run-20260720-181807 barChart stub fixture, and
the bound-sheds-hits (never protected fields) property.
-}
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
import Siza.Agent.Tools (renderOutcome)
import Test.DiscoverFixtures (hitsOf, textField)

-- | A signature comfortably above 'blockFloor' (the elision-risk class).
longSig :: Text
longSig =
    T.intercalate " -> " (replicate 8 "Maybe (Either Text Double)")
        <> " -> [(Text, Double)] -> Plot"

hitJ :: Text -> Text -> Text -> Value
hitJ name kind ty =
    object
        [ "name" .= name
        , "module" .= ("Cumulus.Plot" :: Text)
        , "package" .= ("cumulus" :: Text)
        , "version" .= ("0.3.1" :: Text)
        , "install" .= ("hidden" :: Text)
        , "matchKind" .= kind
        , "origin" .= ("hoogle" :: Text)
        , "type" .= ty
        , "cabal" .= ("-- cabal: build-depends: cumulus" :: Text)
        ]

foundE :: Text -> Text -> Value
foundE kind q =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "hits" .= [hitJ "bars" kind longSig]
        , "shown" .= (1 :: Int)
        , "omitted" .= (0 :: Int)
        , "total" .= (1 :: Int)
        ]

missE :: Text -> Value
missE q =
    object
        [ "query" .= q
        , "state" .= ("not_found" :: Text)
        , "next" .= next
        ]
  where
    next =
        "No match held anywhere consulted. "
            <> T.unwords (replicate 8 "Narrow with module= or package= or act on held facts.")

cardE :: Text -> Value
cardE q =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "card"
            .= object
                [ "module" .= ("Cumulus.Plot" :: Text)
                , "status" .= ("hidden-package" :: Text)
                , "exports" .= ["bars :: " <> longSig, "cols :: " <> longSig]
                ]
        ]

constructE :: Text -> Value
constructE q =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "hits"
            .= [ Object
                    ( km (hitJ "defaultPlot" "type" longSig)
                        <> KM.fromList
                            [
                                ( "use"
                                , String
                                    ( "produces Plot, the argument bars needs. "
                                        <> T.unwords (replicate 6 "Apply it before rendering the chart output.")
                                    )
                                )
                            ]
                    )
               ]
        ]
  where
    km (Object o) = o
    km _ = KM.empty

dupE :: Text -> Value
dupE q =
    object
        [ "query" .= q
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ("call 3" :: Text)
        , "summary"
            .= T.unwords
                (replicate 10 "same ranked answer; your query change did not change it.")
        ]

classes :: [(String, Text -> Value)]
classes =
    [ ("found-exact", foundE "exact")
    , ("found-weak", foundE "substring")
    , ("miss", missE)
    , ("card", cardE)
    , ("construct", constructE)
    , ("duplicate", dupE)
    ]

render :: Value -> Text
render = renderOutcome . Right . ToolOk

prose :: Text
prose =
    "The catalogue was consulted across every backend this session. "
        <> T.unwords
            (replicate 10 "This paragraph is summarisable prose and may dedup freely.")

-- | Every string value sitting under a load-bearing key, anywhere in v.
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

esc :: Text -> Text
esc t = T.dropEnd 1 (T.drop 1 (TE.decodeUtf8 (LBS.toStrict (encode t))))

occursIn :: Text -> Text -> Bool
occursIn v emission = v `T.isInfixOf` emission || esc v `T.isInfixOf` emission

runSeq :: [Text] -> [Text]
runSeq = go 1 emptyEmitLedger
  where
    go _ _ [] = []
    go turn led (c : cs) =
        let (c', led') = dedupText turn c led
         in c' : go (turn + 1) led' cs

wholeReplacement :: Text -> Bool
wholeReplacement oc =
    "[as established turn " `T.isPrefixOf` oc
        || "[changed since turn " `T.isPrefixOf` oc

{- | The invariant: in every emission, each chunk either survives with all its
protected values byte-complete, or is a whole-chunk back-reference\/diff (the
honest whole-envelope repeat).
-}
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
emitLedgerProtectSpec = describe "load-bearing fields are elision-exempt (R8-T1)" $ do
    it "the load-bearing key set is the section 10 contract" $
        forM_ ["type", "signature", "use", "cabal", "name", "next", "exports"] $
            \k -> loadBearingKeys `shouldSatisfy` elem k

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
