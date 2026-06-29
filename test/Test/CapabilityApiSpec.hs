{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of the RESULT→USE bridge: ranking a discovered package's
hoogle exports against the query keywords (so an action query surfaces the
function, not the data accessor), and shaping the enriched
@{query, hits:[{package, synopsis, cabal, modules, api:[...]}]}@ wire result.
The per-package @hoogle "+pkg ..."@ shell-out itself is exercised in the live
smoke; these pin the parsing and shaping decisions. No network.
-}
module Test.CapabilityApiSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.CapabilityApi (
    ApiFn (..),
    apiKeywords,
    isValueItem,
    rankApiFns,
    splitArrow,
    usageExample,
 )
import Sabela.AI.Capabilities.CapabilitySearch (
    enrichedOutcome,
    packageBuckets,
 )
import Sabela.AI.HoogleResolve (HoogleHit (..), parseHoogleBlob)
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Test.Hspec

{- | A real @hoogle "+qrcode-core encode" --json@-shaped blob: the encoder, two
sibling encoders, a data accessor (value with a record sig), a @data@ decl with
no signature, and a @module@ row. Ranking must put the encoder first and drop
the non-value declarations to the back.
-}
qrBlob :: Text
qrBlob =
    T.concat
        [ "[{\"item\":\"encode :: ToText a => QRCodeOptions -> TextEncoding -> a -> Maybe QRImage\""
        , ",\"docs\":\"Encode a string into a QR code.\""
        , ",\"module\":{\"name\":\"Codec.QRCode\"}"
        , ",\"package\":{\"name\":\"qrcode-core\"}},"
        , "{\"item\":\"encodeText :: ToText a => QRCodeOptions -> TextEncoding -> a -> Maybe QRImage\""
        , ",\"docs\":\"Generate a QR code representing the specified text.\""
        , ",\"module\":{\"name\":\"Codec.QRCode\"}"
        , ",\"package\":{\"name\":\"qrcode-core\"}},"
        , "{\"item\":\"qroErrorLevel :: QRCodeOptions -> ErrorLevel\""
        , ",\"docs\":\"The error level accessor.\""
        , ",\"module\":{\"name\":\"Codec.QRCode\"}"
        , ",\"package\":{\"name\":\"qrcode-core\"}},"
        , "{\"item\":\"data TextEncoding\""
        , ",\"docs\":\"How to encode text.\""
        , ",\"module\":{\"name\":\"Codec.QRCode.Data.TextEncoding\"}"
        , ",\"package\":{\"name\":\"qrcode-core\"}},"
        , "{\"item\":\"module Codec.QRCode\""
        , ",\"module\":{\"name\":\"Codec.QRCode\"}"
        , ",\"package\":{\"name\":\"qrcode-core\"}}]"
        ]

hitsField :: ToolOutcome -> Value
hitsField o = case toolOutcomeValue o of
    Object m -> fromMaybe Null (KM.lookup "hits" m)
    _ -> Null

spec :: Spec
spec = describe "Sabela.AI.Capabilities.CapabilityApi" $ do
    describe "apiKeywords" $ do
        it "drops stop-words and language/meta noise, keeps domain terms" $
            apiKeywords "generate a QR code from text in haskell"
                `shouldBe` ["generate", "qr", "code", "text"]

        it "is empty for an empty query" $
            apiKeywords "   " `shouldBe` []

    describe "isValueItem" $ do
        it "is True for a value/function (has a :: signature)" $
            isValueItem (head (parseHoogleBlob qrBlob)) `shouldBe` True

        it "is False for a data/type/class/module declaration" $ do
            let dataHit = parseHoogleBlob qrBlob !! 3
            isValueItem dataHit `shouldBe` False

    describe "rankApiFns" $ do
        it "surfaces the query-matching function first, drops non-values to back" $ do
            let kws = apiKeywords "generate a QR code from text"
                ranked = rankApiFns 6 kws (parseHoogleBlob qrBlob)
            -- encodeText and encode both match; both must precede the accessor
            -- and the data decl. The top entry is a value with the QR signature.
            afType (head ranked)
                `shouldSatisfy` ("Maybe QRImage" `T.isInfixOf`)
            map afName (take 3 ranked)
                `shouldContain` ["encode"]
            -- the data/module declarations (no signature) rank behind values
            last (map afType ranked) `shouldBe` ""
            -- every value item precedes every non-value item
            let (vals, decls) = break (T.null . afType) ranked
            all (T.null . afType) decls `shouldBe` True
            length vals `shouldBe` 3

        it "keeps at most k and dedups by (name, module)" $ do
            let hit = HoogleHit "f" "pkg" "Mod" "Int" "d"
                ranked = rankApiFns 6 [] [hit, hit, hit]
            length ranked `shouldBe` 1

        it "carries name, module, and signature" $ do
            let ranked = rankApiFns 1 ["encode"] (parseHoogleBlob qrBlob)
            ranked
                `shouldBe` [ ApiFn
                                "encode"
                                "Codec.QRCode"
                                "ToText a => QRCodeOptions -> TextEncoding -> a -> Maybe QRImage"
                           ]

    describe "splitArrow" $ do
        it "splits a flat type on top-level arrows" $
            splitArrow "QRCodeOptions -> Text -> Maybe QRImage"
                `shouldBe` ["QRCodeOptions ", " Text ", " Maybe QRImage"]
        it "keeps arrows nested inside parens/brackets intact" $
            splitArrow "(a -> b) -> [a] -> [b]"
                `shouldBe` ["(a -> b) ", " [a] ", " [b]"]

    describe "usageExample" $ do
        it "synthesizes import + a call skeleton with one typed hole per arg" $
            usageExample
                [ApiFn "encode" "Codec.QRCode" "QRCodeOptions -> Text -> Maybe QRImage"]
                `shouldBe` T.intercalate
                    "\n"
                    [ "import Codec.QRCode (encode)"
                    , "-- encode :: QRCodeOptions -> Text -> Maybe QRImage"
                    , "let result = encode (_ :: QRCodeOptions) (_ :: Text)"
                    ]
        it "drops a class context and forall before splitting args" $
            usageExample
                [ ApiFn "encode" "Codec.QRCode" "ToText a => QRCodeOptions -> a -> Maybe QRImage"
                ]
                `shouldBe` T.intercalate
                    "\n"
                    [ "import Codec.QRCode (encode)"
                    , "-- encode :: ToText a => QRCodeOptions -> a -> Maybe QRImage"
                    , "let result = encode (_ :: QRCodeOptions) (_ :: a)"
                    ]
        it "calls a no-argument value with no holes" $
            usageExample [ApiFn "stdGen" "System.Random" "StdGen"]
                `shouldBe` T.intercalate
                    "\n"
                    [ "import System.Random (stdGen)"
                    , "-- stdGen :: StdGen"
                    , "let result = stdGen"
                    ]
        it "is empty when there are no exports" $
            usageExample [] `shouldBe` ""

    describe "packageBuckets" $
        it "collapses symbol hits to per-package (pkg, synopsis) in first-seen order" $ do
            let hits =
                    [ HoogleHit "a" "geohash" "Data.Geohash" "x" "Geohash encoding"
                    , HoogleHit "b" "geohash" "Data.Geohash" "y" "second"
                    , HoogleHit "c" "split" "Data.List.Split" "z" "split lists"
                    ]
            packageBuckets hits
                `shouldBe` [("geohash", "Geohash encoding"), ("split", "split lists")]

    describe "enrichedOutcome (wire shape)" $ do
        it
            "shapes {package, synopsis, cabal, modules, api:[{name,module,type}], example}"
            $ do
                let api =
                        [ ApiFn "encode" "Data.Geohash" "Int -> (Double, Double) -> Maybe String"
                        ]
                    out = enrichedOutcome "lat/long prefix code" [("geohash", "Geohash encoding", api)]
                out `shouldSatisfy` isOk
                hitsField out
                    `shouldBe` toJSON
                        [ object
                            [ "package" .= ("geohash" :: Text)
                            , "synopsis" .= ("Geohash encoding" :: Text)
                            , "cabal" .= ("-- cabal: build-depends: geohash" :: Text)
                            , "modules" .= (["Data.Geohash"] :: [Text])
                            , "api"
                                .= [ object
                                        [ "name" .= ("encode" :: Text)
                                        , "module" .= ("Data.Geohash" :: Text)
                                        , "type" .= ("Int -> (Double, Double) -> Maybe String" :: Text)
                                        ]
                                   ]
                            , "example"
                                .= T.intercalate
                                    "\n"
                                    [ "import Data.Geohash (encode)"
                                    , "-- encode :: Int -> (Double, Double) -> Maybe String"
                                    , "let result = encode (_ :: Int) (_ :: (Double, Double))"
                                    ]
                            ]
                        ]

        it "attaches an example only to the top few packages, by the top export" $ do
            let mk n =
                    ( n
                    , "syn"
                    , [ApiFn ("f" <> n) ("M." <> n) "Int -> Int"]
                    )
                out = enrichedOutcome "q" (map (mk . T.pack . show) [1 .. 5 :: Int])
            case hitsField out of
                Array v ->
                    [KM.member "example" h | Object h <- toList v]
                        `shouldBe` [True, True, True, False, False]
                other -> expectationFailure ("unexpected hits shape: " <> show other)

        it "omits the example field when no export surfaces (empty api)" $ do
            let out = enrichedOutcome "q" [("only-data", "syn", [])]
            case hitsField out of
                Array v
                    | [Object h] <- toList v ->
                        KM.member "example" h `shouldBe` False
                other -> expectationFailure ("unexpected hits shape: " <> show other)

        it "is an OK outcome with empty hits on no results (no crash)" $ do
            let out = enrichedOutcome "nothing here" []
            out
                `shouldBe` ToolOk (object ["query" .= ("nothing here" :: Text), "hits" .= ([] :: [Value])])

        it "emits the cabal line and empty api for a package with no surfaced exports" $ do
            let out = enrichedOutcome "q" [("mystery-pkg", "syn", [])]
            case hitsField out of
                Array v
                    | [Object h] <- foldr (:) [] v -> do
                        KM.lookup "cabal" h
                            `shouldBe` Just (String "-- cabal: build-depends: mystery-pkg")
                        KM.lookup "api" h `shouldBe` Just (Array mempty)
                        KM.lookup "modules" h `shouldBe` Just (Array mempty)
                other -> expectationFailure ("unexpected hits shape: " <> show other)
  where
    isOk ToolOk{} = True
    isOk _ = False
