{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of @search_capability@: parsing the local hoogle @--json@
blob into rich hits (name/type/module/package/docs) and the JSON shaping into
@{query, hits:[...]}@. Empty/garbage input yields an OK outcome with no hits.
The IO shell (the actual @hoogle@ shell-out) is exercised in the live smoke,
not here; these pin the shaping decisions. No network.
-}
module Test.CapabilitySearchSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.CapabilityHelper (
    HelperHit (..),
    helperToHoogleHits,
    parseHelperHits,
 )
import Sabela.AI.Capabilities.CapabilitySearch (
    capabilityOutcome,
    exactOnlyHits,
    semanticWanted,
 )
import Sabela.AI.HoogleResolve (
    HoogleHit (..),
    bigrams,
    denoise,
    isSingleToken,
    isTypeOrName,
    keywords,
    parseHoogleBlob,
    rankHits,
    roundRobin,
 )
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Test.Hspec

{- | A hoogle @search --json@ blob with a real value hit (type after @::@, HTML
docs), a near hit in a different package, and an @.Internal@ hit to be dropped.
-}
jsonBlob :: Text
jsonBlob =
    T.concat
        [ "[{\"item\":\"count :: Text -> Text -> Int\""
        , ",\"docs\":\"<i>O(n+m)</i> The <a>count</a> function returns the number.\""
        , ",\"module\":{\"name\":\"Data.Text\"}"
        , ",\"package\":{\"name\":\"text\"}},"
        , "{\"item\":\"count :: Text -> Text -> Int\""
        , ",\"docs\":\"counts things\""
        , ",\"module\":{\"name\":\"RIO.Text.Partial\"}"
        , ",\"package\":{\"name\":\"rio\"}},"
        , "{\"item\":\"count :: a\""
        , ",\"module\":{\"name\":\"Data.Text.Internal\"}"
        , ",\"package\":{\"name\":\"text\"}}]"
        ]

hitsField :: ToolOutcome -> Value
hitsField o = case toolOutcomeValue o of
    Object m -> fromMaybe Null (KM.lookup "hits" m)
    _ -> Null

spec :: Spec
spec = describe "Sabela.AI.Capabilities.CapabilitySearch" $ do
    describe "parseHoogleBlob (rich hits)" $ do
        it "parses name, type, module, package, and stripped docs" $ do
            let hits = parseHoogleBlob jsonBlob
            map hhName hits `shouldBe` ["count", "count", "count"]
            map hhType hits
                `shouldBe` ["Text -> Text -> Int", "Text -> Text -> Int", "a"]
            map hhPackage hits `shouldBe` ["text", "rio", "text"]
            hhDocs (head hits)
                `shouldBe` "O(n+m) The count function returns the number."

        it "is empty on a hoogle no-database error line" $
            parseHoogleBlob "Error, database does not exist" `shouldBe` []

        it "is empty on malformed JSON" $
            parseHoogleBlob "{not json" `shouldBe` []

    describe "rankHits" $ do
        it "preserves hoogle's order and drops .Internal modules" $ do
            let ranked = rankHits (parseHoogleBlob jsonBlob)
            map hhModule ranked `shouldBe` ["Data.Text", "RIO.Text.Partial"]

    describe "capabilityOutcome" $ do
        it "shapes hits into {query, hits:[{name,type,module,package,docs}]}" $ do
            let out = capabilityOutcome "diff strings" (rankHits (parseHoogleBlob jsonBlob))
            out `shouldSatisfy` isOk
            hitsField out
                `shouldBe` toJSON
                    [ object
                        [ "name" .= ("count" :: Text)
                        , "type" .= ("Text -> Text -> Int" :: Text)
                        , "module" .= ("Data.Text" :: Text)
                        , "package" .= ("text" :: Text)
                        , "docs" .= ("O(n+m) The count function returns the number." :: Text)
                        ]
                    , object
                        [ "name" .= ("count" :: Text)
                        , "type" .= ("Text -> Text -> Int" :: Text)
                        , "module" .= ("RIO.Text.Partial" :: Text)
                        , "package" .= ("rio" :: Text)
                        , "docs" .= ("counts things" :: Text)
                        ]
                    ]

        it "truncates docs to ~160 chars with an ellipsis" $ do
            let longDocs = T.replicate 300 "x"
                hit = HoogleHit "f" "pkg" "Mod" "Int" longDocs
                out = capabilityOutcome "q" [hit]
            case hitsField out of
                Array v
                    | [Object h] <- toList v
                    , Just (String d) <- KM.lookup "docs" h ->
                        T.length d `shouldBe` 161
                other -> expectationFailure ("unexpected hits shape: " <> show other)

        it "is an OK outcome with empty hits on no results (no crash)" $ do
            let out = capabilityOutcome "nothing here" (rankHits (parseHoogleBlob "garbage"))
            out
                `shouldBe` ToolOk (object ["query" .= ("nothing here" :: Text), "hits" .= ([] :: [Value])])

    describe "parseHelperHits (SHIP helper JSON)" $ do
        it "parses a JSON array of {package,synopsis,score} rows in order" $ do
            let blob =
                    "[{\"package\":\"geohash\",\"synopsis\":\"Geohash encoding\",\"score\":1.23},\
                    \{\"package\":\"qrcode-core\",\"synopsis\":\"QR codes\",\"score\":0.9}]"
                hits = parseHelperHits blob
            map hePackage hits `shouldBe` ["geohash", "qrcode-core"]
            map heSynopsis hits `shouldBe` ["Geohash encoding", "QR codes"]
            map heScore hits `shouldBe` [1.23, 0.9]

        it "treats the graceful-degrade empty array as no hits" $
            parseHelperHits "[]" `shouldBe` []

        it "drops rows without a package name" $ do
            let blob = "[{\"synopsis\":\"x\",\"score\":1},{\"package\":\"yaml\",\"score\":2}]"
            map hePackage (parseHelperHits blob) `shouldBe` ["yaml"]

        it "tolerates missing synopsis/score (defaults empty/0)" $
            parseHelperHits "[{\"package\":\"split\"}]"
                `shouldBe` [HelperHit "split" "" 0]

        it "is empty on malformed output (so the caller falls back)" $ do
            parseHelperHits "not json" `shouldBe` []
            parseHelperHits "" `shouldBe` []

    describe "helperToHoogleHits" $
        it "maps package->name+package and synopsis->docs; type/module empty" $
            helperToHoogleHits [HelperHit "geohash" "Geohash encoding" 1.0]
                `shouldBe` [HoogleHit "geohash" "geohash" "" "" "Geohash encoding"]

    describe "capabilityOutcome over helper hits" $
        it "shapes SHIP package hits into the {query,hits:[...]} wire shape" $ do
            let hits = helperToHoogleHits [HelperHit "geohash" "Geohash encoding" 1.0]
                out = capabilityOutcome "lat/long prefix code" hits
            out `shouldSatisfy` isOk
            hitsField out
                `shouldBe` toJSON
                    [ object
                        [ "name" .= ("geohash" :: Text)
                        , "type" .= ("" :: Text)
                        , "module" .= ("" :: Text)
                        , "package" .= ("geohash" :: Text)
                        , "docs" .= ("Geohash encoding" :: Text)
                        ]
                    ]

    describe "stage-0 exact flag and the semantic lever (search-api sections 2, 7)" $ do
        it "exactOnlyHits keeps only name- or module-equal hits" $ do
            let hits = parseHoogleBlob jsonBlob
            map hhModule (exactOnlyHits "count" hits)
                `shouldBe` ["Data.Text", "RIO.Text.Partial", "Data.Text.Internal"]
            exactOnlyHits "coun" hits `shouldBe` []
            map hhName (exactOnlyHits "Data.Text" hits) `shouldBe` ["count"]
        it "semanticWanted defaults ON and only an explicit false disables it" $ do
            semanticWanted (object ["query" .= ("q" :: Text)]) `shouldBe` True
            semanticWanted (object ["semantic" .= True]) `shouldBe` True
            semanticWanted (object ["semantic" .= False]) `shouldBe` False

    describe "free-text fallback helpers" $ do
        it "treats a type signature / name query as verbatim (no fallback)" $ do
            isTypeOrName "Text -> Text -> Int" `shouldBe` True
            isTypeOrName "f :: a -> b" `shouldBe` True
            isTypeOrName "levenshtein" `shouldBe` False

        it "drops English stop-words from a description, keeping salient keywords" $
            keywords "compute how different two strings are"
                `shouldBe` ["compute", "different", "strings"]

        it "never empties an all-stop-word query" $
            keywords "how to be" `shouldBe` ["how", "to", "be"]

        it "forms adjacent salient bigrams for the widening fallback" $
            bigrams ["parse", "semantic", "version"]
                `shouldBe` ["parse semantic", "semantic version"]

        it "round-robins keyword result lists so no source floods the top" $
            roundRobin [[1, 2, 3], [4, 5], [6 :: Int]]
                `shouldBe` [1, 4, 6, 2, 5, 3]

    describe "denoise (free-text query cleaning)" $ do
        it "strips the language word so the domain word surfaces the package" $
            denoise "Geohash Haskell" `shouldBe` "geohash"

        it "strips meta tokens (library/package/module/ghc/hackage)" $ do
            denoise "qr code library" `shouldBe` "qr code"
            denoise "yaml package" `shouldBe` "yaml"
            denoise "parser module ghc hackage" `shouldBe` "parser"

        it "drops multi-word meta phrases" $ do
            denoise "how to encode an image in haskell" `shouldBe` "encode an image"
            denoise "i want to parse json" `shouldBe` "parse json"

        it "preserves domain words it must never strip" $ do
            denoise "geohash" `shouldBe` "geohash"
            denoise "encode an image as qr" `shouldBe` "encode an image as qr"
            denoise "parse yaml entity" `shouldBe` "parse yaml entity"

        it "leaves a multi-word query untouched when nothing is noise" $
            denoise "compute how different two strings are"
                `shouldBe` "compute how different two strings are"

        it "never empties an all-noise query" $
            denoise "haskell library" `shouldBe` "haskell library"

        it "treats a single-token name query as verbatim (no cleaning path)" $ do
            isSingleToken "geohash" `shouldBe` True
            isSingleToken "Data.Text" `shouldBe` True
            isSingleToken "two words" `shouldBe` False

        it "leaves a type signature untouched (run verbatim)" $ do
            isTypeOrName "Text -> Text -> Int" `shouldBe` True
            isTypeOrName "a -> b -> c" `shouldBe` True
  where
    isOk ToolOk{} = True
    isOk _ = False
