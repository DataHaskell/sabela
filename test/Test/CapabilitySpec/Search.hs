{-# LANGUAGE OverloadedStrings #-}

-- | searchCapabilities: what a query reaches, and how much context it returns.
module Test.CapabilitySpec.Search (searchSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    Match (..),
    defaultSynonyms,
    parseCapabilities,
    searchCapabilities,
 )
import Test.CapabilitySpec.Fixtures (cap, idx, top, via)
import Test.Hspec

searchSpec :: Spec
searchSpec = describe "Sabela.AI.Capability.searchCapabilities" $ do
    describe "composition vocabulary reaches the combining API" $ do
        it "finds group for the words a caller actually uses" $
            mapM_
                (\q -> (q, fmap snd (top q)) `shouldBe` (q, Just "group"))
                ["overlay", "superimpose", "combine", "compose"]

        it "finds the picture API for a pluralised query" $
            fmap fst (top "pictures") `shouldBe` Just "Sabela.Notebook"

    describe "a near-spelling query still reaches the name" $ do
        it "finds summarize for summary" $
            fmap snd (top "summary") `shouldBe` Just "summarize"

        it "does not fuzzy-match an unrelated name" $
            fmap snd (top "zzzznotathing") `shouldBe` Nothing

    it "a name keyword finds the function (animate)" $ do
        top "animate" `shouldBe` Just ("Sabela.Notebook.Anim", "animate")
        via "animate" `shouldBe` Just ByName

    it "a prefix finds it (line -> lineGraph)" $
        top "line" `shouldBe` Just ("Granite.Svg", "lineGraph")

    it "finds fit by exact name" $
        top "fit" `shouldBe` Just ("DataFrame.Model", "fit")

    it "a type fragment finds by signature (Double -> Picture -> animate)" $ do
        top "Double -> Picture" `shouldBe` Just ("Sabela.Notebook.Anim", "animate")
        via "Double -> Picture" `shouldBe` Just ByType

    it "a synonym bridges the vocabulary (classification -> logistic)" $ do
        top "classification"
            `shouldBe` Just ("DataFrame.LinearModel.Logistic", "defaultLogisticConfig")
        via "classification" `shouldBe` Just BySynonym

    it "a module-ish keyword surfaces that module's functions" $ do
        fmap fst (top "granite") `shouldBe` Just "Granite.Svg"
        via "granite" `shouldBe` Just ByModule

    describe "context economy — focused hits, not walls" $ do
        let wallIdx =
                cap "Synth.Osc" "osc" "Wave a => Text -> Gen a"
                    : cap "Synth.Patch.Deep.Internal.Wire" "oscillator" "Patch -> Int"
                    : [ cap
                            "Synth.Codec.Midi.Internal.Frame"
                            ("evt_osc_internal_" <> T.pack (show i))
                            "MidiEvent -> Int"
                      | i <- [1 .. 19 :: Int]
                      ]
        it "a type application in the query is noise, not a match killer" $ do
            let hits = searchCapabilities defaultSynonyms wallIdx "osc @Sine"
            (capName . hitCap <$> take 1 hits) `shouldBe` ["osc"]
            (hitVia <$> take 1 hits) `shouldBe` [ByName]
        it "a string-literal argument in the query is stripped the same way" $ do
            let hits = searchCapabilities defaultSynonyms wallIdx "osc \"a440\""
            (capName . hitCap <$> take 1 hits) `shouldBe` ["osc"]
        it "an exact hit silences the token-noise tail" $ do
            let hits = searchCapabilities defaultSynonyms wallIdx "osc @"
            (capName . hitCap <$> take 1 hits) `shouldBe` ["osc"]
            length hits `shouldSatisfy` (<= 5)
            hits
                `shouldSatisfy` ( not
                                    . any (T.isPrefixOf "evt_" . capName . hitCap)
                                )
        it "with no high-tier hit the list is capped well below a wall" $ do
            let hits = searchCapabilities defaultSynonyms wallIdx "internal"
            length hits `shouldSatisfy` (<= 8)
        it "ties prefer the SHORTER module path (public API over internals)" $ do
            let idx2 =
                    [ cap "Geo.Shape.Internal.Mesh.Raw" "areaOf" "Mesh -> Double"
                    , cap "Geo" "area" "Shape -> Double"
                    ]
                hits = searchCapabilities defaultSynonyms idx2 "area"
            (capModule . hitCap <$> take 1 hits) `shouldBe` ["Geo"]

    describe "synonyms match whole tokens, never substrings" $ do
        let idx =
                [ cap "Sound.Fx" "reverb" "Time -> Audio -> Audio"
                , cap "Mail.Send" "sendMail" "Message -> IO ()"
                ]
            syns = [("email", ["sendmail"]), ("verb", ["sendmail"])]
        it "a substring of a query word does not trigger the synonym tier" $
            searchCapabilities syns idx "reverbnation upload client"
                `shouldBe` []
        it "the synonym still fires on the whole token" $ do
            let hits = searchCapabilities syns idx "email for messages"
            (capName . hitCap <$> take 1 hits) `shouldBe` ["sendMail"]

    it "returns nothing for an unrelated query (no misleading near-miss)" $
        searchCapabilities defaultSynonyms idx "quantum teleportation" `shouldBe` []

    describe "parseCapabilities (live :browse output)" $ do
        let caps = parseCapabilities "Sabela.Notebook.Anim" animBrowse
            names = map capName caps

        it "recovers a multi-line, qualified signature that line parsing drops" $ do
            ("animate" `elem` names) `shouldBe` True
            lookup "animate" [(capName c, capType c) | c <- caps]
                `shouldBe` Just "Time -> (Time -> Picture) -> IO ()"

        it "strips the module qualifier and indexes type decls alongside values" $ do
            ("defaultAnim" `elem` names) `shouldBe` True
            ("AnimOpts" `elem` names) `shouldBe` True
            all (\c -> not ("." `T.isInfixOf` capName c)) caps `shouldBe` True

        it "the parsed index finds animate by name (the transcript failure)" $
            case searchCapabilities defaultSynonyms caps "animate" of
                (h : _) -> (capName (hitCap h), hitVia h) `shouldBe` ("animate", ByName)
                [] -> expectationFailure "no match for animate"

        it "extracts record field selectors as Record -> Field capabilities" $ do
            ("animFps" `elem` names) `shouldBe` True
            lookup "animFps" [(capName c, capType c) | c <- caps]
                `shouldBe` Just "AnimOpts -> Int"

        it "find_function can now locate a record field by name" $
            case searchCapabilities defaultSynonyms caps "animFps" of
                (h : _) -> capName (hitCap h) `shouldBe` "animFps"
                [] -> expectationFailure "no match for the record field animFps"

        it "keeps a field type carrying its own comma whole" $ do
            let recCaps =
                    parseCapabilities
                        "M"
                        "data M.Cfg = M.Cfg {M.pairs :: [(Int, Int)], M.n :: Int}"
            lookup "pairs" [(capName c, capType c) | c <- recCaps]
                `shouldBe` Just "Cfg -> [(Int, Int)]"

        it "names the record a field belongs to, unlike an ordinary function" $ do
            let recCaps =
                    parseCapabilities
                        "M"
                        "data M.Cfg = M.Cfg {M.pairs :: [(Int, Int)], M.n :: Int}"
            lookup "pairs" [(capName c, capField c) | c <- recCaps]
                `shouldBe` Just (Just "Cfg")
            lookup "animate" [(capName c, capField c) | c <- idx]
                `shouldBe` Just Nothing

        it "extracts a class method as a capability (the polymorphic verb)" $ do
            let cs =
                    parseCapabilities
                        "DataFrame.Model"
                        "class Fit cfg input model where\n  fit :: cfg -> input -> DataFrame -> model"
            ("fit" `elem` map capName cs) `shouldBe` True
            lookup "fit" [(capName c, capType c) | c <- cs]
                `shouldBe` Just "(Fit cfg input model) => cfg -> input -> DataFrame -> model"

        it "find_function locates the class method fit by name" $ do
            let cs =
                    parseCapabilities
                        "DataFrame.Model"
                        "class Fit cfg input model where\n  fit :: cfg -> input -> DataFrame -> model"
            case searchCapabilities defaultSynonyms cs "fit" of
                (h : _) -> capName (hitCap h) `shouldBe` "fit"
                [] -> expectationFailure "no match for the class method fit"

        it "splits a two-method class into both methods" $ do
            let cs =
                    parseCapabilities
                        "M"
                        "class Predict model r where\n  predict :: model -> Expr r\n  predictProba :: model -> Expr Double"
            map capName cs `shouldContain` ["predict"]
            map capName cs `shouldContain` ["predictProba"]

animBrowse :: Text
animBrowse =
    T.unlines
        [ "type Sabela.Notebook.Anim.AnimOpts :: *"
        , "data Sabela.Notebook.Anim.AnimOpts"
        , "  = Sabela.Notebook.Anim.AnimOpts {Sabela.Notebook.Anim.animCanvas :: sabela-notebook-0.1.0.0:Sabela.Notebook.Picture.Internal.Canvas,"
        , "                                   Sabela.Notebook.Anim.animFps :: Int}"
        , "Sabela.Notebook.Anim.animate ::"
        , "  Sabela.Notebook.Behavior.Time"
        , "  -> (Sabela.Notebook.Behavior.Time"
        , "      -> sabela-notebook-0.1.0.0:Sabela.Notebook.Picture.Internal.Picture)"
        , "  -> IO ()"
        , "Sabela.Notebook.Anim.defaultAnim :: Sabela.Notebook.Anim.AnimOpts"
        ]
