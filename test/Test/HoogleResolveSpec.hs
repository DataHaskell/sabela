{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of the Phase-0.2 local-Hoogle resolver: parse the hoogle
CLI blob into hits, rank an exact-name match to a (package, module), and the
'addImport' rewrite that feeds the repair loop. The IO shell is verified live
against a generated database; these pin the decisions it makes. No network.
-}
module Test.HoogleResolveSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.HoogleResolve (
    HoogleHit (..),
    parseHoogleBlob,
    rankResolve,
    rankResolveTopK,
 )
import Sabela.AI.ImportRepair (addImport)
import Sabela.Diagnose (notInScopeName)
import Test.Hspec

{- | A hoogle @search --json@ array blob: an exact @runConduit@ hit in conduit,
a near-miss in a different module, and an @.Internal@ hit that must be dropped.
-}
jsonBlob :: Text
jsonBlob =
    T.concat
        [ "[{\"item\":\"runConduit :: Monad m => ConduitT () Void m r -> m r\""
        , ",\"module\":{\"name\":\"Conduit\"}"
        , ",\"package\":{\"name\":\"conduit\"}},"
        , "{\"item\":\"runConduitRes :: a\""
        , ",\"module\":{\"name\":\"Conduit\"}"
        , ",\"package\":{\"name\":\"conduit\"}},"
        , "{\"item\":\"runConduit :: a\""
        , ",\"module\":{\"name\":\"Data.Conduit.Internal\"}"
        , ",\"package\":{\"name\":\"conduit\"}}]"
        ]

-- | The same three hits as JSONL (one JSON object per line).
jsonlBlob :: Text
jsonlBlob =
    T.unlines
        [ "{\"item\":\"runConduit :: Monad m => ConduitT () Void m r -> m r\",\"module\":{\"name\":\"Conduit\"},\"package\":{\"name\":\"conduit\"}}"
        , "{\"item\":\"runConduitRes :: a\",\"module\":{\"name\":\"Conduit\"},\"package\":{\"name\":\"conduit\"}}"
        , "{\"item\":\"runConduit :: a\",\"module\":{\"name\":\"Data.Conduit.Internal\"},\"package\":{\"name\":\"conduit\"}}"
        ]

{- | An ambiguous @decode@: a well-known ecosystem hit (aeson) alongside a
niche, long-hyphenated package (pusher-http-haskell) and an .Internal hit.
-}
ambiguousBlob :: Text
ambiguousBlob =
    T.concat
        [ "[{\"item\":\"decode :: FromJSON a => ByteString -> Maybe a\""
        , ",\"module\":{\"name\":\"Data.Aeson\"}"
        , ",\"package\":{\"name\":\"aeson\"}},"
        , "{\"item\":\"decode :: a\""
        , ",\"module\":{\"name\":\"Network.Pusher.Internal\"}"
        , ",\"package\":{\"name\":\"pusher-http-haskell\"}},"
        , "{\"item\":\"decode :: a\""
        , ",\"module\":{\"name\":\"Network.Pusher\"}"
        , ",\"package\":{\"name\":\"pusher-http-haskell\"}}]"
        ]

spec :: Spec
spec = describe "Sabela.AI.HoogleResolve" $ do
    describe "parseHoogleBlob" $ do
        it "parses a JSON array into name/package/module hits" $ do
            let hits = parseHoogleBlob jsonBlob
            map hhName hits `shouldBe` ["runConduit", "runConduitRes", "runConduit"]
            map hhPackage hits `shouldBe` ["conduit", "conduit", "conduit"]
            map hhModule hits
                `shouldBe` ["Conduit", "Conduit", "Data.Conduit.Internal"]

        it "parses JSONL (one object per line) the same way" $
            parseHoogleBlob jsonlBlob `shouldBe` parseHoogleBlob jsonBlob

        it "is empty on a hoogle no-database error line" $
            parseHoogleBlob "Error, database does not exist (run 'hoogle generate' first)"
                `shouldBe` []

        it "is empty on malformed JSON" $
            parseHoogleBlob "{not json" `shouldBe` []

    describe "rankResolve" $ do
        it "picks the exact-name hit and its (package, module), dropping .Internal" $
            rankResolve "runConduit" (parseHoogleBlob jsonBlob)
                `shouldBe` Just ("conduit", "Conduit")

        it "is Nothing when no hit's name equals the query exactly" $
            rankResolve "noSuchName" (parseHoogleBlob jsonBlob)
                `shouldBe` Nothing

        it "is Nothing on no hits" $
            rankResolve "runConduit" [] `shouldBe` Nothing

        it "is Nothing when the only exact hit is an .Internal module" $
            rankResolve
                "foo"
                [HoogleHit "foo" "pkg" "Data.Foo.Internal" "" ""]
                `shouldBe` Nothing

    describe "addImport" $ do
        it "inserts an import after existing imports" $
            addImport "Conduit" "import Data.Text\nx = 1"
                `shouldBe` "import Data.Text\nimport Conduit\nx = 1"

        it "is a no-op when the import is already present" $
            addImport "Conduit" "import Conduit\nx = 1"
                `shouldBe` "import Conduit\nx = 1"

        it "is idempotent across repeated calls" $
            addImport "Conduit" (addImport "Conduit" "import Data.Text\nx = 1")
                `shouldBe` addImport "Conduit" "import Data.Text\nx = 1"

        it "inserts after the -- cabal: line when the cell has no imports" $
            addImport "Conduit" "-- cabal: build-depends: conduit\nx = 1"
                `shouldBe` "-- cabal: build-depends: conduit\nimport Conduit\nx = 1"

        it "leaves a qualified import of the same module alone" $
            addImport "Conduit" "import qualified Conduit as C\nx = 1"
                `shouldBe` "import qualified Conduit as C\nx = 1"

    describe "rankResolveTopK" $ do
        it "returns an ordered shortlist of exact-name candidates" $
            rankResolveTopK 3 "runConduit" (parseHoogleBlob jsonBlob)
                `shouldBe` [("conduit", "Conduit")]

        it "drops .Internal modules from the shortlist" $
            map snd (rankResolveTopK 3 "decode" (parseHoogleBlob ambiguousBlob))
                `shouldNotContain` ["Network.Pusher.Internal"]

        it "ranks a well-known ecosystem package above a niche one" $
            take 1 (rankResolveTopK 3 "decode" (parseHoogleBlob ambiguousBlob))
                `shouldBe` [("aeson", "Data.Aeson")]

        it "is empty when no hit's name matches exactly" $
            rankResolveTopK 3 "noSuchName" (parseHoogleBlob ambiguousBlob)
                `shouldBe` []

        it "caps the shortlist at K" $
            length (rankResolveTopK 1 "decode" (parseHoogleBlob ambiguousBlob))
                `shouldBe` 1

    describe "notInScopeName feeds the resolver" $
        it "extracts the name the resolver then queries" $
            notInScopeName "Variable not in scope: runConduit :: ConduitT a b m r"
                `shouldBe` Just "runConduit"
