{-# LANGUAGE OverloadedStrings #-}

module Test.BrowseCardSanitizeSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Capabilities.BrowseCard (browseCard)
import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.AI.LeakShape (leakyToken)

cardText :: Value -> Text
cardText = TE.decodeUtf8 . LBS.toStrict . encode

hasVersionQualified :: Text -> Bool
hasVersionQualified t = any versionColon (T.words (T.map depunct t))
  where
    depunct c = if c `elem` ("(){}[]\",=" :: String) then ' ' else c
    versionColon w = case T.breakOn ":" w of
        (pre, post) -> not (T.null post) && versionSuffixed pre
    versionSuffixed pre = case reverse (T.splitOn "-" pre) of
        (v : _ : _) ->
            not (T.null v) && T.all (\c -> isDigit c || c == '.') v
        _ -> False

{- | A @:browse@ listing as GHCi renders one: unit-qualified names, and a
record body written as an indented continuation of its @data@ line.
-}
generatedApi :: Text -> Text -> Text
generatedApi pkg ver =
    T.unlines
        [ "type (" <> u "Gale.Internal.Wind.:~:" <> ") :: forall {k}."
        , u "Gale.Internal.Wind.Refl" <> " :: forall {k}"
        , "type " <> u "Gale.Internal.Wind.Speed" <> " :: *"
        , "data " <> u "Gale.Internal.Wind.Speed"
        , "  = "
            <> u "Gale.Internal.Wind.Speed"
            <> " {"
            <> u "Gale.Internal.Wind.knots"
            <> " :: Int}"
        , u "Gale.Internal.Wind.gust"
            <> " :: "
            <> u "Gale.Internal.Wind.Speed"
            <> " -> Int"
        , "plainGust :: Int -> Int"
        ]
  where
    u n = pkg <> "-" <> ver <> ":" <> n

{- | What 'generatedApi' declares: seven names over six declarations, because
the record's selector is an export in its own right.
-}
generatedApiDeclarations :: [Text]
generatedApiDeclarations =
    ["(:~:)", "Refl", "Speed", "Speed", "knots", "gust", "plainGust"]

run130012Fixture :: Text
run130012Fixture =
    T.unlines
        [ "type (ghc-internal-9.1202.0:GHC.Internal.Data.Type.Equality.:~:) :: forall {k}."
        , "ghc-internal-9.1202.0:GHC.Internal.Data.Type.Equality.Refl :: forall {k}"
        , "(a :: k)."
        , "type ghc-internal-9.1202.0:GHC.Internal.Data.Data.ConIndex :: *"
        , "type ghc-internal-9.1202.0:GHC.Internal.Data.Data.Constr :: *"
        , "= ghc-internal-9.1202.0:GHC.Internal.Data.Data.Constr {ghc-internal-9.1202.0:GHC.Internal.Data.Data.conrep :: ghc-internal-9.1202.0:GHC.Internal.Data.Data.ConstrRep,"
        , "ghc-internal-9.1202.0:GHC.Internal.Data.Data.constring :: String,"
        , "ghc-internal-9.1202.0:GHC.Internal.Data.Data.gfoldl :: (forall d b."
        , "ghc-internal-9.1202.0:GHC.Internal.Data.Data.dataCast1 :: ghc-internal-9.1202.0:GHC.Internal.Data.Typeable.Internal.Typeable"
        ]

spec :: Spec
spec = describe "browse-card R3.10 sanitizer (R6-T1)" $ do
    it "run-20260720-130012 fixture: no ghc-internal token in the card" $ do
        let card = browseCard "Data.Data" run130012Fixture
        cardText card `shouldSatisfy` (not . T.isInfixOf "ghc-internal")
    it "run-20260720-130012 fixture: no version-qualified token in the card" $ do
        let card = browseCard "Data.Data" run130012Fixture
        cardText card `shouldSatisfy` (not . hasVersionQualified)
    it "generated APIs: the property holds for every (package, version)"
        $ forM_
            [ (p, v)
            | p <- ["zephyr", "stratus-core", "ghc-internal"]
            , v <- ["1.2.3", "9.1202.0"]
            ]
        $ \(pkg, ver) -> do
            let card = browseCard "Gale.Wind" (generatedApi pkg ver)
            cardText card `shouldSatisfy` (not . hasVersionQualified)
            cardText card
                `shouldSatisfy` (not . T.isInfixOf (pkg <> "-" <> ver))
    it "sanitised exports keep their writable names" $ do
        let card = browseCard "Gale.Wind" (generatedApi "zephyr" "1.2.3")
        cardText card `shouldSatisfy` T.isInfixOf "gust ::"
        cardText card `shouldSatisfy` T.isInfixOf "plainGust :: Int -> Int"
        cardText card `shouldSatisfy` T.isInfixOf "knots :: Speed -> Int"
    it "total counts what the listing declares, not what it renders (R3.4)" $ do
        let card = browseCard "Gale.Wind" (generatedApi "zephyr" "1.2.3")
        cardText card
            `shouldSatisfy` T.isInfixOf
                ("\"total\":" <> tShow (length generatedApiDeclarations))
    onePredicateSpec
  where
    tShow = T.pack . show

{- | C1-12b: one predicate classifies build provenance. The sanitiser presents
what the leak predicate calls clean, for every unit-id shape — including the
hash-suffixed ones a version-only predicate reads as an ordinary name.
-}
onePredicateSpec :: Spec
onePredicateSpec =
    describe "the sanitiser and the leak predicate agree (C1-12b)" $ do
        prop "no token sanitizeTypeText emits is leaky" $
            forAll genQualified $ \t ->
                let out = sanitizeTypeText t
                 in counterexample (T.unpack (t <> " -> " <> out)) $
                        property (not (any leakyToken (identRuns out)))
        prop "a name the predicate calls clean is left alone" $
            forAll genClean $
                \t -> sanitizeTypeText t === t

identRuns :: Text -> [Text]
identRuns = filter (not . T.null) . T.split (not . identChar)
  where
    identChar c = isAlphaNum c || c `elem` ("._':-" :: String)

genClean :: Gen Text
genClean =
    elements
        [ "Speed"
        , "Data.Map.insertWith"
        , "gust"
        , "Int"
        , "decodeStrict'"
        ]

{- | A unit-qualified name as GHCi renders one, over every unit-id shape: with
and without a hash component, and with a version that has one or three dots.
-}
genQualified :: Gen Text
genQualified = do
    pkg <- elements ["zephyr", "stratus-core", "ghc-internal", "aeson"]
    ver <- elements ["1.2.3", "0.11", "9.1202.0"]
    hash <- elements ["", "-c1e52ef7", "-a3f9b2c1d4e5"]
    name <- genClean
    pure (pkg <> "-" <> ver <> hash <> ":" <> name)
