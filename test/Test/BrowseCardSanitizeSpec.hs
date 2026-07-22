{-# LANGUAGE OverloadedStrings #-}

{- | R3.10 whole-card property (R6-T1): no version-qualified and no
@ghc-internal@ token in ANY rendered browse card — over generated APIs and
over the run-20260720-130012 @Data.Data@ leak reproduced verbatim.
-}
module Test.BrowseCardSanitizeSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.AI.Capabilities.BrowseCard (browseCard)

-- | Serialised card text, for whole-card token scans.
cardText :: Value -> Text
cardText = TE.decodeUtf8 . LBS.toStrict . encode

{- | A version-qualified token (@pkg-1.2.3:Rest@) anywhere in the text: the
R3.10 forbidden shape, checked structurally rather than by any package name.
-}
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

-- | Synthetic unit-qualified browse output for one (package, version).
generatedApi :: Text -> Text -> Text
generatedApi pkg ver =
    T.unlines
        [ "type (" <> u "Gale.Internal.Wind.:~:" <> ") :: forall {k}."
        , u "Gale.Internal.Wind.Refl" <> " :: forall {k}"
        , "type " <> u "Gale.Internal.Wind.Speed" <> " :: *"
        , "= "
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

-- | The run-20260720-130012 leak, verbatim raw lines from the transcript.
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
    it "counts are unchanged by sanitisation (R3.4)" $ do
        let raw = generatedApi "zephyr" "1.2.3"
            card = browseCard "Gale.Wind" raw
            nonBlank = length (filter (not . T.null . T.strip) (T.lines raw))
        cardText card `shouldSatisfy` T.isInfixOf ("\"total\":" <> tShow nonBlank)
  where
    tShow = T.pack . show
