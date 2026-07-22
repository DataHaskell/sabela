{-# LANGUAGE OverloadedStrings #-}

{- | R3.10/P6: no card field may carry a version-qualified GHC unit label
(@dataframe-core-2.0.0.0:internal@) — the run-20260720-195038 withholding
defect. The scrub keys on the version-qualified evidence class, never a
library name; plain package names and public sublib refs pass untouched.
-}
module Test.DiscoverUnitScrubSpec (discoverUnitScrubSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Classify (sessionAnswer)
import Siza.Agent.Discover.Types (
    DHit (..),
    Interpreted (..),
    SourceAnswer (..),
 )
import Siza.Agent.Discover.UnitName (scrubCardUnits, unitPackageName)

interp :: Interpreted
interp = Interpreted "col" "col" Nothing "name" "" []

-- | The exact card shape the withheld run's D.col envelope carried.
leakCard :: Value
leakCard =
    object
        [ "status" .= ("hidden-package" :: Text)
        , "module" .= ("DataFrame.Internal.Expression" :: Text)
        , "package" .= ("dataframe-core-2.0.0.0:internal" :: Text)
        , "cabal"
            .= ("-- cabal: build-depends: dataframe-core-2.0.0.0:internal" :: Text)
        ]

-- | The lint's predicate (Eval.TranscriptLint.versionQualified), re-stated.
versionQualified :: Text -> Bool
versionQualified w = case T.breakOn ":" w of
    (pre, post) -> not (T.null (T.drop 1 post)) && versioned pre
  where
    versioned pre = case reverse (T.splitOn "-" pre) of
        (v : _ : _) ->
            not (T.null v)
                && T.count "." v >= 1
                && T.all (\c -> isDigit c || c == '.') v
        _ -> False

textField :: Text -> Value -> Text
textField k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
textField _ _ = ""

discoverUnitScrubSpec :: Spec
discoverUnitScrubSpec = describe "card unit-name scrub (R3.10/P6)" $ do
    describe "unitPackageName" $ do
        it "collapses a version-qualified private-sublib unit" $
            unitPackageName "dataframe-core-2.0.0.0:internal"
                `shouldBe` "dataframe-core"
        it "collapses a bare version-qualified unit" $
            unitPackageName "blaze-html-0.9.1.2" `shouldBe` "blaze-html"
        it "leaves a plain package name untouched" $
            unitPackageName "streaming" `shouldBe` "streaming"
        it "leaves an unversioned public-sublib ref untouched" $
            unitPackageName "attoparsec:internal" `shouldBe` "attoparsec:internal"

    describe "the hidden-package card seam" $ do
        let ans = sessionAnswer interp (Just leakCard)
        it "scrubs the card's package field to the installable name" $
            fmap (textField "package") (saCard ans) `shouldBe` Just "dataframe-core"
        it "scrubs the card's cabal action into a usable dep line" $
            fmap (textField "cabal") (saCard ans)
                `shouldBe` Just "-- cabal: build-depends: dataframe-core"
        it "the hidden hit carries the scrubbed package and cabal line" $ do
            map dhPackage (saHits ans) `shouldBe` ["dataframe-core"]
            map dhCabal (saHits ans)
                `shouldBe` [Just "-- cabal: build-depends: dataframe-core"]

    describe "general invariant over generated unit labels" $
        it "no scrubbed card token is version-qualified; plain cards unchanged" $ do
            let pkgs = ["alpha", "beta-lib", "gamma-core", "delta-x"]
                vers = ["1.0", "2.0.0.0", "0.9.1.2", "10.3"]
                subs = ["", ":internal", ":impl", ":private-core"]
            forM_ [(p, ver, s) | p <- pkgs, ver <- vers, s <- subs] $
                \(p, ver, s) -> do
                    let unit = p <> "-" <> ver <> s
                        card =
                            object
                                [ "status" .= ("hidden-package" :: Text)
                                , "module" .= ("M" :: Text)
                                , "package" .= unit
                                , "cabal"
                                    .= ("-- cabal: build-depends: " <> unit)
                                ]
                        scrubbed = scrubCardUnits card
                    textField "package" scrubbed `shouldBe` p
                    forM_ (T.words (textField "cabal" scrubbed)) $ \w ->
                        w `shouldSatisfy` (not . versionQualified)
                    -- A plain-name card is a fixed point of the scrub.
                    let plain =
                            object
                                [ "status" .= ("hidden-package" :: Text)
                                , "package" .= p
                                , "cabal" .= ("-- cabal: build-depends: " <> p)
                                ]
                    scrubCardUnits plain `shouldBe` plain
