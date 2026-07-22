{-# LANGUAGE OverloadedStrings #-}

{- | The prose ladder's package-scoped rescue (search-api.md section 4 step
4): a free-text query naming a package can never silently lose the package
term to a widening stage — the run-085948 zero-granite-rows regression.
-}
module Test.HoogleProseSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoogleProse (
    hoogleQueryWith,
    isPackageRow,
    packageScopedQueries,
 )
import Sabela.AI.HoogleResolve (HoogleHit (..))

-- | A symbol hit in the named package's module.
hit :: Text -> Text -> Text -> Text -> HoogleHit
hit n p m ty = HoogleHit n p m ty ""

-- | The hoogle row shape for @package NAME@ (empty module, no signature).
pkgRow :: Text -> HoogleHit
pkgRow p = HoogleHit p p "" "" ""

barsHit :: HoogleHit
barsHit = hit "bars" "granite" "Granite.Svg" "[(Text, Double)] -> Plot -> Text"

chartHit :: HoogleHit
chartHit = hit "chartToScene" "granite" "Granite.Render.Pipeline" "Chart -> Scene"

foreignWall :: [HoogleHit]
foreignWall =
    [ hit "bars" "chart-svg" "Chart.Bar" "BarOptions -> BarData -> [Chart]"
    , hit "Bar" "plotlyhs" "Graphics.Plotly" "TraceType"
    ]

{- | The run-085948 world: the full phrase misses, the bigram windows only
reach foreign packages, and the named package's rows exist solely under the
package-scoped spellings.
-}
scriptedRun :: Int -> Text -> IO [HoogleHit]
scriptedRun _ q = pure $ case T.strip q of
    "granite" -> [pkgRow "granite", hit "Granite" "granite" "Granite" ""]
    "bar chart" -> foreignWall
    "+granite bar" -> [barsHit]
    "+granite chart" -> [chartHit]
    _ -> []

-- | A world where no query term names a package: the ladder is unchanged.
noPkgRun :: Int -> Text -> IO [HoogleHit]
noPkgRun _ q = pure $ case T.strip q of
    "bar chart" -> foreignWall
    _ -> []

spec :: Spec
spec = describe "prose package-scoped rescue (run-085948 regression)" $ do
    it "a prose query naming a package keeps that package's rows" $ do
        hits <- hoogleQueryWith scriptedRun 8 "bar chart granite"
        map hhName hits `shouldContain` ["bars"]
        [hhPackage h | h <- hits, hhName h == "bars"] `shouldBe` ["granite"]

    it "the scoped stage outranks the foreign-package bigram wall" $ do
        hits <- hoogleQueryWith scriptedRun 8 "bar chart granite"
        -- Every scoped-rescue row honours the named package; the bigram
        -- wall that lost the term must not be the answer.
        map hhPackage hits `shouldSatisfy` all (== "granite")

    it "without a package-named term the ladder is unchanged" $ do
        hits <- hoogleQueryWith noPkgRun 8 "bar chart granite"
        map hhPackage hits `shouldBe` map hhPackage foreignWall

    it "single-token and type queries never enter the rescue" $ do
        one <- hoogleQueryWith scriptedRun 8 "granite"
        map hhName one `shouldBe` ["granite", "Granite"]
        typed <- hoogleQueryWith scriptedRun 8 "Text -> Plot"
        typed `shouldBe` []

    it "packageScopedQueries scopes every other term, phrase first" $ do
        packageScopedQueries ["granite"] ["bar", "chart", "granite"]
            `shouldBe` ["+granite bar chart", "+granite bar", "+granite chart"]
        packageScopedQueries [] ["bar", "chart"] `shouldBe` []

    it "isPackageRow keys on the package-row shape, not the name" $ do
        isPackageRow "granite" (pkgRow "granite") `shouldBe` True
        isPackageRow "granite" barsHit `shouldBe` False
        isPackageRow "chart" (pkgRow "granite") `shouldBe` False
