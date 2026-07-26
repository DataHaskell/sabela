{-# LANGUAGE OverloadedStrings #-}

module Test.HoogleProseSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoogleIntent (ActionClass (RenderAction), actionNeedQueries)
import Sabela.AI.HoogleProse (
    hoogleQueryWith,
    isPackageRow,
    packageScopedQueries,
 )
import Sabela.AI.HoogleResolve (HoogleHit (..))

hit :: Text -> Text -> Text -> Text -> HoogleHit
hit n p m ty = HoogleHit n p m ty ""

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

scriptedRun :: Int -> Text -> IO [HoogleHit]
scriptedRun _ q = pure $ case T.strip q of
    "granite" -> [pkgRow "granite", hit "Granite" "granite" "Granite" ""]
    "bar chart" -> foreignWall
    "+granite bar" -> [barsHit]
    "+granite chart" -> [chartHit]
    _ -> []

noPkgRun :: Int -> Text -> IO [HoogleHit]
noPkgRun _ q = pure $ case T.strip q of
    "bar chart" -> foreignWall
    _ -> []

spec :: Spec
spec = packageRescueSpec >> sineSpec

packageRescueSpec :: Spec
packageRescueSpec = describe "prose package-scoped rescue (run-085948 regression)" $ do
    it "a prose query naming a package keeps that package's rows" $ do
        hits <- hoogleQueryWith scriptedRun 8 "bar chart granite"
        map hhName hits `shouldContain` ["bars"]
        [hhPackage h | h <- hits, hhName h == "bars"] `shouldBe` ["granite"]

    it "the scoped stage outranks the foreign-package bigram wall" $ do
        hits <- hoogleQueryWith scriptedRun 8 "bar chart granite"
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

sineSpec :: Spec
sineSpec = describe "action-need stage (intent-blind regression)" $ do
    it "a plotting request finds the chart library, never the isolated object noun" $ do
        hits <- hoogleQueryWith sineRun 8 "plot a sine wave"
        map hhPackage hits `shouldBe` ["chart-svg"]
        map hhName hits `shouldNotContain` ["sine"]

    it "the action-need queries actually tried are RenderAction's" $
        actionNeedQueries RenderAction `shouldBe` ["chart library", "SVG rendering"]
  where
    chartLibHit = hit "linePlot" "chart-svg" "Chart.Line" "[Double] -> Svg"
    sineOscillatorHit =
        hit "sine" "tidal" "Sound.Tidal.Boot" "Time -> Signal Double"
    sineRun :: Int -> Text -> IO [HoogleHit]
    sineRun _ q = pure $ case T.strip (T.toLower q) of
        "chart library" -> [chartLibHit]
        "sine" -> [sineOscillatorHit]
        _ -> []
