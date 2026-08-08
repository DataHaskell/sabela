{-# LANGUAGE OverloadedStrings #-}

module Test.HoogleProseSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoogleProse (hoogleQueryWith)
import Sabela.AI.HoogleResolve (HoogleHit (..))
import Sabela.AI.Search.Need (parseNeed)
import Sabela.AI.Search.Probe (Probe (..), expandProbes, planProbes)
import Sabela.AI.Search.Row (RowKind (..), coverage, grounded, humps, rowKind)

hit :: Text -> Text -> Text -> Text -> HoogleHit
hit n p m ty = HoogleHit n p m ty "" ""

docHit :: Text -> Text -> Text -> Text -> Text -> HoogleHit
docHit n p m ty d = HoogleHit n p m ty d ""

pkgRow :: Text -> HoogleHit
pkgRow p = HoogleHit p p "" "" "" ""

modRow :: Text -> Text -> HoogleHit
modRow p m = HoogleHit m p m "" "" ""

readParquetHit :: HoogleHit
readParquetHit =
    docHit
        "readParquet"
        "dataframe"
        "DataFrame"
        "FilePath -> IO DataFrame"
        "Read a parquet file from path and load it into a dataframe."

spec :: Spec
spec = do
    rowSpec
    probeSpec
    groundingSpec
    pivotSpec
    corroborationSpec

rowSpec :: Spec
rowSpec = describe "row kind and coverage" $ do
    it "recovers the row kind from the flattened shape" $ do
        rowKind (pkgRow "dataframe") `shouldBe` RowPackage
        rowKind (modRow "dataframe" "DataFrame.IO.Parquet") `shouldBe` RowModule
        rowKind readParquetHit `shouldBe` RowSymbol

    it "splits identifiers into camel humps" $
        humps "readParquet" `shouldBe` ["read", "parquet"]

    it "counts a docs match, so a term absent from the name still grounds" $ do
        let need = parseNeed Set.empty "dataframe"
            docsOnly =
                docHit "readParquet" "other" "Other" "" "load it into a dataframe"
        grounded need docsOnly `shouldBe` True

    -- The ceiling is what stops a row that restates a term in its own type from
    -- outscoring the plainer function; the name-length tiebreak then separates
    -- them, which "resolves a query that only Hoogle package rows answer" pins.
    it "saturates a term at the weight of an exact name match" $ do
        let need = parseNeed Set.empty "parquet"
            restated =
                docHit
                    "readParquetWithOpts"
                    "dataframe"
                    "DataFrame"
                    "ParquetReadOptions -> FilePath -> IO DataFrame"
                    "Read a parquet file from path."
        coverage need restated `shouldBe` coverage need readParquetHit

probeSpec :: Spec
probeSpec = describe "probe plan" $ do
    it "issues the whole query, its bigrams and its unigrams" $ do
        let qs =
                map probeQuery (planProbes (parseNeed Set.empty "read parquet dataframe"))
        qs `shouldContain` ["read parquet dataframe"]
        qs `shouldContain` ["read parquet"]
        qs `shouldContain` ["parquet dataframe"]
        qs `shouldContain` ["parquet"]

    it "never issues a query that is not built from the caller's own words" $ do
        let asked = "plot a sine wave"
            need = parseNeed Set.empty asked
            derived p =
                all (`elem` T.words asked) (T.words (T.toLower (probeQuery p)))
        all derived (planProbes need) `shouldBe` True

{- | The regression this suite exists for: a hit sharing no term with the query
is not an answer, however confidently the index returns it.
-}
groundingSpec :: Spec
groundingSpec = describe "grounding" $ do
    it "rejects a row that shares no term with the query" $ do
        let need = parseNeed Set.empty "read parquet dataframe"
        grounded need (hit "ImportDataCon" "ghc-lib-parser" "GHC.Types.Hint" "")
            `shouldBe` False
        grounded need (hit "SVGRenderingIntent" "jsaddle-dom" "JSDOM.Types" "")
            `shouldBe` False

    it "an ungrounded index answer never reaches the caller" $ do
        hits <- hoogleQueryWith intentJunkRun 8 "read parquet dataframe"
        map hhPackage hits `shouldNotContain` ["lambdabot-core"]
        map hhName hits `shouldBe` ["readParquet"]

    it "a plotting request never answers with an unrelated SVG type" $ do
        hits <- hoogleQueryWith sineRun 8 "plot a sine wave"
        map hhPackage hits `shouldNotContain` ["jsaddle-dom"]
        map hhName hits `shouldContain` ["plot"]

-- | A package or module row is Hoogle saying where to look, not what to use.
pivotSpec :: Spec
pivotSpec = describe "package and module pivot" $ do
    it "scopes the remaining terms into a package row" $ do
        let need = parseNeed Set.empty "read parquet dataframe"
            qs = map probeQuery (expandProbes need (pkgRow "dataframe"))
        qs `shouldContain` ["+dataframe read"]
        qs `shouldContain` ["+dataframe parquet"]

    it "a symbol row is an answer, so it expands to nothing" $
        expandProbes (parseNeed Set.empty "read parquet") readParquetHit `shouldBe` []

    it "resolves a query that only Hoogle package rows answer" $ do
        hits <- hoogleQueryWith pivotRun 8 "parquet dataframe"
        map hhName hits `shouldStartWith` ["readParquet"]

    -- Naming a package ranks it first without hiding the alternatives; the old
    -- ladder could only express this by discarding every other stage's rows.
    it "a prose query naming a package ranks it first, foreign rows below" $ do
        hits <- hoogleQueryWith graniteRun 8 "bar chart granite"
        [hhPackage h | h <- hits, hhName h == "bars"]
            `shouldBe` ["granite", "chart-svg"]

corroborationSpec :: Spec
corroborationSpec = describe "corroboration" $
    it "a row found by several probes outranks one found by a single probe" $ do
        hits <- hoogleQueryWith corroborateRun 8 "chart bar"
        map hhName hits `shouldStartWith` ["barChart"]

-- Retrievers ----------------------------------------------------------------

-- The canned intent phrases used to answer here. They are gone; the shape is
-- kept so that reintroducing them fails this suite.
intentJunkRun :: Int -> Text -> IO [HoogleHit]
intentJunkRun _ q = pure $ case T.strip (T.toLower q) of
    "data import" -> [hit "ImportDataCon" "ghc-lib-parser" "GHC.Types.Hint" ""]
    "file reading" -> [hit "findLBFileForReading" "lambdabot-core" "Lambdabot.File" ""]
    "read parquet" -> [readParquetHit]
    _ -> []

sineRun :: Int -> Text -> IO [HoogleHit]
sineRun _ q = pure $ case T.strip (T.toLower q) of
    "svg rendering" -> [hit "SVGRenderingIntent" "jsaddle-dom" "JSDOM.Types" ""]
    "plot" ->
        [hit "plot" "sabela-notebook" "Sabela.Notebook" "[(Double, Double)] -> Picture"]
    "sine" -> [hit "sine" "tidal" "Sound.Tidal.Boot" "Time -> Signal Double"]
    _ -> []

pivotRun :: Int -> Text -> IO [HoogleHit]
pivotRun _ q = pure $ case T.strip (T.toLower q) of
    "parquet dataframe" -> [pkgRow "dataframe-parquet", pkgRow "dataframe"]
    "+dataframe parquet" ->
        [ docHit
            "ParquetReadOptions"
            "dataframe"
            "DataFrame"
            ""
            "Options for reading Parquet data."
        , readParquetHit
        ]
    _ -> []

graniteRun :: Int -> Text -> IO [HoogleHit]
graniteRun _ q = pure $ case T.strip (T.toLower q) of
    "granite" -> [pkgRow "granite"]
    "bar chart" -> [hit "bars" "chart-svg" "Chart.Bar" "BarOptions -> BarData -> [Chart]"]
    "+granite bar" ->
        [hit "bars" "granite" "Granite.Svg" "[(Text, Double)] -> Plot -> Text"]
    "+granite chart" ->
        [hit "chartToScene" "granite" "Granite.Render.Pipeline" "Chart -> Scene"]
    _ -> []

corroborateRun :: Int -> Text -> IO [HoogleHit]
corroborateRun _ q = pure $ case T.strip (T.toLower q) of
    "chart bar" -> [barChart]
    "chart" -> [chartOnly, barChart]
    "bar" -> [barChart]
    _ -> []
  where
    barChart = hit "barChart" "chart-svg" "Chart.Bar" "BarData -> [Chart]"
    chartOnly = hit "chartAspect" "chart-svg" "Chart.Style" "ChartAspect"
