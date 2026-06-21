{-# LANGUAGE OverloadedStrings #-}

{- | Built-in example notebooks shown in the editor's Examples panel. Extracted
from 'Sabela.Output' to keep modules under the size cap.
-}
module Sabela.Output.Examples (builtinExamples) where

import qualified Data.Text as T
import Sabela.Api (Example (..))

builtinExamples :: [Example]
builtinExamples =
    [ Example
        "Hello World"
        "Print a greeting"
        "Basics"
        "putStrLn \"Hello, Sabela!\""
    , Example
        "Fibonacci"
        "Lazy infinite list"
        "Basics"
        "let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)\n\nmapM_ print (take 15 fibs)"
    , Example
        "List comprehension"
        "Pythagorean triples"
        "Basics"
        "let triples = [(a,b,c) | c <- [1..20], b <- [1..c], a <- [1..b], a*a + b*b == c*c]\n\nprint triples"
    , Example
        "Map & Filter"
        "Higher-order functions"
        "Basics"
        "let xs = [1..20]\n\nprint $ filter even $ map (^2) xs"
    , Example
        "Working with Text"
        "Text manipulation with the text library"
        "Libraries"
        "-- cabal: build-depends: text\nimport qualified Data.Text as T\nimport qualified Data.Text.IO as TIO\n\nlet msg = T.pack \"Hello, World!\"\n\nTIO.putStrLn $ T.toUpper msg\n\nTIO.putStrLn $ T.reverse msg\n\nprint $ T.words msg"
    , Example
        "HTTP Request"
        "Fetch a URL with http-conduit"
        "Libraries"
        "-- cabal: build-depends: http-conduit, bytestring\nimport Network.HTTP.Simple\nimport qualified Data.ByteString.Lazy.Char8 as L8\n\nresponse <- httpLBS \"http://httpbin.org/get\"\n\nL8.putStrLn $ getResponseBody response"
    , Example
        "JSON Parsing"
        "Decode JSON with aeson"
        "Libraries"
        "-- cabal: build-depends: aeson, text, bytestring\n{-# LANGUAGE DeriveGeneric #-}\nimport Data.Aeson\nimport GHC.Generics\nimport qualified Data.ByteString.Lazy.Char8 as L8\n\ndata Person = Person { name :: String, age :: Int } deriving (Show, Generic)\ninstance FromJSON Person\n\nlet json = L8.pack \"{\\\"name\\\": \\\"Alice\\\", \\\"age\\\": 30}\"\n\nprint (decode json :: Maybe Person)"
    , Example
        "HTML Output"
        "Render rich HTML output"
        "Display"
        "displayHtml $ unlines\n  [ \"<div style='font-family: sans-serif; padding: 16px;'>\"\n  , \"  <h2 style='color: #4a9eff;'>Hello from Sabela</h2>\"\n  , \"  <p>This is <strong>rich HTML</strong> output.</p>\"\n  , \"  <ul>\"\n  , \"    <li>Item one</li>\"\n  , \"    <li>Item two</li>\"\n  , \"  </ul>\"\n  , \"</div>\"\n  ]"
    , Example
        "SVG Chart"
        "Draw an SVG bar chart"
        "Display"
        ( T.unlines
            [ "-- cabal: build-depends: text, granite"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified Data.Text as T"
            , "import Granite.Svg"
            , ""
            , "displaySvg $ T.unpack (bars [(\"Q1\",12),(\"Q2\",18),(\"Q3\",9),(\"Q4\",15)] defPlot {plotTitle=\"Sales\"})"
            ]
        )
    , Example
        "Markdown Output"
        "Render formatted markdown"
        "Display"
        "displayMarkdown $ unlines\n  [ \"# Analysis Results\"\n  , \"\"\n  , \"The computation found **42** as the answer.\"\n  , \"\"\n  , \"| Metric | Value |\"\n  , \"|--------|-------|\"\n  , \"| Speed  | Fast  |\"\n  , \"| Memory | Low   |\"\n  ]"
    , Example
        "Latex Output"
        "Render latex equations"
        "Display"
        "displayLatex \"x^2 + y^2 = z^2\""
    , Example
        "Layered Plot"
        "Scatter with an OLS best-fit line"
        "Plotting"
        ( T.unlines
            [ "-- cabal: build-depends: text, granite"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified Data.Text as T"
            , "import Granite.Color (Color (..))"
            , "import Granite.Data.Frame"
            , "import Granite.Render.Pipeline (renderChartSvg)"
            , "import Granite.Spec"
            , ""
            , "fitX = [0,1,2,3,4,5,6,7,8,9] :: [Double]"
            , "fitY = [1.3,2.5,5.7,6.8,9.4,10.4,13.1,15.3,16.6,19.5]"
            , "fitDf = fromColumns [(\"x\", ColNum fitX), (\"y\", ColNum fitY)]"
            , "fitMap = emptyMapping {aesX = Just (ColumnRef \"x\"), aesY = Just (ColumnRef \"y\")}"
            , "fitPts = (defLayer GeomPoint) {layerMapping = fitMap}"
            , "fitLine = (defLayer GeomLine) {layerMapping = fitMap, layerStat = StatSmooth SmoothLm, layerAesDef = emptyAesDefaults {defColor = Just (NamedColor BrightRed), defLineWidth = Just 2}}"
            , "fitChart = emptyChart {chartData = fitDf, chartLayers = [fitPts, fitLine], chartTitle = Just \"Scatter + OLS fit\", chartSize = SizeChars 60 18}"
            , ""
            , "displaySvg (T.unpack (renderChartSvg fitChart))"
            ]
        )
    , Example
        "Grouped Bars"
        "Multi-series bars with a fill mapping"
        "Plotting"
        ( T.unlines
            [ "-- cabal: build-depends: text, granite"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified Data.Text as T"
            , "import Granite.Data.Frame"
            , "import Granite.Render.Pipeline (renderChartSvg)"
            , "import Granite.Spec"
            , ""
            , "barQuarters = concat [replicate 3 q | q <- [\"Q1\",\"Q2\",\"Q3\",\"Q4\"]]"
            , "barProducts = take 12 (cycle [\"Widgets\",\"Gadgets\",\"Gizmos\"])"
            , "barSales = [12,8,4,15,10,6,18,12,8,22,14,10] :: [Double]"
            , "barDf = fromColumns [(\"quarter\", ColCat barQuarters), (\"product\", ColCat barProducts), (\"sales\", ColNum barSales)]"
            , "barLayer = (defLayer GeomBar) {layerMapping = emptyMapping {aesX = Just (ColumnRef \"quarter\"), aesY = Just (ColumnRef \"sales\"), aesGroup = Just (ColumnRef \"product\"), aesFill = Just (ColumnRef \"product\")}, layerStat = StatIdentity, layerPosition = PosDodge 0.25}"
            , "barChart = emptyChart {chartData = barDf, chartLayers = [barLayer], chartTitle = Just \"Sales by quarter\", chartSize = SizeChars 64 18}"
            , ""
            , "displaySvg (T.unpack (renderChartSvg barChart))"
            ]
        )
    , Example
        "Faceted Charts"
        "Small multiples, one panel per series"
        "Plotting"
        ( T.unlines
            [ "-- cabal: build-depends: text, granite"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified Data.Text as T"
            , "import Granite.Data.Frame"
            , "import Granite.Render.Pipeline (renderChartSvg)"
            , "import Granite.Spec"
            , ""
            , "facetDf = fromColumns [(\"x\", ColNum [0,1,2,3,0,1,2,3,0,1,2,3]), (\"y\", ColNum [1,4,9,16,0,2,4,6,5,4,3,2]), (\"series\", ColCat (replicate 4 \"A\" <> replicate 4 \"B\" <> replicate 4 \"C\"))]"
            , "facetLayer = (defLayer GeomLine) {layerMapping = emptyMapping {aesX = Just (ColumnRef \"x\"), aesY = Just (ColumnRef \"y\")}}"
            , "facetChart = emptyChart {chartData = facetDf, chartLayers = [facetLayer], chartFacet = FacetWrap (ColumnRef \"series\") (Just 3) Nothing ScalesFixed, chartTitle = Just \"Faceted by series\", chartSize = SizeChars 72 18}"
            , ""
            , "displaySvg (T.unpack (renderChartSvg facetChart))"
            ]
        )
    , Example
        "Interactive Slider"
        "Temperature converter with a live slider"
        "Widgets"
        "c <- display (slider \"celsius\" (20 :: Int) (-40) 120)\nlet f = c * 9 `div` 5 + 32\n    k = c + 273\n\ndisplayHtml $ \"<p><b>\" ++ show c ++ \" \176C</b> = \" ++ show f ++ \" \176F = \" ++ show k ++ \" K</p>\""
    , Example
        "Interactive Dropdown"
        "Shape viewer driven by a select control"
        "Widgets"
        "shape <- display (dropdown \"shape\" [\"Circle\", \"Square\", \"Triangle\"] \"Circle\")\nlet svg = case shape of\n      \"Circle\"   -> \"<circle cx='60' cy='60' r='50' fill='#3498db'/>\"\n      \"Square\"   -> \"<rect x='10' y='10' width='100' height='100' rx='4' fill='#e74c3c'/>\"\n      _          -> \"<polygon points='60,10 110,110 10,110' fill='#2ecc71'/>\"\n\ndisplayHtml $ \"<svg width='120' height='120' xmlns='http://www.w3.org/2000/svg'>\" ++ svg ++ \"</svg>\""
    , Example
        "Interactive Button"
        "Prime sieve with a slider and compute button"
        "Widgets"
        "clicked <- display (button \"Compute primes\" \"go\")\nn <- display (slider \"limit\" (50 :: Int) 2 500)\nlet sieve []     = []\n    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]\n\ncase clicked of\n  Nothing -> displayHtml \"<p>Press the button to compute.</p>\"\n  Just () -> let ps = sieve [2..n] in displayHtml $ \"<p><b>\" ++ show (length ps) ++ \" primes \\8804 \" ++ show n ++ \"</b><br>\" ++ unwords (map show ps) ++ \"</p>\""
    , Example
        "Interactive Checkbox"
        "Gate output behind a checkbox"
        "Widgets"
        "verbose <- display (checkbox \"verbose\" False)\nn <- display (slider \"n\" (1000 :: Int) 1 10000)\n\nif verbose then displayMarkdown (\"Computing sum from 1 to \" ++ show n) else return ()\n\ndisplayHtml $ \"<p>Result: <b>\" ++ show (sum [1..n]) ++ \"</b></p>\""
    , Example
        "Interactive Text Input"
        "Greet a name entered in a text box"
        "Widgets"
        "name <- display (textInput \"name\" \"World\")\ndisplayHtml $ \"<h2>Hello, \" ++ name ++ \"!</h2>\""
    , Example
        "Composed Inputs"
        "Combine two sliders with liftA2"
        "Widgets"
        "area <- display (liftA2 (*) (slider \"width\" (10 :: Int) 1 100) (slider \"height\" (10 :: Int) 1 100))\ndisplayHtml $ \"<p>Area: <b>\" ++ show area ++ \"</b></p>\""
    , Example
        "Concurrent IO"
        "Async with threads"
        "Advanced"
        "import Control.Concurrent\nimport Control.Monad\n\nmv <- newMVar (0 :: Int)\nlet inc = modifyMVar_ mv (\\n -> pure (n+1))\n\nts <- forM [1..100] (\\_ -> forkIO inc)\n\nmapM_ (\\_ -> threadDelay 1000) ts\nthreadDelay 50000\nresult <- readMVar mv\n\nputStrLn $ \"Counter: \" ++ show result"
    , Example
        "QuickCheck"
        "Property-based testing"
        "Advanced"
        "-- cabal: build-depends: QuickCheck\nimport Test.QuickCheck\n\nlet prop_reverse xs = reverse (reverse xs) == (xs :: [Int])\nquickCheck prop_reverse\n\nlet prop_sort_length xs = length (filter even xs) + length (filter odd xs) == length (xs :: [Int])\n\nquickCheck prop_sort_length"
    , Example
        "File I/O"
        "Read and write files"
        "Advanced"
        "writeFile \"/tmp/sabela-test.txt\" \"Hello from Sabela!\\nLine two.\\n\"\ncontents <- readFile \"/tmp/sabela-test.txt\"\n\nputStrLn contents\n\nputStrLn $ \"Lines: \" ++ show (length (lines contents))"
    ]
