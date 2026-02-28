{-# LANGUAGE OverloadedStrings #-}

module Sabela.Output where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Api (Example (..))

mimeMarkerPrefix :: Text
mimeMarkerPrefix = "---MIME:"

mimeMarkerSuffix :: Text
mimeMarkerSuffix = "---"

parseMimeOutput :: Text -> (Text, Text)
parseMimeOutput raw =
    case T.lines raw of
        (firstLine : rest)
            | Just afterPrefix <- T.stripPrefix mimeMarkerPrefix firstLine
            , Just mimeType <- T.stripSuffix mimeMarkerSuffix afterPrefix
            , not (T.null mimeType) ->
                (T.strip mimeType, T.unlines rest)
        _ -> ("text/plain", raw)

displayPrelude :: Text
displayPrelude =
    T.unlines
        [ ":{"
        , "let { displayMime_ t c = putStrLn (\"---MIME:\" ++ t ++ \"---\") >> putStrLn c"
        , "    ; displayHtml     = displayMime_ \"text/html\""
        , "    ; displayMarkdown = displayMime_ \"text/markdown\""
        , "    ; displaySvg      = displayMime_ \"image/svg+xml\""
        , "    ; displayLatex    = displayMime_ \"text/latex\""
        , "    ; displayJson     = displayMime_ \"application/json\""
        , "    ; displayImage mime b64 = putStrLn (\"---MIME:\" ++ mime ++ \";base64---\") >> putStrLn b64"
        , "    }"
        , ":}"
        ]

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
