{-# LANGUAGE OverloadedStrings #-}

module Sabela.Output where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Api (Example (..))

mimeMarkerPrefix :: Text
mimeMarkerPrefix = "---MIME:"

mimeMarkerSuffix :: Text
mimeMarkerSuffix = "---"

parseMimeOutputs :: Text -> [(Text, Text)]
parseMimeOutputs raw =
    let ls = T.lines raw
        (finalMime, finalLines, acc) = foldl step ("text/plain", [], []) ls
        finalBlock = T.unlines (reverse finalLines)
        result =
            if T.null (T.strip finalBlock)
                then acc
                else (T.strip finalMime, finalBlock) : acc
     in reverse result
  where
    step (curMime, curLines, acc) l =
        case T.stripPrefix mimeMarkerPrefix l >>= T.stripSuffix mimeMarkerSuffix of
            Just mime
                | not (T.null (T.strip mime)) ->
                    let block = T.unlines (reverse curLines)
                        acc' =
                            if T.null (T.strip block)
                                then acc
                                else (T.strip curMime, block) : acc
                     in (mime, [], acc')
            _ -> (curMime, l : curLines, acc)

displayPrelude :: Text
displayPrelude =
    T.unlines
        [ "import Data.IORef"
        , "_sabelaWidgetRef <- newIORef ([] :: [(String, String)])"
        , "_sabelaCellIdRef <- newIORef (\"0\" :: String)"
        , ":{"
        , "data Behavior a = Behavior { bSample :: IO a, bRender :: IO () }"
        , "instance Functor Behavior where"
        , "  fmap f b = Behavior { bSample = fmap f (bSample b), bRender = bRender b }"
        , "instance Applicative Behavior where"
        , "  pure x = Behavior { bSample = pure x, bRender = pure () }"
        , "  bf <*> bx = Behavior { bSample = bSample bf <*> bSample bx, bRender = bRender bf >> bRender bx }"
        , ":}"
        , ":{"
        , "let { displayMime_ t c = putStrLn (\"---MIME:\" ++ t ++ \"---\") >> putStrLn c"
        , "    ; displayHtml     = displayMime_ \"text/html\""
        , "    ; displayMarkdown = displayMime_ \"text/markdown\""
        , "    ; displaySvg      = displayMime_ \"image/svg+xml\""
        , "    ; displayLatex    = displayMime_ \"text/latex\""
        , "    ; displayJson     = displayMime_ \"application/json\""
        , "    ; displayImage mime b64 = putStrLn (\"---MIME:\" ++ mime ++ \";base64---\") >> putStrLn b64"
        , "    ; widgetGet name = fmap (lookup name) (readIORef _sabelaWidgetRef)"
        , "    ; widgetRead name def = fmap (lookup name) (readIORef _sabelaWidgetRef) >>= \\mv -> pure $ case mv of { Nothing -> def; Just s -> case reads s of { [(v,\"\")] -> v; _ -> def } }"
        , "    ; displaySlider name lo hi val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml $ concat"
        , "        [ \"<input type='range' min='\" ++ show lo ++ \"' max='\" ++ show hi"
        , "        , \"' value='\" ++ show val ++ \"' \""
        , "        , \"oninput=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "        , \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\" ]"
        , "    ; displayButton label name = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml $ concat"
        , "        [ \"<button onclick=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "        , \",name:'\" ++ name ++ \"',value:'clicked'},'*')\\\">\" ++ label ++ \"</button>\" ]"
        , "    ; displaySelect name opts val = readIORef _sabelaCellIdRef >>= \\cid -> displayHtml $ concat"
        , "        [ \"<select onchange=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "        , \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\""
        , "        , concatMap (\\o -> \"<option\" ++ (if o == val then \" selected\" else \"\") ++ \">\" ++ o ++ \"</option>\") opts"
        , "        , \"</select>\" ]"
        , "    ; display b = bRender b >> bSample b"
        , "    ; sample = bSample"
        , "    ; render = bRender"
        , "    ; slider name def lo hi = Behavior"
        , "        { bSample = widgetRead name def"
        , "        , bRender = widgetRead name def >>= \\val ->"
        , "            readIORef _sabelaCellIdRef >>= \\cid ->"
        , "            displayMime_ \"text/html\" $ \"<input type='range' min='\" ++ show lo ++ \"' max='\" ++ show hi"
        , "              ++ \"' value='\" ++ show val"
        , "              ++ \"' oninput=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "              ++ \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\" }"
        , "    ; dropdown name opts def = Behavior"
        , "        { bSample = fmap (maybe def id) (widgetGet name)"
        , "        , bRender = fmap (maybe def id) (widgetGet name) >>= \\val ->"
        , "            readIORef _sabelaCellIdRef >>= \\cid ->"
        , "            displayMime_ \"text/html\" $ \"<select onchange=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "              ++ \",name:'\" ++ name ++ \"',value:this.value},'*')\\\">\" "
        , "              ++ concatMap (\\o -> \"<option\" ++ (if o == val then \" selected\" else \"\") ++ \">\" ++ o ++ \"</option>\") opts"
        , "              ++ \"</select>\" }"
        , "    ; checkbox name def = Behavior"
        , "        { bSample = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name)"
        , "        , bRender = fmap (\\mv -> case mv of { Just \"true\" -> True; Just \"false\" -> False; _ -> def }) (widgetGet name) >>= \\val ->"
        , "            readIORef _sabelaCellIdRef >>= \\cid ->"
        , "            displayMime_ \"text/html\" $ \"<input type='checkbox'\" ++ (if val then \" checked\" else \"\")"
        , "              ++ \" onchange=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "              ++ \",name:'\" ++ name ++ \"',value:this.checked.toString()},'*')\\\">\" }"
        , "    ; textInput name def = Behavior"
        , "        { bSample = fmap (maybe def id) (widgetGet name)"
        , "        , bRender = fmap (maybe def id) (widgetGet name) >>= \\val ->"
        , "            readIORef _sabelaCellIdRef >>= \\cid ->"
        , "            displayMime_ \"text/html\" $ \"<input type='text' value='\" ++ val"
        , "              ++ \"' oninput=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "              ++ \",name:'\" ++ name ++ \"',value:this.value,sel:this.selectionStart},'*')\\\">\" }"
        , "    ; button label name = Behavior"
        , "        { bSample = fmap (\\mv -> case mv of { Just \"clicked\" -> Just (); _ -> Nothing }) (widgetGet name)"
        , "        , bRender = readIORef _sabelaCellIdRef >>= \\cid ->"
        , "            displayMime_ \"text/html\" $ \"<button onclick=\\\"parent.postMessage({type:'widget',cellId:\" ++ cid"
        , "              ++ \",name:'\" ++ name ++ \"',value:'clicked'},'*')\\\">\" ++ label ++ \"</button>\" }"
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
        "Latex Output"
        "Render latex equations"
        "Display"
        "displayLatex \"x^2 + y^2 = z^2\""
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
        "Composed Behaviors"
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
