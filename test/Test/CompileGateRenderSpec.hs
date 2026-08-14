{-# LANGUAGE OverloadedStrings #-}

module Test.CompileGateRenderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.CompileGate.Render (
    renderForDiagnostics,
    renderNonExecuting,
 )

noBareStatement :: Text -> Bool
noBareStatement rendered =
    all blockOpensWithBinding (blocksOf rendered)
        && all (safeOutside . T.strip) (outsideBlockLines rendered)
  where
    blockOpensWithBinding body = case filter (not . T.null) (map T.strip body) of
        [] -> True
        (l : _) -> isBindingOrDecl l
    isBindingOrDecl l =
        "=" `T.isInfixOf` l
            || "::" `T.isInfixOf` l
            || any (`T.isPrefixOf` l) declStarts
    declStarts = ["data ", "newtype ", "type ", "class ", "instance ", "--", "{-#"]
    safeOutside l =
        T.null l
            || "import " `T.isPrefixOf` l
            || "{-#" `T.isPrefixOf` l
            || "--" `T.isPrefixOf` l

outsideBlockLines :: Text -> [Text]
outsideBlockLines rendered = go (map T.strip (T.lines rendered))
  where
    go ls = case break (== ":{") ls of
        (before, _ : rest) -> before <> go (drop 1 (dropWhile (/= ":}") rest))
        (before, []) -> before

blocksOf :: Text -> [[Text]]
blocksOf rendered = go (map T.strip (T.lines rendered))
  where
    go ls = case break (== ":{") ls of
        (_, _ : rest) ->
            let (body, after) = break (== ":}") rest
             in body : go (drop 1 after)
        _ -> []

balancedBrackets :: Text -> Bool
balancedBrackets = go (0 :: Int) . T.unpack
  where
    go depth s = case s of
        [] -> depth == 0
        ('"' : rest) -> go depth (skipString rest)
        (c : rest)
            | c `elem` ("([" :: String) -> go (depth + 1) rest
            | c `elem` (")]" :: String) -> depth > 0 && go (depth - 1) rest
            | otherwise -> go depth rest
    skipString s = case s of
        [] -> []
        ('\\' : rest) -> skipString (drop 1 rest)
        ('"' : rest) -> rest
        (_ : rest) -> skipString rest

sharesABlock :: Text -> Text -> Text -> Bool
sharesABlock sig bind rendered =
    any
        (\b -> any (T.isInfixOf sig) b && any (T.isInfixOf bind) b)
        (blocksOf rendered)

probeContinuationsIndented :: Text -> Bool
probeContinuationsIndented rendered = go (T.lines rendered)
  where
    go [] = True
    go (l : ls)
        | "_sabelaGateProbe" `T.isInfixOf` l
            || "= _sabelaGateBound" `T.isInfixOf` l =
            let (body, rest) = break (== ":}") ls
             in all indentedOrBlank body && go rest
        | otherwise = go ls
    indentedOrBlank t = T.null t || " " `T.isPrefixOf` t

spec :: Spec
spec = do
    describe "renderForDiagnostics (G6 evidence rendering)" $ do
        it "keeps a signature with its binding, as the commit rendering does" $
            renderForDiagnostics "y :: Int\ny = 3"
                `shouldSatisfy` sharesABlock "y :: Int" "y = 3"

        it "separates independent declarations so each reports its own defect" $ do
            let rendered = renderForDiagnostics "a = notInScopeOne\nb = notInScopeTwo"
            length (blocksOf rendered) `shouldBe` 2
            rendered `shouldSatisfy` (not . sharesABlock "a = " "b = ")

        it "keeps a multi-equation function whole (splitting it would shadow)" $ do
            let rendered = renderForDiagnostics "f 0 = 1\nf n = n * 2"
            length (blocksOf rendered) `shouldBe` 1
            rendered `shouldSatisfy` sharesABlock "f 0 = 1" "f n = n * 2"

        it "keeps a signature with a multi-equation binding it introduces" $
            renderForDiagnostics "f :: Int -> Int\nf 0 = 1\nf n = n * 2"
                `shouldSatisfy` sharesABlock "f :: Int -> Int" "f n = n * 2"

        it "gives a data declaration its own block" $
            length (blocksOf (renderForDiagnostics "data T = T\nx = 1")) `shouldBe` 2

        it "never leaves a bare executable statement" $
            renderForDiagnostics "x = 1\nprint x"
                `shouldSatisfy` noBareStatement

        it "a bind keeps its pattern, so localisation sees what it defines" $ do
            let rendered =
                    renderForDiagnostics
                        "contents <- readFile \"x\"\n\
                        \ls = drop 1 (lines contents)"
            rendered `shouldSatisfy` T.isInfixOf "contents = _sabelaGateBound ("
            rendered `shouldSatisfy` T.isInfixOf "_sabelaGateBound :: IO a -> a"
            rendered `shouldSatisfy` T.isInfixOf "ls = drop 1 (lines contents)"

        it "a multi-line bind body never continues at column 1" $
            renderForDiagnostics "df <- D.readCsv\n  \"./data.csv\""
                `shouldSatisfy` probeContinuationsIndented

        it
            "a comment attached to the trailing expression stays layout-safe (live_gemma)"
            $ do
                let rendered =
                        renderForDiagnostics
                            "-- Plot the data\nplot [(0, 0), (1, 1)]"
                rendered `shouldSatisfy` probeContinuationsIndented
                rendered `shouldSatisfy` T.isInfixOf "plot [(0, 0), (1, 1)]"

        it "a multi-line probe body never continues at column 1" $
            renderForDiagnostics "print\n  (1 :: Int)"
                `shouldSatisfy` probeContinuationsIndented

        it "a bind arrow with no pattern falls back to a probe binding" $ do
            let rendered = renderForDiagnostics "<- getLine"
            rendered `shouldSatisfy` T.isInfixOf "_sabelaGateProbe"
            rendered `shouldSatisfy` T.isInfixOf "getLine"
            rendered `shouldSatisfy` (not . T.isInfixOf "_sabelaGateBound")

    describe "renderNonExecuting (G1 compile-gate candidate rendering)" $ do
        describe "a bind declares its pattern at the top level (live_test24)" $ do
            let rendered =
                    renderNonExecuting
                        "import qualified DataFrame as D\n\
                        \df <- D.readCsv \"./data.csv\"\n\
                        \print (D.take 10 df)"

            it "redeclares the bind's pattern, so later code sees it" $
                rendered `shouldSatisfy` T.isInfixOf "df = _sabelaGateBound ("

            it "declares the proxy that types the pattern without running it" $
                rendered `shouldSatisfy` T.isInfixOf "_sabelaGateBound :: IO a -> a"

            it "still executes nothing: every block opens on a binding" $
                rendered `shouldSatisfy` noBareStatement

            it "the trailing action still compiles under a generated binder" $ do
                rendered `shouldSatisfy` T.isInfixOf "= do"
                rendered `shouldSatisfy` T.isInfixOf "_sabelaGateStmts"

            it "keeps the import outside the block" $
                rendered `shouldSatisfy` T.isInfixOf "import qualified DataFrame as D"

        describe "a declaration after a bind (live-eval join-fanout)" $ do
            let rendered =
                    renderNonExecuting
                        "contents <- readFile \"tickets.csv\"\n\
                        \ls = drop 1 (lines contents)"

            it "the bound name is a top-level declaration the next one can use" $ do
                rendered `shouldSatisfy` T.isInfixOf "contents = _sabelaGateBound ("
                rendered `shouldSatisfy` T.isInfixOf "ls = drop 1 (lines contents)"
                rendered `shouldSatisfy` noBareStatement

        it "declares the proxy once, however many binds there are" $ do
            let rendered = renderNonExecuting "a <- pure 1\nb <- pure 2"
            T.count "_sabelaGateBound ::" rendered `shouldBe` 1
            rendered `shouldSatisfy` T.isInfixOf "a = _sabelaGateBound ("
            rendered `shouldSatisfy` T.isInfixOf "b = _sabelaGateBound ("

        it "declares no proxy when the cell has no binds" $
            renderNonExecuting "x = 1\nprint x"
                `shouldSatisfy` (not . T.isInfixOf "_sabelaGateBound")

        it "a tuple pattern survives as a top-level pattern binding" $
            renderNonExecuting "(a, b) <- pure (1, 2)"
                `shouldSatisfy` T.isInfixOf "(a, b) = _sabelaGateBound ("

        it "an action between binds still sees both bound names" $ do
            let rendered = renderNonExecuting "x <- readLn\nprint x\ny <- readLn"
            rendered `shouldSatisfy` T.isInfixOf "x = _sabelaGateBound ("
            rendered `shouldSatisfy` T.isInfixOf "y = _sabelaGateBound ("
            rendered `shouldSatisfy` T.isInfixOf "print x"
            rendered `shouldSatisfy` noBareStatement

        it "keeps a type signature in the same block as its binding (live_test6)" $ do
            let rendered = renderNonExecuting "y :: Int\ny = 3"
            rendered `shouldSatisfy` sharesABlock "y :: Int" "y = 3"
            rendered `shouldSatisfy` noBareStatement

        it "keeps them together across the blank line a cell usually has" $
            renderNonExecuting "y :: Int\n\ny = 3"
                `shouldSatisfy` sharesABlock "y :: Int" "y = 3"

        it "keeps each signature with its own binding in a multi-definition cell" $ do
            let rendered =
                    renderNonExecuting
                        "sineWaveSvg :: String\nsineWaveSvg = \"a\"\n\nwidth :: Int\nwidth = 400"
            rendered `shouldSatisfy` sharesABlock "sineWaveSvg :: String" "sineWaveSvg ="
            rendered `shouldSatisfy` sharesABlock "width :: Int" "width = 400"
            rendered `shouldSatisfy` noBareStatement

        it "keeps a signature with its binding after an import (the live_test6 shape)" $
            renderNonExecuting "import Data.List (intercalate)\n\ny :: Int\ny = 3"
                `shouldSatisfy` sharesABlock "y :: Int" "y = 3"

        it "a pure declaration cell passes through unchanged in content" $ do
            let rendered = renderNonExecuting "x = 1\ny = x + 1"
            rendered `shouldSatisfy` T.isInfixOf "x = 1"
            rendered `shouldSatisfy` T.isInfixOf "y = x + 1"
            rendered `shouldSatisfy` noBareStatement

        it "a bare pure expression is bound, never left as a bare statement" $ do
            let rendered = renderNonExecuting "1 + 1"
            rendered `shouldSatisfy` T.isInfixOf "_sabelaGateStmts"
            rendered `shouldSatisfy` T.isInfixOf "1 + 1"
            rendered `shouldSatisfy` noBareStatement

        it
            "a runaway IO action is bound to a probe, never left as a bare statement (the live_test4-class regression)"
            $ do
                let rendered = renderNonExecuting "print (length [(1 :: Integer) ..])"
                rendered `shouldSatisfy` T.isInfixOf "_sabelaGateStmts"
                rendered `shouldSatisfy` T.isInfixOf "print (length [(1 :: Integer) ..])"
                rendered `shouldSatisfy` noBareStatement

        it
            "a main = <action> binding passes through as a declaration (already non-executing)"
            $ do
                let rendered = renderNonExecuting "main = putStrLn \"hi\""
                rendered `shouldSatisfy` T.isInfixOf "main = putStrLn \"hi\""
                rendered `shouldSatisfy` noBareStatement

        it "a monadic bind KEEPS its pattern, as a non-executing declaration" $ do
            let rendered = renderNonExecuting "x <- readFile \"input.txt\""
            rendered `shouldSatisfy` T.isInfixOf "x = _sabelaGateBound ("
            rendered `shouldSatisfy` T.isInfixOf "readFile \"input.txt\""
            rendered `shouldSatisfy` noBareStatement

        it "a comprehension's arrow is not mistaken for a bind pattern" $ do
            let src = "animate 0 (\\t -> plot [(x, sin x) | x <- [0,0.01..(2*pi)]])"
                rendered = renderNonExecuting src
            rendered `shouldSatisfy` T.isInfixOf src
            rendered `shouldSatisfy` balancedBrackets
            rendered `shouldSatisfy` noBareStatement

        it "a bind whose right-hand side holds a comprehension keeps it whole" $ do
            let rendered = renderNonExecuting "ys <- pure [y | y <- [1,2]]"
            rendered `shouldSatisfy` T.isInfixOf "pure [y | y <- [1,2]]"
            rendered `shouldSatisfy` T.isInfixOf "ys = _sabelaGateBound ("
            rendered `shouldSatisfy` balancedBrackets
            rendered `shouldSatisfy` noBareStatement

        it "imports and pragmas pass through untouched (never executable statements)" $ do
            let rendered =
                    renderNonExecuting
                        "import qualified Data.Map.Strict as M\nx = M.empty"
            rendered `shouldSatisfy` T.isInfixOf "import qualified Data.Map.Strict as M"
            rendered `shouldSatisfy` T.isInfixOf "x = M.empty"
            rendered `shouldSatisfy` noBareStatement

        it "a multi-line action stays syntactically intact inside its probe binding" $ do
            let src =
                    T.unlines
                        [ "do"
                        , "  putStrLn \"a\""
                        , "  putStrLn \"b\""
                        ]
                rendered = renderNonExecuting src
            rendered `shouldSatisfy` T.isInfixOf "_sabelaGateStmts"
            rendered `shouldSatisfy` T.isInfixOf "putStrLn \"a\""
            rendered `shouldSatisfy` T.isInfixOf "putStrLn \"b\""
