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

blockCount :: Text -> Int
blockCount = length . filter (T.isInfixOf "= do") . T.lines

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

    describe "renderNonExecuting (G1 compile-gate candidate rendering)" $ do
        describe "gate-drops-bind (live_test24)" $ do
            let rendered =
                    renderNonExecuting
                        "import qualified DataFrame as D\n\
                        \df <- D.readCsv \"./data.csv\"\n\
                        \print (D.take 10 df)"

            it "keeps the bind's pattern, so a later statement sees it" $
                rendered `shouldSatisfy` T.isInfixOf "df <- D.readCsv"

            it "folds the run into ONE do block, not per-statement probes" $ do
                rendered `shouldSatisfy` T.isInfixOf "= do"
                blockCount rendered `shouldBe` 1

            it "still executes nothing: the block is bound, never forced" $
                rendered `shouldSatisfy` T.isInfixOf "_sabelaGateStmts"

            it "keeps the import outside the block" $
                rendered `shouldSatisfy` T.isInfixOf "import qualified DataFrame as D"

            it "never ends a do block on a bind" $ do
                let endsBind = renderNonExecuting "x <- readLn"
                endsBind `shouldSatisfy` T.isInfixOf "pure ()"

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

        it "a monadic bind KEEPS its pattern, inside a non-executing block" $ do
            let rendered = renderNonExecuting "x <- readFile \"input.txt\""
            rendered `shouldSatisfy` T.isInfixOf "_sabelaGateStmts"
            rendered `shouldSatisfy` T.isInfixOf "readFile \"input.txt\""
            rendered `shouldSatisfy` T.isInfixOf "x <- readFile"
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
            rendered `shouldSatisfy` T.isInfixOf "ys <- pure"
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
