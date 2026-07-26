{-# LANGUAGE OverloadedStrings #-}

{- | Pins 'renderNonExecuting': G1's gate must type-check a candidate WITHOUT
ever letting GHCi execute it. A live_test4-class regression (a runaway
@print (length [1..])@ candidate actually ran for a full 120s inside the
gate) is exactly what this guards — fast, no live GHCi needed.
-}
module Test.CompileGateRenderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.CompileGate.Render (
    renderForDiagnostics,
    renderNonExecuting,
 )

{- | Nothing in the render is a bare, immediately-runnable GHCi statement:
every non-declaration piece is wrapped in a fresh probe binding.
-}

{- | Nothing GHCi would EXECUTE. GHCi runs a bare statement but never the
BODY of a binding, so the invariant is per block, not per line: each
@:{ … :}@ block must OPEN with a binding or declaration, and everything
outside a block must be an import, pragma or comment. Checking every line
instead wrongly flagged the statements inside a non-executing @do@ block,
which are exactly as inert as the binding that holds them.
-}
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

-- | Lines that sit outside every @:{ … :}@ block.
outsideBlockLines :: Text -> [Text]
outsideBlockLines rendered = go (map T.strip (T.lines rendered))
  where
    go ls = case break (== ":{") ls of
        (before, _ : rest) -> before <> go (drop 1 (dropWhile (/= ":}") rest))
        (before, []) -> before

{- | The @:{ … :}@ blocks of a rendering. GHCi judges each block on its own,
so what shares a block is what shares a scope.
-}
blocksOf :: Text -> [[Text]]
blocksOf rendered = go (map T.strip (T.lines rendered))
  where
    go ls = case break (== ":{") ls of
        (_, _ : rest) ->
            let (body, after) = break (== ":}") rest
             in body : go (drop 1 after)
        _ -> []

{- | Brackets balance outside string literals. A render that drops part of an
expression produces a stray closing bracket, which GHC reports at a column the
submitted source does not have — the live_test19 phantom diagnostic.
-}
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

{- | A type signature split from its binding makes GHCi report the signature's
own name as not in scope, so the two must land in one block.
-}
sharesABlock :: Text -> Text -> Text -> Bool
sharesABlock sig bind rendered =
    any
        (\b -> any (T.isInfixOf sig) b && any (T.isInfixOf bind) b)
        (blocksOf rendered)

-- | How many non-executing statement blocks the rendering emits.
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
        {- live_test24: the gate rejected the commonest idiom there is —
        `df <- readCsv ...` then any use of `df` — because each bind's
        pattern was dropped. A gate with false negatives starves a session
        exactly as a missing gate does; the housing probe could not load a
        CSV at all until this was folded into one do block. -}
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

        {- Inverted 2026-07-25: the pattern is now KEPT. Dropping it is what
        made `df <- readCsv …` + any use of `df` unpassable — see
        'gate-drops-bind'. The invariant that still holds is that nothing
        executes, which the enclosing binding guarantees. -}
        it "a monadic bind KEEPS its pattern, inside a non-executing block" $ do
            let rendered = renderNonExecuting "x <- readFile \"input.txt\""
            rendered `shouldSatisfy` T.isInfixOf "_sabelaGateStmts"
            rendered `shouldSatisfy` T.isInfixOf "readFile \"input.txt\""
            rendered `shouldSatisfy` T.isInfixOf "x <- readFile"
            rendered `shouldSatisfy` noBareStatement

        -- live_test19: the gate rejected every animate candidate with a
        -- fabricated "parse error on input ']'" because a comprehension's
        -- `<-` was read as a bind pattern and everything before it dropped,
        -- leaving an unbalanced bracket the model could not see in its source.
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
