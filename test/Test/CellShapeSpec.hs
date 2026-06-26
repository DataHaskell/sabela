{-# LANGUAGE OverloadedStrings #-}

{- | The pre-GHC structural validator (C2): at the cell-mutation boundary it
rejects obviously-wrong cell shapes — a top-level @let@ in a code cell and
Haskell code pasted into a 'ProseCell' — before they reach the compiler,
with a deterministic message (the @let@ one deduped against
'Sabela.Diagnose.letParse').
-}
module Test.CellShapeSpec (spec) where

import qualified Data.Text as T
import Sabela.Model (CellType (..))
import Sabela.Parse (staleBindings, validateCellShape)
import Sabela.Parse.Normalize (looksLikeHaskellCode, unwrapMain)
import Test.Hspec

spec :: Spec
spec = describe "Sabela.Parse.validateCellShape" $ do
    describe "top-level let in a code cell is rejected" $ do
        it "rejects `let x = 1` with the deduped letParse message" $ do
            let r = validateCellShape CodeCell "let x = 1"
            r `shouldSatisfy` rejected
            fmap (T.isInfixOf "top-level `let`") r `shouldBe` Just True

        it "rejects an indented/multi-line cell whose first stmt is a let" $ do
            let r = validateCellShape CodeCell "let y = 2\nmain = print y"
            r `shouldSatisfy` rejected

    describe "code in a ProseCell is rejected" $ do
        it "rejects a value binding pasted into prose" $ do
            let r = validateCellShape ProseCell "x = 1"
            r `shouldSatisfy` rejected
            fmap (T.isInfixOf "ProseCell") r `shouldBe` Just True

        it "rejects a function definition pasted into prose" $
            validateCellShape ProseCell "square n = n * n"
                `shouldSatisfy` rejected

        it "rejects a data declaration pasted into prose" $
            validateCellShape ProseCell "data Foo = Bar | Baz"
                `shouldSatisfy` rejected

    describe "unwrapMain (auto-rewrite a top-level main to top level)" $ do
        it "drops the signature and `main = do`, leaving a top-level do" $
            unwrapMain "main :: IO ()\nmain = do\n  putStrLn \"hi\"\n  print 5"
                `shouldBe` "do\n  putStrLn \"hi\"\n  print 5\n"

        it "turns `main = e` into `e`" $
            unwrapMain "main = print 5" `shouldBe` "print 5\n"

        it "leaves a cell with no top-level main unchanged" $
            unwrapMain "x = 1\ny = 2" `shouldBe` "x = 1\ny = 2"

        it "does not touch a binding merely named like main (mainLoop)" $
            unwrapMain "mainLoop = go" `shouldBe` "mainLoop = go"

        it "rewrites main even when the cell defeats the parser (TH splice + pragma)" $ do
            let cell =
                    T.unlines
                        [ "{-# LANGUAGE TemplateHaskell #-}"
                        , "import Language.Haskell.TH (runIO)"
                        , "url = \"http://x\""
                        , "csvData = $(runIO (pure undefined))"
                        , "main :: IO ()"
                        , "main = do"
                        , "  putStrLn url"
                        ]
                out = unwrapMain cell
            out `shouldSatisfy` (not . T.isInfixOf "main ::")
            out `shouldSatisfy` (not . T.isInfixOf "main = do")
            out `shouldSatisfy` T.isInfixOf "url ="

    describe "well-formed cells pass" $ do
        it "a plain value binding in a code cell passes" $
            validateCellShape CodeCell "x = 1" `shouldBe` Nothing

        it "a function definition in a code cell passes" $
            validateCellShape CodeCell "square n = n * n" `shouldBe` Nothing

        it "a `let ... in` expression in a code cell passes" $
            validateCellShape CodeCell "let x = 1 in x + 1" `shouldBe` Nothing

        it "a let-statement nested in a do block passes (not top-level)" $
            validateCellShape CodeCell "h = do\n  let a = 1\n  print a"
                `shouldBe` Nothing

        it "the reported do/let example passes (indented let is not top-level)" $
            validateCellShape
                CodeCell
                "f :: Int -> Maybe Int\nf x = do\n  let y = 5\n      z = 6\n  pure (x + y + z)"
                `shouldBe` Nothing

        it "a bare expression in a code cell passes" $
            validateCellShape CodeCell "print (1 + 2)" `shouldBe` Nothing

        it "markdown prose in a ProseCell passes" $
            validateCellShape ProseCell "# Heading\n\nSome explanatory prose."
                `shouldBe` Nothing

        it "an empty ProseCell passes" $
            validateCellShape ProseCell "   " `shouldBe` Nothing

    describe "looksLikeHaskellCode (auto-correct detection for code-as-prose)" $ do
        it "flags an import-led cell with no top-level def (the csDefs gap)" $
            looksLikeHaskellCode "import Data.List\n\nfoldr (+) 0 [1, 2, 3]"
                `shouldBe` True

        it "flags a -- cabal: line and a LANGUAGE pragma" $ do
            looksLikeHaskellCode
                "-- cabal: build-depends: dataframe\nimport qualified DataFrame as D"
                `shouldBe` True
            looksLikeHaskellCode "{-# LANGUAGE OverloadedStrings #-}\nx = \"hi\""
                `shouldBe` True

        it "flags a binding, a signature, and a data declaration" $ do
            looksLikeHaskellCode "result = sum [1 .. 10]" `shouldBe` True
            looksLikeHaskellCode "factorial :: Int -> Int" `shouldBe` True
            looksLikeHaskellCode "data Tree = Leaf | Node Tree Tree" `shouldBe` True

        it "does NOT flag ordinary prose or a markdown heading" $ do
            looksLikeHaskellCode "This notebook explores the housing dataset."
                `shouldBe` False
            looksLikeHaskellCode "# Results\n\nThe model fits well." `shouldBe` False

        it "does NOT flag prose that merely mentions a function" $
            looksLikeHaskellCode "We then call animate to play the scene." `shouldBe` False

    describe "staleBindings (N7 — bindings a replace_cell_source no longer defines)" $ do
        it "lists a binding the new source dropped" $
            staleBindings "main = print 1\nx = 2" "x = 2" `shouldBe` ["main"]
        it "is empty when the new source still defines everything (plus more)" $
            staleBindings "x = 1\ny = 2" "x = 1\ny = 2\nz = 3" `shouldBe` []
        it "lists only the dropped binding, not the ones kept" $
            staleBindings "a = 1\nb = 2" "a = 1\nc = 2" `shouldBe` ["b"]
  where
    rejected = maybe False (not . T.null)
