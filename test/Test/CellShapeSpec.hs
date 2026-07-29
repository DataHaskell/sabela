{-# LANGUAGE OverloadedStrings #-}

module Test.CellShapeSpec (spec) where

import qualified Data.Text as T
import Sabela.Model (CellType (..))
import Sabela.Parse (staleBindings, unparseableChunks, validateCellShape)
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

    describe "unwrapMain (where bindings become lets above first use)" $ do
        it "gives each independent binding its own let above its own use" $
            unwrapMain
                ( T.unlines
                    [ "main = do"
                    , "  putStrLn (greeting \"world\")"
                    , "  print 0"
                    , "  putStrLn farewell"
                    , "  where"
                    , "    greeting w = \"hi \" <> w"
                    , "    farewell = \"bye\""
                    ]
                )
                `shouldBe` T.unlines
                    [ "do"
                    , "  let greeting w = \"hi \" <> w"
                    , "  putStrLn (greeting \"world\")"
                    , "  print 0"
                    , "  let farewell = \"bye\""
                    , "  putStrLn farewell"
                    ]

        it "hoists a where binding to a let above its first use" $
            unwrapMain
                ( T.unlines
                    [ "main :: IO ()"
                    , "main = do"
                    , "  putStrLn \"start\""
                    , "  print (double 2)"
                    , "  where"
                    , "    double n = n * 2"
                    ]
                )
                `shouldBe` T.unlines
                    [ "do"
                    , "  putStrLn \"start\""
                    , "  let double n = n * 2"
                    , "  print (double 2)"
                    ]

        it "puts the let at the top when the first statement uses it" $
            unwrapMain
                ( T.unlines
                    [ "main = do"
                    , "  print (double 2)"
                    , "  putStrLn \"done\""
                    , "  where"
                    , "    double n = n * 2"
                    ]
                )
                `shouldBe` T.unlines
                    [ "do"
                    , "  let double n = n * 2"
                    , "  print (double 2)"
                    , "  putStrLn \"done\""
                    ]

        it "groups bindings that reference each other into one let" $
            unwrapMain
                ( T.unlines
                    [ "main = do"
                    , "  putStrLn \"areas:\""
                    , "  print (area 3)"
                    , "  where"
                    , "    area r = tau * r * r"
                    , "    tau = 6.28"
                    ]
                )
                `shouldBe` T.unlines
                    [ "do"
                    , "  putStrLn \"areas:\""
                    , "  let area r = tau * r * r"
                    , "      tau = 6.28"
                    , "  print (area 3)"
                    ]

        it "keeps a multi-line, multi-equation binding intact and parseable" $ do
            let out =
                    unwrapMain
                        ( T.unlines
                            [ "main :: IO ()"
                            , "main = do"
                            , "  mapM_ printItem [0, 1]"
                            , "  where"
                            , "    printItem 0 = do"
                            , "      putStrLn \"zero\""
                            , "      pure ()"
                            , "    printItem n = print n"
                            ]
                        )
            out
                `shouldBe` T.unlines
                    [ "do"
                    , "  let printItem 0 = do"
                    , "        putStrLn \"zero\""
                    , "        pure ()"
                    , "      printItem n = print n"
                    , "  mapM_ printItem [0, 1]"
                    ]
            unparseableChunks out `shouldBe` []

        it "carries a signature with its binding" $
            unwrapMain
                ( T.unlines
                    [ "main = do"
                    , "  print (val + 1)"
                    , "  where"
                    , "    val :: Int"
                    , "    val = 41"
                    ]
                )
                `shouldBe` T.unlines
                    [ "do"
                    , "  let val :: Int"
                    , "      val = 41"
                    , "  print (val + 1)"
                    ]

        it "places an unused where binding at the top of the do block" $
            unwrapMain
                ( T.unlines
                    [ "main = do"
                    , "  putStrLn \"hi\""
                    , "  where"
                    , "    unused = 99"
                    ]
                )
                `shouldBe` T.unlines
                    [ "do"
                    , "  let unused = 99"
                    , "  putStrLn \"hi\""
                    ]

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
