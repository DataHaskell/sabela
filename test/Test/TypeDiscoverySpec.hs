{-# LANGUAGE OverloadedStrings #-}

module Test.TypeDiscoverySpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.CellEco (
    CellEco (..),
    FitCand (..),
    cellEco,
    fitProvenance,
    rankFits,
 )

spec :: Spec
spec = describe "type-directed discovery beyond in-scope (intention)" $ do
    describe "cellEco — the ecosystem the cell has committed to" $ do
        it "extracts imported modules and declared build-depends" $
            cellEco
                ( T.unlines
                    [ "-- cabal: build-depends: megaparsec, text"
                    , "import Text.Megaparsec"
                    , "import qualified Text.Megaparsec.Char.Lexer as L"
                    , "digits = undefined"
                    ]
                )
                `shouldBe` CellEco
                    (S.fromList ["Text.Megaparsec", "Text.Megaparsec.Char.Lexer"])
                    (S.fromList ["megaparsec", "text"])

    describe "fitProvenance — capture a hole fit's home module" $ do
        it "reads the (imported from M) line the parser currently drops" $
            fitProvenance
                ( T.unlines
                    [ "  Valid hole fits include"
                    , "    takeWhileP :: Maybe String -> (Token s -> Bool) -> m (Tokens s)"
                    , "      (imported from Text.Megaparsec.Char)"
                    , "    some :: Alternative f => f a -> f [a]"
                    , "      (imported from Control.Applicative)"
                    ]
                )
                `shouldBe` [ ("takeWhileP", "Text.Megaparsec.Char")
                           , ("some", "Control.Applicative")
                           ]

        it "reads the real GHC 9.12 form: smart quotes, close paren on the next line" $
            fitProvenance
                ( T.unlines
                    [ "  Valid hole fits include"
                    , "    genericLength :: (Num i) => [a] -> i"
                    , "      (imported from \8216Data.List\8217"
                    , "       (and originally defined in \8216GHC.Internal.Data.List\8217))"
                    ]
                )
                `shouldBe` [("genericLength", "Data.List")]

    describe "rankFits — type-compatible, cell's ecosystem first" $ do
        let goal = "(Char -> Bool) -> ParsecT Void String Identity String"
            eco = CellEco (S.fromList ["Text.Megaparsec.Char"]) (S.fromList ["megaparsec"])
            good =
                FitCand
                    "takeWhileP"
                    "Maybe String -> (Token s -> Bool) -> m (Tokens s)"
                    "Text.Megaparsec.Char"
                    "megaparsec"
            foreign_ =
                FitCand
                    "takeWhile1"
                    "(Char -> Bool) -> Parser Text"
                    "Data.Attoparsec.Text"
                    "attoparsec"

        it "demotes a foreign type-incompatible fit (attoparsec Parser vs ParsecT goal)" $
            rankFits goal eco [foreign_, good] `shouldBe` [good, foreign_]

        it "keeps an in-ecosystem polymorphic fit" $
            rankFits goal eco [good] `shouldBe` [good]

        it "KEEPS a fit whose concrete result head equals the goal's" $ do
            let matching =
                    FitCand
                        "takeWhileX"
                        "(Char -> Bool) -> ParsecT Void String Identity String"
                        "Some.Parser.Module"
                        "some-parser"
            rankFits goal eco [matching] `shouldBe` [matching]

        it "ranks the cell's own ecosystem module ahead of an equally-typed outsider" $ do
            let outsider =
                    FitCand
                        "takeWhileP"
                        "Maybe String -> (Token s -> Bool) -> m (Tokens s)"
                        "Some.Other.Parser"
                        "other-parser"
            map fcModule (rankFits goal eco [outsider, good])
                `shouldBe` ["Text.Megaparsec.Char", "Some.Other.Parser"]
