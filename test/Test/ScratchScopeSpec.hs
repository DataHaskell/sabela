{-# LANGUAGE OverloadedStrings #-}

module Test.ScratchScopeSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.ScratchVet (cellScopeLines)

cellSrc :: T.Text
cellSrc =
    T.unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , "-- cabal: build-depends: megaparsec"
        , "import Text.Megaparsec"
        , "import qualified Text.Megaparsec.Char.Lexer as L"
        , "type Parser = Parsec Void String"
        , "sc :: Parser ()"
        , "sc = L.space space1 empty empty"
        ]

spec :: Spec
spec = describe "scratch scope replay (intention)" $ do
    it "replays the cell's imports" $ do
        cellScopeLines cellSrc
            `shouldSatisfy` elem "import Text.Megaparsec"
        cellScopeLines cellSrc
            `shouldSatisfy` elem "import qualified Text.Megaparsec.Char.Lexer as L"

    it "replays a single-line type synonym — the goal type may be spelled with it" $
        cellScopeLines cellSrc
            `shouldSatisfy` elem "type Parser = Parsec Void String"

    it "does not replay bindings, signatures, pragmas, or cabal comments" $ do
        let ls = cellScopeLines cellSrc
        ls `shouldSatisfy` (not . any ("sc " `T.isPrefixOf`))
        ls `shouldSatisfy` (not . any ("sc ::" `T.isInfixOf`))
        ls `shouldSatisfy` (not . any ("{-#" `T.isPrefixOf`))
        ls `shouldSatisfy` (not . any ("-- cabal:" `T.isPrefixOf`))

    it "preserves source order (a synonym may use an imported type)" $
        cellScopeLines cellSrc
            `shouldBe` [ "import Text.Megaparsec"
                       , "import qualified Text.Megaparsec.Char.Lexer as L"
                       , "type Parser = Parsec Void String"
                       ]
