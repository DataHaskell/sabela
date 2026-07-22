{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for the scratch-vet SCOPE REPLAY (evalExpr deep-dive
finding 1).

The live gate showed the vet false-declining a correct candidate (@chainl1@ →
parser-combinators) because GHC printed the goal with the CELL's own type
synonym (@t0 -> Parser (…) -> Parser Double@, where @type Parser = Parsec Void
String@ is declared in the cell) — the scratch replayed the cell's imports but
not its type declarations, so the probe could not parse.

Proposed API (extends Sabela.AI.Capabilities.Edit.ScratchVet):

  cellScopeLines :: Text -> [Text]   -- imports AND single-line type synonyms

'scratchVet' replays 'cellScopeLines' instead of 'cellImportLines'.
-}
module Test.ScratchScopeSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.ScratchVet (cellScopeLines)

-- | The real evalExpr cell shape: pragma, imports, a type synonym, bindings.
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
