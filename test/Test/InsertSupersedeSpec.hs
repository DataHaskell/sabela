{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for INSERT SUPERSESSION: an @insert_cell@ that rewrites
the same definitions as the pending RED cell is a fix attempt, not a new cell —
it must redirect to @replace_cell_source@ on that cell (running the full repair
cascade on the new source) instead of being rejected with the old error blob
re-dumped into the model's context.

Proposed API (Sabela.AI.Capabilities.Edit.Admit):

  supersedesRedCell :: Text -> Text -> Bool  -- new source -> red source
  supersedeNote     :: Int -> Text           -- the response note
-}
module Test.InsertSupersedeSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.Admit (
    restickCabal,
    supersedeNote,
    supersedesRedCell,
 )

-- | The red cell (abridged from the gate transcript).
redSrc :: Text
redSrc =
    T.unlines
        [ "import Text.Megaparsec"
        , "type Parser = Parsec Void String"
        , "numberParser :: Parser Double"
        , "numberParser = lexeme (takeWhile1 (\\c -> isDigit c))"
        , "evalExpr :: String -> Maybe Double"
        , "evalExpr input = Nothing"
        ]

-- | The model's follow-up insert: a revision defining the same names.
revisionSrc :: Text
revisionSrc =
    T.unlines
        [ "import Text.Megaparsec"
        , "type Parser = Parsec Void String"
        , "numberParser :: Parser Double"
        , "numberParser = lexeme (takeWhileP Nothing (\\c -> isDigit c))"
        , "evalExpr :: String -> Maybe Double"
        , "evalExpr input = parseMaybe numberParser input"
        ]

spec :: Spec
spec = describe "insert supersession (intention)" $ do
    describe "supersedesRedCell — a rewrite of the same definitions" $ do
        it "a revision defining the same names supersedes" $
            supersedesRedCell revisionSrc redSrc `shouldBe` True
        it "an unrelated cell does not" $
            supersedesRedCell "salesPlot = displaySvg chart" redSrc
                `shouldBe` False
        it "an empty insert does not" $
            supersedesRedCell "" redSrc `shouldBe` False
        it "prose-like source with no definitions does not" $
            supersedesRedCell "-- just a comment" redSrc `shouldBe` False

    describe "supersedeNote — the model is told what happened" $ do
        it "names the replaced cell and the reason" $ do
            let n = supersedeNote 0
            n `shouldSatisfy` T.isInfixOf "cell 0"
            n `shouldSatisfy` T.isInfixOf "REPLACED"
        it "explains the redirect trigger" $
            supersedeNote 0 `shouldSatisfy` T.isInfixOf "same definitions"

    describe "restickCabal — a replace must not silently drop the dep line" $ do
        -- Measured: the model re-submitted the cell without the repair-added
        -- `-- cabal:` line, costing a kernel restart and ~90s of the episode.
        let old = "-- cabal: build-depends: megaparsec\nimport Text.Megaparsec\nx = 1"
            newNoCabal = "import Text.Megaparsec\nx = 2"
        it "re-adds the old cabal line when the new source drops it but still imports" $
            restickCabal old newNoCabal
                `shouldBe` "-- cabal: build-depends: megaparsec\nimport Text.Megaparsec\nx = 2"
        it "keeps the new source's own cabal line untouched" $ do
            let newOwn = "-- cabal: build-depends: aeson\nimport Data.Aeson\nx = 2"
            restickCabal old newOwn `shouldBe` newOwn
        it "does not stick when the new source imports nothing (deliberate rewrite)" $
            restickCabal old "x = 2" `shouldBe` "x = 2"
        it "no-op when the old source had no cabal line" $
            restickCabal "import A\nx = 1" newNoCabal `shouldBe` newNoCabal
