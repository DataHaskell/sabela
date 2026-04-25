{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.AiDocSpec (spec) where

import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Test.Hspec

import Sabela.AI.Doc (cellHash, defaultDocOpts, renderNotebookDoc)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))

mkCell :: Int -> Text -> Cell
mkCell cid src = Cell cid CodeCell Haskell src [] Nothing False

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup (Key.fromText k) o
lookupKey _ _ = Nothing

encodeToText :: Value -> Text
encodeToText = TL.toStrict . TLE.decodeUtf8 . encode

spec :: Spec
spec = do
    describe "Sabela.AI.Doc.cellHash" $ do
        it "is stable for the same inputs" $
            cellHash (mkCell 1 "x = 1") `shouldBe` cellHash (mkCell 1 "x = 1")

        it "changes when source changes" $
            cellHash (mkCell 1 "x = 1")
                `shouldNotBe` cellHash (mkCell 1 "x = 2")

        it "distinguishes trailing whitespace" $
            cellHash (mkCell 1 "x = 1")
                `shouldNotBe` cellHash (mkCell 1 "x = 1 ")

        it "returns a short non-empty identifier" $ do
            let h = cellHash (mkCell 1 "let y = 42")
            h `shouldSatisfy` (not . T.null)
            T.length h `shouldSatisfy` (< 32)

    describe "Sabela.AI.Doc.renderNotebookDoc" $ do
        let multiSrc = T.unlines ["firstLine = 1", "SECRET_SECOND_LINE = 2", "thirdLine = 3"]
            nb =
                Notebook
                    "Demo"
                    [ mkCell 10 "import Data.List"
                    , mkCell 11 multiSrc
                    ]
            doc = renderNotebookDoc defaultDocOpts nb

        it "round-trips through JSON encode/decode" $ do
            let bs = encode doc
                decoded = decode bs :: Maybe Value
            decoded `shouldBe` Just doc

        it "includes title and cellCount" $ do
            lookupKey "title" doc `shouldBe` Just (String "Demo")
            lookupKey "cellCount" doc `shouldBe` Just (Number 2)

        it "every cell entry has a hash" $ do
            let cells = case lookupKey "cells" doc of
                    Just (Array a) -> a
                    _ -> error "cells not an array"
            length cells `shouldBe` 2
            mapM_ (\c -> lookupKey "hash" c `shouldSatisfy` isJustString) cells

        it "summary keeps only the first line, not the whole source" $ do
            let txt = encodeToText doc
            txt `shouldSatisfy` T.isInfixOf "firstLine = 1"
            txt `shouldNotSatisfy` T.isInfixOf "SECRET_SECOND_LINE"
  where
    isJustString (Just (String s)) = not (T.null s)
    isJustString _ = False
