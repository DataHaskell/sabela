{-# LANGUAGE OverloadedStrings #-}

{- | Pins the notebook document shape the editor and the agent both read.

Nothing covered this before: @SizaContractWireSpec@ pins tool names, required
inputs, enums and the result envelope, but never @Cell@ itself. So a field added
to or renamed on @Cell@ changed the wire silently. These assertions are on the
exact key set, which makes any such change test-red rather than compile-green.
-}
module Test.CellWireSpec (spec) where

import Data.Aeson (Value (..), decode, encode, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Sabela.AI.Capabilities.Notebook (cellListEntry)
import Sabela.Model (
    Cell (..),
    CellError (..),
    CellType (..),
    MimeType (MimePlain),
    Notebook (..),
    OutputItem (..),
 )
import qualified Sabela.SessionTypes as ST
import Test.Hspec

ranCell :: Cell
ranCell =
    Cell
        { cellId = 3
        , cellType = CodeCell
        , cellLang = ST.Haskell
        , cellSource = "print x"
        , cellOutputs = [OutputItem MimePlain "7\n"]
        , cellError = Nothing
        , cellDirty = False
        }

objectOf :: Value -> KM.KeyMap Value
objectOf (Object o) = o
objectOf other = error ("expected an object, got " <> show other)

keysOf :: Value -> [String]
keysOf = sort . map Key.toString . KM.keys . objectOf

spec :: Spec
spec = describe "Cell wire shape (the notebook document)" $ do
    it "carries exactly these seven keys" $
        keysOf (toJSON ranCell)
            `shouldBe` [ "cellDirty"
                       , "cellError"
                       , "cellId"
                       , "cellLang"
                       , "cellOutputs"
                       , "cellSource"
                       , "cellType"
                       ]

    it "encodes cellType and cellLang as bare tags the frontend matches on" $ do
        let o = objectOf (toJSON ranCell)
        KM.lookup "cellType" o `shouldBe` Just (String "CodeCell")
        KM.lookup "cellLang" o `shouldBe` Just (String "Haskell")

    it "reports staleness as a boolean named cellDirty" $ do
        let o = objectOf (toJSON ranCell{cellDirty = True})
        KM.lookup "cellDirty" o `shouldBe` Just (Bool True)

    it "encodes a clean cell's error as null, not as an absent key" $
        KM.lookup "cellError" (objectOf (toJSON ranCell)) `shouldBe` Just Null

    it "encodes each output as {oiMime, oiOutput}" $
        case KM.lookup "cellOutputs" (objectOf (toJSON ranCell)) of
            Just (Array arr) -> case foldr (:) [] arr of
                [item] -> do
                    keysOf item `shouldBe` ["oiMime", "oiOutput"]
                    KM.lookup "oiOutput" (objectOf item) `shouldBe` Just (String "7\n")
                other -> expectationFailure ("expected one output, got " <> show other)
            other -> expectationFailure ("expected an array, got " <> show other)

    it "keeps a diagnostic's four fields when a cell failed" $ do
        let failed = ranCell{cellError = Just "Variable not in scope: x"}
        KM.lookup "cellError" (objectOf (toJSON failed))
            `shouldBe` Just (String "Variable not in scope: x")
        keysOf (toJSON (CellError (Just 1) (Just 2) "boom" (Just 40910)))
            `shouldBe` ["ceCode", "ceCol", "ceLine", "ceMessage"]

    it "round-trips, so the editor can send back what it was given" $
        decode (encode ranCell) `shouldBe` Just ranCell

    it "wraps cells in a notebook of exactly nbTitle and nbCells" $
        keysOf (toJSON (Notebook{nbTitle = "n.md", nbCells = [ranCell]}))
            `shouldBe` ["nbCells", "nbTitle"]

    describe "what list_cells shows the agent" $ do
        it "keeps the key set the skill prompt documents" $
            keysOf (cellListEntry False 1 ranCell)
                `shouldBe` [ "defines"
                           , "dirty"
                           , "firstLine"
                           , "hasError"
                           , "hash"
                           , "id"
                           , "lang"
                           , "lineCount"
                           , "position"
                           , "type"
                           ]

        it
            "spells staleness `dirty` and failure `hasError`: renaming either\
            \ silently breaks an agent following the documented contract"
            $ do
                let o = objectOf (cellListEntry False 1 ranCell{cellDirty = True})
                KM.lookup "dirty" o `shouldBe` Just (Bool True)
                KM.lookup "hasError" o `shouldBe` Just (Bool False)

        it "adds source only when asked for the full entry" $ do
            KM.lookup "source" (objectOf (cellListEntry False 1 ranCell))
                `shouldBe` Nothing
            KM.lookup "source" (objectOf (cellListEntry True 1 ranCell))
                `shouldBe` Just (String "print x")
