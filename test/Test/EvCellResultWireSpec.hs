{-# LANGUAGE OverloadedStrings #-}

{- | Pins the @cellResult@ SSE event wire shape. The frontend ('04-sse.js'
→ 'applyErrorMarkers'\/'applyWarningMarkers') and the AI listener
('Sabela.AI.Capabilities.Edit.Run.executeCell') both consume these exact keys,
so @errors@ and @warnings@ are a contract: a 'CellError' list each, carrying
@ceLine@\/@ceCol@\/@ceMessage@.
-}
module Test.EvCellResultWireSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec

import Sabela.Model (CellError (..), NotebookEvent (..))

spec :: Spec
spec = describe "EvCellResult wire shape" $ do
    let warn = CellError (Just 1) (Just 3) "Defined but not used: `x'" (Just 40910)
        err = CellError (Just 2) (Just 7) "Couldn't match type" (Just 83865)
        ev = EvCellResult 5 [] (Just "Couldn't match type") [err] [warn]
        obj = case toJSON ev of
            Object o -> o
            other -> error ("expected object, got " <> show other)
        look k = KM.lookup (Key.fromString k) obj

    it "tags the event type cellResult" $
        look "type" `shouldBe` Just (String "cellResult")

    it "carries errors and warnings as separate keyed arrays" $ do
        look "errors" `shouldBe` Just (toJSON [err])
        look "warnings" `shouldBe` Just (toJSON [warn])

    it "keeps the holistic error and cellId" $ do
        look "cellId" `shouldBe` Just (toJSON (5 :: Int))
        look "error" `shouldBe` Just (String "Couldn't match type")

    it "encodes each diagnostic with ceLine/ceCol/ceMessage/ceCode" $
        case look "warnings" of
            Just (Array arr) -> case toList arr of
                [Object d] -> do
                    KM.lookup "ceLine" d `shouldBe` Just (toJSON (1 :: Int))
                    KM.lookup "ceCol" d `shouldBe` Just (toJSON (3 :: Int))
                    KM.lookup "ceMessage" d
                        `shouldBe` Just (String "Defined but not used: `x'")
                    KM.lookup "ceCode" d `shouldBe` Just (toJSON (40910 :: Int))
                other -> expectationFailure ("expected one warning, got " <> show other)
            other -> expectationFailure ("expected warnings array, got " <> show other)
  where
    toList = foldr (:) []
