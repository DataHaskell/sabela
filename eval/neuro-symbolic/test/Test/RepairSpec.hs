{-# LANGUAGE OverloadedStrings #-}

module Test.RepairSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

import Eval.Ollama (ToolCall (..))
import Eval.Repair (substituteAndVerify)

blob :: Text
blob =
    "Valid hole fits include\n\
    \  columnAsList :: Columnable a => Expr a -> DataFrame -> [a]\n\
    \  toColumn :: Columnable a => DataFrame -> [a]"

mockDispatch :: IORef [ToolCall] -> ToolCall -> IO (Either Text ToolOutcome)
mockDispatch ref tc@(ToolCall name _) = do
    modifyIORef' ref (++ [tc])
    pure $
        Right $
            ToolOk $ case name of
                "ghci_query" -> object ["result" .= blob]
                "read_cell" -> object ["source" .= ("total = sum (getCol df)" :: Text)]
                "replace_cell_source" -> object ["cellId" .= (1 :: Int), "ok" .= True]
                _ -> object []

newSourceOf :: Value -> Text
newSourceOf (Object o) = case KM.lookup (K.fromText "new_source") o of
    Just (String s) -> s
    _ -> ""
newSourceOf _ = ""

spec :: Spec
spec = describe "Eval.Repair.substituteAndVerify" $ do
    it "queries hole-fits at the goal, substitutes the top fit, and re-runs" $ do
        ref <- newIORef []
        res <-
            substituteAndVerify
                (mockDispatch ref)
                1
                "Variable not in scope: getCol :: DataFrame -> [Double]"
        case res of
            Just (ToolCall "replace_cell_source" args, Right (ToolOk _)) ->
                newSourceOf args `shouldBe` "total = sum (columnAsList df)"
            _ -> expectationFailure ("unexpected outcome: " <> show res)
        calls <- readIORef ref
        map tcName calls `shouldBe` ["ghci_query", "read_cell", "replace_cell_source"]

    it "does nothing, and asks GHC nothing, when the error has no typed goal" $ do
        ref <- newIORef []
        res <- substituteAndVerify (mockDispatch ref) 1 "Not in scope: \8216Foo\8217"
        res `shouldSatisfy` isNothing
        calls <- readIORef ref
        calls `shouldSatisfy` null
