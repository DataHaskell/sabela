{-# LANGUAGE OverloadedStrings #-}

module Test.RepairSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

import Eval.Ollama (ToolCall (..))
import Eval.Repair (substituteAndVerify)

blob :: Text
blob =
    "Valid hole fits include\n\
    \  columnAsList :: Columnable a => Expr a -> DataFrame -> [a]\n\
    \  toColumn :: Columnable a => DataFrame -> [a]"

origSrc :: Text
origSrc = "total = sum (getCol df)"

goal :: Text
goal = "Variable not in scope: getCol :: DataFrame -> [Double]"

{- | A dispatch whose @replace_cell_source@ greens exactly when @greenIf@ accepts
the proposed source, returning the real @execution.ok@ outcome shape so the
verify step is exercised.
-}
mockDispatch ::
    (Text -> Bool) -> IORef [ToolCall] -> ToolCall -> IO (Either Text ToolOutcome)
mockDispatch greenIf ref tc@(ToolCall name args) = do
    modifyIORef' ref (++ [tc])
    pure $
        Right $
            ToolOk $ case name of
                "find_by_type" -> object ["result" .= blob]
                "read_cell" -> object ["source" .= origSrc]
                "replace_cell_source" ->
                    object
                        [ "cellId" .= (1 :: Int)
                        , "execution" .= object ["ok" .= greenIf (newSourceOf args)]
                        ]
                _ -> object []

newSourceOf :: Value -> Text
newSourceOf (Object o) = case KM.lookup (K.fromText "new_source") o of
    Just (String s) -> s
    _ -> ""
newSourceOf _ = ""

lastReplaceSource :: [ToolCall] -> Text
lastReplaceSource calls =
    case reverse [a | ToolCall "replace_cell_source" a <- calls] of
        (a : _) -> newSourceOf a
        [] -> ""

spec :: Spec
spec = describe "Eval.Repair.substituteAndVerify" $ do
    it "substitutes the first fit and keeps it when it compiles" $ do
        ref <- newIORef []
        res <- substituteAndVerify (mockDispatch (const True) ref) 1 goal
        case res of
            Just (ToolCall "replace_cell_source" args, _) ->
                newSourceOf args `shouldBe` "total = sum (columnAsList df)"
            _ -> expectationFailure ("unexpected outcome: " <> show res)
        calls <- readIORef ref
        map tcName calls `shouldBe` ["find_by_type", "read_cell", "replace_cell_source"]

    it "backtracks to the next fit when the first does not compile" $ do
        ref <- newIORef []
        res <- substituteAndVerify (mockDispatch ("toColumn" `T.isInfixOf`) ref) 1 goal
        case res of
            Just (ToolCall "replace_cell_source" args, _) ->
                newSourceOf args `shouldBe` "total = sum (toColumn df)"
            _ -> expectationFailure ("unexpected outcome: " <> show res)
        calls <- readIORef ref
        map tcName calls
            `shouldBe` [ "find_by_type"
                       , "read_cell"
                       , "replace_cell_source"
                       , "replace_cell_source"
                       ]

    it "restores the original source and gives up when no fit compiles" $ do
        ref <- newIORef []
        res <- substituteAndVerify (mockDispatch (const False) ref) 1 goal
        res `shouldSatisfy` isNothing
        calls <- readIORef ref
        map tcName calls
            `shouldBe` [ "find_by_type"
                       , "read_cell"
                       , "replace_cell_source"
                       , "replace_cell_source"
                       , "replace_cell_source"
                       ]
        lastReplaceSource calls `shouldBe` origSrc

    it "does nothing, and asks GHC nothing, when the error has no typed goal" $ do
        ref <- newIORef []
        res <-
            substituteAndVerify
                (mockDispatch (const True) ref)
                1
                "Not in scope: \8216Foo\8217"
        res `shouldSatisfy` isNothing
        calls <- readIORef ref
        calls `shouldSatisfy` null
