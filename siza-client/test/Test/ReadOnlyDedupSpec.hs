{-# LANGUAGE OverloadedStrings #-}

module Test.ReadOnlyDedupSpec (readOnlyDedupSpec) where

import Control.Monad (replicateM)
import Data.Aeson (Value, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Tools (renderOutcome)

data Step = Same | Near | New deriving (Eq, Show)

cells :: Text -> Value
cells src =
    object
        [ "cells"
            .= [ object
                    [ "id" .= (0 :: Int)
                    , "defines" .= ([] :: [Text])
                    , "source" .= src
                    ]
               ]
        ]

callFor :: Step -> ToolCall
callFor Same = ToolCall "list_cells" (object [])
callFor Near = ToolCall "list_cells" (object ["full" .= False])
callFor New = ToolCall "kernel_status" (object [])

answerFor :: Step -> Value
answerFor Same = cells "print 5"
answerFor Near = cells "print 5"
answerFor New = object ["state" .= ("idle" :: Text)]

run :: [Step] -> IO ([Text], Int)
run steps = do
    ledger <- newSearchLedger
    calls <- newIORef (0 :: Int)
    let inner tc = do
            modifyIORef' calls (+ 1)
            pure . Right . ToolOk $
                answerFor
                    ( if tcName tc == "kernel_status"
                        then New
                        else if tcArgs tc == object [] then Same else Near
                    )
    outs <- mapM (fmap renderOutcome . guardDiscover ledger inner . callFor) steps
    (,) outs <$> readIORef calls

readOnlyDedupSpec :: Spec
readOnlyDedupSpec = describe "read-only diagnostic dedup" $ do
    it "never suppresses a first occurrence and suppresses every exact repeat" $ do
        let sequences = replicateM 4 [Same, Near, New]
        mapM_ checkSequence sequences

    it "returns a bounded one-line list_cells back-reference with source preview" $ do
        (outs, calls) <- run [Same, Same]
        calls `shouldBe` 1
        let ref = outs !! 1
        T.lines ref `shouldSatisfy` ((== 1) . length)
        T.length ref `shouldSatisfy` (<= 240)
        ref `shouldSatisfy` T.isInfixOf "same as your last list_cells"
        ref `shouldSatisfy` T.isInfixOf "1 cells, cell 0: print 5"

checkSequence :: [Step] -> Expectation
checkSequence xs = do
    (outs, calls) <- run xs
    calls `shouldBe` length (nub (map callFor xs))
    mapM_ inspect (zip3 [0 :: Int ..] xs outs)
  where
    inspect (i, step, out)
        | step `notElem` take i xs =
            out `shouldSatisfy` T.isInfixOf (if step == New then "idle" else "print 5")
        | otherwise = out `shouldSatisfy` T.isInfixOf "same as your last"
