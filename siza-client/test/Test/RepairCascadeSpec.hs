{-# LANGUAGE OverloadedStrings #-}

module Test.RepairCascadeSpec (repairCascadeSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Repair (repairRedCells)
import Siza.Agent.Repair.Blocking (repairBlockingCell)
import Test.StackFixtures (blockingHoleNotebook, mockNotebook)

repairCascadeSpec :: Spec
repairCascadeSpec =
    describe "the agent cascade end-to-end (verify-and-revert)" $ do
        it "keeps a did-you-mean repair that heals the notebook" $ do
            (disp, lastSrc) <- mockNotebook dymDiag origSrc goodVerdict
            fixes <- repairRedCells disp [(1, dymDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "kept"
            src <- readIORef lastSrc
            src `shouldSatisfy` T.isInfixOf "gust "
        it "reverts and reports attempted-and-reverted when nothing heals" $ do
            (disp, lastSrc) <- mockNotebook dymDiag origSrc (const False)
            fixes <- repairRedCells disp [(1, dymDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "reverted"
            src <- readIORef lastSrc
            src `shouldBe` origSrc
        it "keeps an arity permutation that heals the target (R7.5)" $ do
            (disp, lastSrc) <- mockNotebook arityDiag aritySrc arityHeals
            fixes <- repairRedCells disp [(1, arityDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "kept"
            src <- readIORef lastSrc
            src `shouldBe` "total = plot thePlot vals"
        it "reverts an arity permutation byte-identically when nothing heals" $ do
            (disp, lastSrc) <- mockNotebook arityDiag aritySrc (const False)
            fixes <- repairRedCells disp [(1, arityDiag)]
            fixes `shouldSatisfy` (not . null)
            src <- readIORef lastSrc
            src `shouldBe` aritySrc
        it "confirms a dep-add whose post-restart re-check is clean (R7.3)" $ do
            (disp, lastSrc) <- mockNotebook hiddenDiag depSrc depHeals
            fixes <- repairRedCells disp [(1, hiddenDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) -> do
                reportOf out `shouldSatisfy` T.isInfixOf "kept"
                reportOf out `shouldSatisfy` T.isInfixOf "re-check: cell clean"
                reportOf out
                    `shouldSatisfy` (not . T.isInfixOf "unconfirmed")
            src <- readIORef lastSrc
            src `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
        it "flags a dep-add kept-but-unconfirmed when the cell stays red" $ do
            (disp, lastSrc) <- mockNotebook hiddenDiag depSrc (const False)
            fixes <- repairRedCells disp [(1, hiddenDiag)]
            fixes `shouldSatisfy` (not . null)
            forM_ (take 1 fixes) $ \(_, out) ->
                reportOf out `shouldSatisfy` T.isInfixOf "kept-but-unconfirmed"
            src <- readIORef lastSrc
            src `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
        it "repairs the model-owned blocking cell through a real replacement" $ do
            (disp, calls) <- blockingHoleNotebook producerFits
            fixed <- repairBlockingCell disp 7
            fixed `shouldSatisfy` maybe False (compiledReplacement 7)
            seen <- readIORef calls
            seen `shouldSatisfy` any (isReplacementOf 7)
        it "does not invent a repair when the producer result is genuinely empty" $ do
            (disp, calls) <- blockingHoleNotebook []
            repairBlockingCell disp 7 `shouldReturn` Nothing
            seen <- readIORef calls
            seen `shouldSatisfy` (not . any (isReplacementOf 7))
  where
    goodVerdict s = "gust " `T.isInfixOf` s
    arityHeals s = "plot thePlot vals" `T.isInfixOf` s
    depHeals s = "-- cabal:" `T.isInfixOf` s

arityDiag :: Text
arityDiag =
    "• Couldn't match expected type: Plot -> [(Text, Double)] -> Text\n"
        <> "  with actual type: Text"

aritySrc :: Text
aritySrc = "total = plot vals thePlot"

hiddenDiag :: Text
hiddenDiag =
    "Could not load module `Cumulus.Plot'.\n"
        <> "It is a member of the hidden package `cumulus-0.3.1'."

depSrc :: Text
depSrc = "import Cumulus.Plot\ntotal = bars pairs thePlot"

dymDiag :: Text
dymDiag =
    "Variable not in scope: gustt :: Int -> Wind\n"
        <> "  Perhaps use `gust' (imported from Zephyr.Core)"

origSrc :: Text
origSrc = "total = gustt 3"

{- | The shape find_by_type actually emits: structured fits, not a raw GHC
blob and not a `result` field.
-}
producerFits :: [Value]
producerFits =
    [ object
        [ "write" .= ("mkZephyr" :: Text)
        , "type" .= ("Zephyr" :: Text)
        , "refined" .= False
        ]
    ]

compiledReplacement :: Int -> (ToolCall, Either Text ToolOutcome) -> Bool
compiledReplacement cid (call, Right (ToolOk (Object o))) =
    isReplacementOf cid call
        && case KM.lookup "execution" o of
            Just (Object e) -> KM.lookup "ok" e == Just (Bool True)
            _ -> False
compiledReplacement _ _ = False

isReplacementOf :: Int -> ToolCall -> Bool
isReplacementOf cid (ToolCall name (Object args)) =
    name == "replace_cell_source"
        && KM.lookup "cell_id" args == Just (Number (fromIntegral cid))
isReplacementOf _ _ = False

reportOf :: Either Text ToolOutcome -> Text
reportOf (Right (ToolOk (Object o))) = case KM.lookup "repair" o of
    Just (String s) -> s
    _ -> ""
reportOf _ = ""
