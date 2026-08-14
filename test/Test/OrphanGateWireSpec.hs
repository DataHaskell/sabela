{-# LANGUAGE OverloadedStrings #-}

{- | The orphan-gate refusal wire shape, pinned at the unit and through the
delete tool itself: the exact key set a client sees, and the proof that a
refused delete commits nothing.
-}
module Test.OrphanGateWireSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.Capabilities.Edit.Delete (execDeleteCell)
import Sabela.AI.Capabilities.Edit.OrphanGate (Orphan (..), orphanRefusal)
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..), newApp)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Test.WorldFixtures (withEnvVars)

keysOf :: Value -> [Text]
keysOf (Object km) = sort (map K.toText (KM.keys km))
keysOf _ = []

valueAt :: Value -> Text -> Maybe Value
valueAt (Object km) k = KM.lookup (K.fromText k) km
valueAt _ _ = Nothing

textAt :: Value -> Text -> Text
textAt v k = case valueAt v k of
    Just (String t) -> t
    _ -> ""

refusalKeys :: [Text]
refusalKeys = ["cellId", "error", "notCommitted", "orphanedImports"]

refusal :: Value
refusal = orphanRefusal 14 [Orphan 3 "Data.List.Split" "split"]

-- | Cell 14 declares split; cell 3 imports it without declaring it.
importerCell :: Cell
importerCell =
    Cell
        3
        CodeCell
        Haskell
        "import Data.List.Split (splitOn)\nx = splitOn \",\" \"a\""
        []
        Nothing
        False

declaringCell :: Cell
declaringCell =
    Cell
        14
        CodeCell
        Haskell
        "-- cabal: build-depends: split\ny = 1"
        []
        Nothing
        False

{- | An app over the two-cell notebook, with the Hackage facts pinned to one
row so the gate resolves split's modules without a store.
-}
withSplitWorld :: (App -> IO a) -> IO a
withSplitWorld act =
    withSystemTempDirectory "sabela-orphan-wire" $ \dir -> do
        let path = dir </> "hackage-facts.tsv"
        TIO.writeFile
            path
            "split\thttps://example.invalid\tsplitting lists\t\
            \Data.List.Split Data.List.Split.Internals\t0.2.5\n"
        withEnvVars [("SABELA_HACKAGE_FACTS", path)] $ do
            app <- newApp "." Set.empty Nothing Nothing []
            modifyNotebook (appNotebook app) $ \nb ->
                nb{nbCells = [importerCell, declaringCell]}
            act app

spec :: Spec
spec = describe "the orphan-gate refusal wire shape" $ do
    describe "orphanRefusal (the unit)" $ do
        it "answers with exactly the pinned keys" $
            keysOf refusal `shouldBe` refusalKeys

        it "marks the refusal uncommitted and names the doomed cell" $ do
            textAt refusal "notCommitted" `shouldBe` "orphaned-imports"
            valueAt refusal "cellId" `shouldBe` Just (toJSON (14 :: Int))

        it "each orphan carries exactly cell, module, and package" $
            case valueAt refusal "orphanedImports" of
                Just (Array os) ->
                    map keysOf (foldr (:) [] os)
                        `shouldBe` [["cellId", "module", "package"]]
                _ -> expectationFailure "no orphanedImports array"

        it "the message states the orphan and the durable fix" $ do
            textAt refusal "error"
                `shouldSatisfy` T.isInfixOf "cell 3 imports Data.List.Split"
            textAt refusal "error"
                `shouldSatisfy` T.isInfixOf "build-depends: split"

    describe "execDeleteCell (the refusal route)" $ do
        it "refuses with the same envelope and commits nothing" $
            withSplitWorld $ \app -> do
                out <- execDeleteCell app (object ["cell_id" .= (14 :: Int)])
                toolOutcomeIsError out `shouldBe` True
                keysOf (toolOutcomeValue out) `shouldBe` refusalKeys
                textAt (toolOutcomeValue out) "notCommitted"
                    `shouldBe` "orphaned-imports"
                nb <- readNotebook (appNotebook app)
                map cellId (nbCells nb) `shouldBe` [3, 14]

        it "a delete the gate allows commits and answers the ok shape" $
            withSplitWorld $ \app -> do
                out <- execDeleteCell app (object ["cell_id" .= (3 :: Int)])
                toolOutcomeIsError out `shouldBe` False
                keysOf (toolOutcomeValue out) `shouldBe` ["cellId", "deleted"]
                nb <- readNotebook (appNotebook app)
                map cellId (nbCells nb) `shouldBe` [14]
