{-# LANGUAGE OverloadedStrings #-}

module Test.ArtifactSpec (artifactSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (Null), object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Owned (
    OwnedCell (..),
    artifactAttempted,
    hasArtifact,
    recordOwned,
 )
import Test.ArtifactFixtures

cell :: Text -> OwnedCell
cell src =
    OwnedCell
        { ocHealthy = True
        , ocExecuted = True
        , ocDiagnostic = ""
        , ocSource = src
        , ocInvariantAlarm = False
        , ocArtifactEligible = True
        , ocHash = Nothing
        }

owned :: [Text] -> Map.Map Int OwnedCell
owned srcs = Map.fromList (zip [0 ..] (map cell srcs))

artifactSpec :: Spec
artifactSpec = describe "an artifact is a cell that does something" $ do
    it "a dependency-only cell is not an artifact" $
        hasArtifact (owned ["-- cabal: build-depends: QuickCheck"]) `shouldBe` False

    it "several dependency-only cells are still not an artifact" $
        hasArtifact
            (owned ["-- cabal: build-depends: text", "-- cabal: build-depends: mtl"])
            `shouldBe` False

    it "a comment-only cell is not an artifact" $
        hasArtifact (owned ["-- we will use this for plotting"]) `shouldBe` False

    it "a pragma-only cell is not an artifact" $
        hasArtifact (owned ["{-# LANGUAGE OverloadedStrings #-}"]) `shouldBe` False

    it "a cell with code is an artifact" $
        hasArtifact (owned ["x = 1 + 1"]) `shouldBe` True

    it "code alongside its dependency line is an artifact" $
        hasArtifact (owned ["-- cabal: build-depends: text\nimport Data.Text\nx = 1"])
            `shouldBe` True

    it "one real cell among dependency lines is an artifact" $
        hasArtifact (owned ["-- cabal: build-depends: text", "main = print 1"])
            `shouldBe` True

    it "an empty owned map is no artifact" $
        hasArtifact (owned []) `shouldBe` False

    it "substantive source is not an artifact until it executed" $
        hasArtifact (Map.singleton 0 (cell "x = 1"){ocExecuted = False})
            `shouldBe` False

    it "an executed red cell is not an artifact" $
        hasArtifact (Map.singleton 0 (cell "x = unresolved"){ocHealthy = False})
            `shouldBe` False

    it "an explicit CodeCell with no execution is still an artifact attempt" $ do
        let call =
                ToolCall
                    "insert_cell"
                    ( object
                        [ "cell_type" .= ("CodeCell" :: Text)
                        , "source" .= ("x = 1" :: Text)
                        ]
                    )
            outcome =
                Right
                    ( ToolOk
                        (object ["cellId" .= (0 :: Int), "execution" .= Null])
                    )
        artifactAttempted (call, outcome) `shouldBe` True

    describe "deferred artifact transitions" $ do
        forM_ [deferredInsert, deferredReplace] $ \write ->
            it ("promotes " <> show (tcName write) <> " after execute_cell succeeds") $ do
                let waiting = recordOwned (write, deferred 7) Map.empty
                    executed = recordOwned (execute 7, execution 7 True) waiting
                hasArtifact waiting `shouldBe` False
                hasArtifact executed `shouldBe` True
                ocSource <$> Map.lookup 7 executed `shouldBe` Just "answer = 42"

        it "does not promote pending ids before their executions settle" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                draining = recordOwned (runPending, pendingOutcome [7, 99]) waiting
            hasArtifact draining `shouldBe` False
            ocExecuted <$> Map.lookup 7 draining `shouldBe` Just False
            Map.keys draining `shouldBe` [7]

        it "promotes a deferred cell after a settled clean snapshot" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                draining = recordOwned (runPending, pendingOutcome [7]) waiting
                settled = recordOwned (listCells, cellsOutcome [(7, False, False)]) draining
            hasArtifact settled `shouldBe` True
            ocHealthy <$> Map.lookup 7 settled `shouldBe` Just True
            ocExecuted <$> Map.lookup 7 settled `shouldBe` Just True

        it "never promotes a prose cell after a clean snapshot" $ do
            let waiting = recordOwned (proseInsert, deferred 7) Map.empty
                settled =
                    recordOwned
                        (listCells, cellsOutcomeOf "ProseCell" [(7, False, False)])
                        waiting
            hasArtifact settled `shouldBe` False
            ocArtifactEligible <$> Map.lookup 7 settled `shouldBe` Just False
            ocExecuted <$> Map.lookup 7 settled `shouldBe` Just True

        it "learns that a replaced prose cell is not artifact-eligible" $ do
            let waiting = recordOwned (deferredReplace, deferred 7) Map.empty
                settled =
                    recordOwned
                        (listCells, cellsOutcomeOf "ProseCell" [(7, False, False)])
                        waiting
            hasArtifact settled `shouldBe` False
            ocArtifactEligible <$> Map.lookup 7 settled `shouldBe` Just False

        it "leaves a dirty snapshot unexecuted after await_idle" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                idle = recordOwned (awaitIdle, Right (ToolOk (object ["idle" .= True]))) waiting
                dirty = recordOwned (listCells, cellsOutcome [(7, True, False)]) idle
            hasArtifact idle `shouldBe` False
            hasArtifact dirty `shouldBe` False
            ocExecuted <$> Map.lookup 7 dirty `shouldBe` Just False

        it "turns a previously healthy cell red when the snapshot has an error" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                green = recordOwned (execute 7, execution 7 True) waiting
                red = recordOwned (listCells, cellsOutcome [(7, False, True)]) green
            hasArtifact red `shouldBe` False
            ocHealthy <$> Map.lookup 7 red `shouldBe` Just False
            ocExecuted <$> Map.lookup 7 red `shouldBe` Just True
            ocDiagnostic <$> Map.lookup 7 red
                `shouldBe` Just "list_cells observed hasError=true for cell 7"

        it "replaces the summary with the cell's actual error when read" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                red = recordOwned (listCells, cellsOutcome [(7, False, True)]) waiting
                diagnosed =
                    recordOwned (readCell, readOutcome 7 (Just "Variable not in scope: total")) red
            ocDiagnostic <$> Map.lookup 7 diagnosed
                `shouldBe` Just "Variable not in scope: total"

        it "records a truthful diagnostic when the failing-cell read fails" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                red = recordOwned (listCells, cellsOutcome [(7, False, True)]) waiting
                diagnosed = recordOwned (readCell, Left "transport timeout") red
            ocDiagnostic <$> Map.lookup 7 diagnosed
                `shouldBe` Just "read_cell failed while retrieving the diagnostic: transport timeout"

        it "removes an owned id absent from a valid notebook snapshot" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                gone = recordOwned (listCells, cellsOutcome []) waiting
            Map.notMember 7 gone `shouldBe` True

        it "ignores an absent or malformed cells snapshot" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                absent = recordOwned (listCells, Right (ToolOk (object []))) waiting
                malformed =
                    recordOwned
                        (listCells, Right (ToolOk (object ["cells" .= [object ["id" .= (7 :: Int)]]])))
                        waiting
            Map.member 7 absent `shouldBe` True
            Map.member 7 malformed `shouldBe` True

        it "invalidates stale source when a snapshot hash changed out of band" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                replaced =
                    recordOwned
                        ( listCells
                        , cellsOutcomeWithHash "CodeCell" [(7, False, False, "other")]
                        )
                        waiting
            Map.notMember 7 replaced `shouldBe` True

        it "treats the structured Deferred summary as healthy but unexecuted" $ do
            let waiting = recordOwned (deferredInsert, structuredDeferred 7) Map.empty
            ocHealthy <$> Map.lookup 7 waiting `shouldBe` Just True
            ocExecuted <$> Map.lookup 7 waiting `shouldBe` Just False
            hasArtifact waiting `shouldBe` False

        it "a later execution failure clears stale health without losing source" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                green = recordOwned (execute 7, execution 7 True) waiting
                red = recordOwned (execute 7, execution 7 False) green
            hasArtifact green `shouldBe` True
            hasArtifact red `shouldBe` False
            ocHealthy <$> Map.lookup 7 red `shouldBe` Just False
            ocExecuted <$> Map.lookup 7 red `shouldBe` Just True
            ocSource <$> Map.lookup 7 red `shouldBe` Just "answer = 42"

        it "a failed run_pending call does not promote a deferred artifact" $ do
            let waiting = recordOwned (deferredInsert, deferred 7) Map.empty
                failed = recordOwned (runPending, Left "transport failure") waiting
            hasArtifact failed `shouldBe` False
