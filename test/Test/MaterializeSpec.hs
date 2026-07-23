{-# LANGUAGE OverloadedStrings #-}

module Test.MaterializeSpec (spec) where

import Data.Either (isLeft)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (doesFileExist, findExecutable, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Sabela.Model (Notebook (..))
import Sabela.Reactivity (computeFullExecutionPlan, haskellCodeCells)
import Sabela.Server (newApp)
import Sabela.Session.Materialize
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.State (App (..), Environment (..))
import Sabela.State.BridgeStore (setBridgeValue)
import Sabela.State.DependencyTracker (getHaskellDeps)
import qualified Sabela.State.EventBus as EventBus
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (setWidget)
import Test.TopoSpec.Helpers (mkCell)

spec :: Spec
spec = describe "disposable notebook materialization" $ do
    it "merges complete notebook, candidate, and global project metadata" $ do
        let notebook =
                Notebook
                    "materialize"
                    [ mkCell
                        1
                        ( T.unlines
                            [ "-- cabal: build-depends: text"
                            , "-- cabal: default-extensions: LambdaCase"
                            , "-- cabal: ghc-options: -Wno-unused-imports"
                            , "-- cabal: packages: ./notebook-pkg"
                            , "notebookValue = 1"
                            ]
                        )
                    ]
            candidate =
                CandidateSpec
                    { candidateMetadataSource =
                        T.unlines
                            [ "-- cabal: build-depends: containers bytestring"
                            , "-- cabal: default-extensions: TypeApplications"
                            , "-- cabal: ghc-options: -O0"
                            , "-- cabal: packages: ./candidate-pkg"
                            , "notebookValue"
                            ]
                    , candidateSetup = ""
                    , candidateExpression = Just "notebookValue"
                    }
            meta = candidateProjectMeta (Set.singleton "aeson") notebook candidate
        metaDeps meta `shouldMatchList` ["aeson", "bytestring", "containers", "text"]
        metaExts meta
            `shouldSatisfy` (\exts -> all (`elem` exts) ["LambdaCase", "TypeApplications"])
        metaGhcOptions meta `shouldBe` ["-Wno-unused-imports", "-O0"]
        metaPackages meta `shouldMatchList` ["./notebook-pkg", "./candidate-pkg"]

    it "reports an unfaithful notebook plan before starting a scratch process" $ do
        let notebook =
                Notebook
                    "duplicate"
                    [ mkCell 1 "answer = 41"
                    , mkCell 2 "answer = 42"
                    ]
            plan = computeFullExecutionPlan (haskellCodeCells notebook) notebook
        case materializationPlanFailure plan of
            Nothing -> expectationFailure "expected duplicate-definition failure"
            Just failure -> do
                failureStage failure `shouldBe` StagePlan
                failureCellId failure `shouldBe` Just 2
                failureMessage failure `shouldSatisfy` T.isInfixOf "answer"

    it "invalidates the pinned context when any source store or generation changes" $
        withSystemTempDirectory "sabela-materialize-snapshot" $ \workDir -> do
            app <- newApp workDir Set.empty Nothing Nothing []
            initial <- requireSnapshot app
            snapshotStillCurrent app initial `shouldReturn` Right ()

            modifyNotebook (appNotebook app) (\nb -> nb{nbTitle = "changed"})
            snapshotStillCurrent app initial >>= (`shouldSatisfy` isLeft)

            afterNotebook <- requireSnapshot app
            setBridgeValue (appBridge app) "dataset" "v1"
            snapshotStillCurrent app afterNotebook >>= (`shouldSatisfy` isLeft)

            afterBridge <- requireSnapshot app
            setWidget (appWidgets app) 4 "threshold" "0.5"
            snapshotStillCurrent app afterBridge >>= (`shouldSatisfy` isLeft)

            afterWidget <- requireSnapshot app
            _ <- EventBus.bumpGeneration (appEvents app)
            snapshotStillCurrent app afterWidget >>= (`shouldSatisfy` isLeft)

    it
        "replays compiled and interpreted context in a fresh process without touching live state"
        $ withSystemTempDirectory "sabela-materialize-spec"
        $ \workDir -> do
            requireLiveIntegration
            TIO.writeFile (workDir </> "input.txt") "x"
            app <- newApp workDir Set.empty Nothing Nothing [buildTimeSupportDir]
            let notebook =
                    Notebook
                        "materialize"
                        [ mkCell 1 "-- compile: Materialized.Seed\ninc x = x + 1"
                        , mkCell
                            2
                            ( T.unlines
                                [ "putStrLn \"error: harmless replay output\""
                                , "contents <- readFile \"input.txt\""
                                , "seed = inc (length contents)"
                                , "exportBridge \"fromScratch\" (show seed)"
                                , "exportBridge _ _ = error \"shadowed until the next prelude\""
                                ]
                            )
                        , mkCell
                            3
                            ( T.unlines
                                [ "exportBridge \"fromNext\" _bridge_fromScratch"
                                , "bridgeSeed = seed + (read _bridge_fromScratch :: Int) - seed"
                                , "_sabelaEvalPure _ = pure \"shadowed by the last cell\""
                                ]
                            )
                        ]
                candidateSource =
                    T.unlines
                        [ "-- cabal: build-depends: containers"
                        , "import qualified Data.Map.Strict as M"
                        , "bridgeSeed + read _bridge_fromNext + M.size (M.fromList [(1 :: Int, ())]) + 37"
                        ]
                candidate =
                    CandidateSpec
                        { candidateMetadataSource = candidateSource
                        , candidateSetup = "import qualified Data.Map.Strict as M"
                        , candidateExpression =
                            Just
                                "bridgeSeed + read _bridge_fromNext + M.size (M.fromList [(1 :: Int, ())]) + 37"
                        }
            modifyNotebook (appNotebook app) (const notebook)
            notebookBefore <- readNotebook (appNotebook app)
            depsBefore <- getHaskellDeps (appDeps app)
            liveBefore <- getHaskellSession (appSessions app)
            scratchDirsBefore <- scratchDirectories (envTmpDir (appEnv app))

            completed <- timeout (180 * 1000000) (runDisposableTry app candidate)
            result <-
                maybe
                    (expectationFailure "disposable materialization timed out" >> fail "unreachable")
                    pure
                    completed

            disposableRoute result `shouldBe` "disposable_scratch"
            result `shouldSatisfy` ((== DisposableOk) . disposableVerdict)
            disposableType result `shouldSatisfy` maybe False (T.isInfixOf "Int")
            T.strip (disposableStdout result) `shouldBe` "42"
            disposableFailure result `shouldBe` Nothing
            disposableReplayedCells result `shouldBe` [1, 2, 3]
            disposableDependencies result `shouldSatisfy` ("containers" `elem`)

            readNotebook (appNotebook app) `shouldReturn` notebookBefore
            getHaskellDeps (appDeps app) `shouldReturn` depsBefore
            isNothing <$> getHaskellSession (appSessions app) `shouldReturn` True
            isNothing liveBefore `shouldBe` True
            scratchDirectories (envTmpDir (appEnv app)) `shouldReturn` scratchDirsBefore

requireLiveIntegration :: Expectation
requireLiveIntegration = do
    cabal <- findExecutable "cabal"
    case cabal of
        Nothing -> pendingWith "cabal not found on PATH; skipping materialization integration"
        Just _ -> pure ()
    supportPresent <-
        doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
    if supportPresent
        then pure ()
        else
            pendingWith
                "sabela-notebook support source not on disk; skipping materialization integration"

scratchDirectories :: FilePath -> IO [FilePath]
scratchDirectories root =
    filter ("sabela-try" `isPrefixOf`) <$> listDirectory root

requireSnapshot :: App -> IO MaterializeSnapshot
requireSnapshot app = do
    captured <- captureMaterializeSnapshot app
    case captured of
        Left message -> expectationFailure (T.unpack message) >> fail "unreachable"
        Right snapshot -> pure snapshot
