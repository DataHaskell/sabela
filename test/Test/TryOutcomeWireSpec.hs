{-# LANGUAGE OverloadedStrings #-}

module Test.TryOutcomeWireSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (isInfixOf)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)
import Test.Hspec

import Sabela.AI.Capabilities.Try (execTry)
import Sabela.AI.Capabilities.Try.Commit (dependenciesAdded)
import Sabela.AI.Capabilities.Try.Payload (disposablePayload, skippedCellHint)
import Sabela.AI.Capabilities.TryPlan (planTrial)
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Server (newApp)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    SkippedCell (..),
 )
import Sabela.SessionTypes (CellLang (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (installHaskellSession)

fakePureBackend :: ST.PureEvalResult -> IO ST.SessionBackend
fakePureBackend result = do
    uid <- newUnique
    pure
        ST.SessionBackend
            { ST.sbSessionId = uid
            , ST.sbJsonDiagnostics = False
            , ST.sbRunBlock = \_ -> pure ("", "")
            , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
            , ST.sbClose = pure ()
            , ST.sbReset = fakePureBackend result
            , ST.sbInterrupt = pure ()
            , ST.sbBusy = pure False
            , ST.sbSessionGen = pure 0
            , ST.sbRequestStale = \_ -> pure False
            , ST.sbQueryComplete = \_ -> pure []
            , ST.sbQueryType = \_ -> pure ""
            , ST.sbQueryInfo = \_ -> pure ""
            , ST.sbQueryKind = \_ -> pure ""
            , ST.sbQueryBrowse = \_ -> pure ""
            , ST.sbQueryBindings = pure ""
            , ST.sbQueryDoc = \_ -> pure ""
            , ST.sbQueryHoleFits = \_ -> pure ""
            , ST.sbEvalPureLive = \_ -> pure result
            }

appWithPureResult :: ST.PureEvalResult -> IO App
appWithPureResult result = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- fakePureBackend result
    nb <- readNotebook (appNotebook app)
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    pure app

pureResult :: ST.PureEvalVerdict -> Text -> Text -> ST.PureEvalResult
pureResult verdict typ err =
    ST.PureEvalResult
        { ST.pureEvalVerdict = verdict
        , ST.pureEvalGeneration = 0
        , ST.pureEvalInferredType = typ
        , ST.pureEvalOutput = "42"
        , ST.pureEvalError = err
        , ST.pureEvalBindingsUnchanged = True
        , ST.pureEvalItUnchanged = True
        , ST.pureEvalRecovery = ST.PureEvalNoRecovery
        }

field :: Text -> Value -> Maybe Value
field key (Object obj) = KM.lookup (Key.fromText key) obj
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of
    Just (String text) -> Just text
    _ -> Nothing

declaringCell :: Int -> Text -> Cell
declaringCell cid pkg =
    Cell
        cid
        CodeCell
        Haskell
        ("-- cabal: build-depends: " <> pkg <> "\nseed = 1")
        []
        Nothing
        False

disposableSample :: DisposableResult
disposableSample =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableOk
        , disposableType = Just "Int"
        , disposableStdout = "42"
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = [1]
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

spec :: Spec
spec = describe "try outcome envelope wire pins" $ do
    it "ok: pure live success is the pure_live route with no reason field" $ do
        app <- appWithPureResult (pureResult ST.PureEvalSucceeded "Int" "")
        outcome <- execTry app (object ["code" .= ("21 * 2" :: Text)])
        toolOutcomeIsError outcome `shouldBe` False
        let v = toolOutcomeValue outcome
        textField "route" v `shouldBe` Just "pure_live"
        textField "verdict" v `shouldBe` Just "ok"
        textField "outcome" v `shouldBe` Just "ok"
        textField "stdout" v `shouldBe` Just "42"
        textField "purityAssurance" v `shouldBe` Just "type_only"
        textField "pollutionContract" v `shouldBe` Just "semantic_read_only"
        textField "recovery" v `shouldBe` Just "none"
        field "reason" v `shouldBe` Nothing

    it "compile error: pure live rejection reports diagnostic/compile_error" $ do
        app <-
            appWithPureResult
                (pureResult ST.PureEvalRejected "" "Variable not in scope: foo")
        outcome <- execTry app (object ["code" .= ("foo" :: Text)])
        toolOutcomeIsError outcome `shouldBe` True
        let v = toolOutcomeValue outcome
        textField "route" v `shouldBe` Just "pure_live"
        textField "verdict" v `shouldBe` Just "diagnostic"
        textField "outcome" v `shouldBe` Just "compile_error"
        textField "stderr" v `shouldBe` Just "Variable not in scope: foo"
        field "reason" v `shouldBe` Nothing

    it "timeout: pure live divergence reports diagnostic/timed_out" $ do
        app <-
            appWithPureResult
                (pureResult ST.PureEvalTimedOut "" "trial exceeded its time budget")
        outcome <- execTry app (object ["code" .= ("length [1..]" :: Text)])
        toolOutcomeIsError outcome `shouldBe` True
        let v = toolOutcomeValue outcome
        textField "route" v `shouldBe` Just "pure_live"
        textField "verdict" v `shouldBe` Just "diagnostic"
        textField "outcome" v `shouldBe` Just "timed_out"
        field "reason" v `shouldBe` Nothing

    it "unavailable: an unsupported language is rejected before anything runs" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        outcome <-
            execTry app (object ["language" .= ("Python" :: Text), "code" .= ("1" :: Text)])
        toolOutcomeIsError outcome `shouldBe` True
        let v = toolOutcomeValue outcome
        textField "route" v `shouldBe` Just "unavailable"
        textField "verdict" v `shouldBe` Just "could-not-run"
        textField "outcome" v `shouldBe` Just "unavailable"
        textField "reason" v `shouldSatisfy` (/= Nothing)

    it
        "plan rejection: multiple candidate expressions are rejected before any code runs"
        $ do
            app <- newApp "." Set.empty Nothing Nothing []
            outcome <- execTry app (object ["code" .= ("1 + 1\n2 + 2" :: Text)])
            toolOutcomeIsError outcome `shouldBe` True
            let v = toolOutcomeValue outcome
            textField "route" v `shouldBe` Just "unavailable"
            textField "verdict" v `shouldBe` Just "diagnostic"
            textField "outcome" v `shouldBe` Just "rejected"
            textField "reason" v
                `shouldBe` Just
                    "cells accept this; try does not, because a trial previews exactly \
                    \one result and cannot follow more than one final expression; a \
                    \committed cell may run as many statements as it likes; no code \
                    \ran. insert_cell is the tool this source goes to."

    it "commit disclosure: a trial carries the source the compiler read" $ do
        app <- appWithPureResult (pureResult ST.PureEvalSucceeded "Int" "")
        outcome <- execTry app (object ["code" .= ("21 * 2" :: Text)])
        textField "source" (toolOutcomeValue outcome) `shouldBe` Just "21 * 2"

    it "commit disclosure: dependencies are the delta, not the closure" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        modifyNotebook
            (appNotebook app)
            (const (Notebook "deps.md" [declaringCell 1 "containers"]))
        let planFor src = case planTrial src of
                Right p -> p
                Left e -> error (show e)
        alreadyThere <-
            dependenciesAdded app (planFor "-- cabal: build-depends: containers\nx = 1")
        alreadyThere `shouldBe` []
        newOne <-
            dependenciesAdded
                app
                (planFor "-- cabal: build-depends: containers, aeson\nx = 1")
        newOne `shouldBe` ["aeson"]

    it "meta query: a read-only type command is routed, not refused" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        outcome <-
            execTry
                app
                (object ["code" .= ("import Data.Map\n:type insert" :: Text)])
        let v = toolOutcomeValue outcome
        textField "routedFrom" v `shouldBe` Just ":type"
        show v `shouldNotSatisfy` isInfixOf "not admitted by try"

    it "meta query: a scope this server cannot open is never an ok trial" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        outcome <-
            execTry
                app
                (object ["code" .= ("import Data.Map\n:type insert" :: Text)])
        let v = toolOutcomeValue outcome
        textField "route" v `shouldBe` Just "check_type"
        (toolOutcomeIsError outcome, textField "outcome" v)
            `shouldBe` (True, Just "unavailable")

    it "meta query: a state-touching command stays refused, naming a tool" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        outcome <- execTry app (object ["code" .= (":cd /tmp" :: Text)])
        toolOutcomeIsError outcome `shouldBe` True
        let v = toolOutcomeValue outcome
        textField "route" v `shouldBe` Just "unavailable"
        textField "reason" v `shouldSatisfy` maybe False ("check_type" `T.isInfixOf`)
        textField "reason" v `shouldSatisfy` maybe False (":cd /tmp" `T.isInfixOf`)

    it "disposable: the route reports isolation as its contract" $ do
        let v = disposablePayload disposableSample
        textField "purityAssurance" v `shouldBe` Just "type_only"
        textField "pollutionContract" v `shouldBe` Just "disposable_session"

    it "disposable: skippedCells renders as {cellId, reason, hint}" $ do
        let result =
                disposableSample
                    { disposableSkippedCells =
                        [SkippedCell 4 "Variable not in scope: notInScope"]
                    }
            v = disposablePayload result
        field "skippedCells" v
            `shouldBe` Just
                ( Array
                    ( pure
                        ( object
                            [ "cellId" .= (4 :: Int)
                            , "reason" .= ("Variable not in scope: notInScope" :: Text)
                            , "hint" .= skippedCellHint 4
                            ]
                        )
                    )
                )

    it "disposable: the skip hint says the cell's names are out of scope" $ do
        skippedCellHint 4 `shouldSatisfy` T.isInfixOf "cell 4"
        skippedCellHint 4 `shouldSatisfy` T.isInfixOf "not in scope"
        skippedCellHint 4 `shouldSatisfy` T.isInfixOf "own unresolved error"

    it
        "disposable replay failure leads with attribution, not the candidate's own error"
        $ do
            let rawDiag =
                    "<interactive>:236:12: error: [GHC-83865]\n  Expecting two more arguments to Point"
                result =
                    disposableSample
                        { disposableVerdict = DisposableCompileError
                        , disposableType = Nothing
                        , disposableStdout = ""
                        , disposableStderr = rawDiag
                        , disposableReplayedCells = [0]
                        , disposableFailure =
                            Just (MaterializeFailure StageCellReplay (Just 4) rawDiag)
                        }
                v = disposablePayload result
            textField "outcome" v `shouldBe` Just "replay_blocked"
            textField "verdict" v `shouldBe` Just "could-not-run"
            field "candidateReached" v `shouldBe` Just (Bool False)
            textField "attribution" v `shouldSatisfy` maybe False ("cell 4" `T.isInfixOf`)
            textField "attribution" v
                `shouldSatisfy` maybe False ("never reached" `T.isInfixOf`)
            textField "stderr" v `shouldBe` Just ""
            let failureMsg = field "failure" v >>= textField "message"
            failureMsg
                `shouldSatisfy` maybe False ("Expecting two more arguments" `T.isInfixOf`)
