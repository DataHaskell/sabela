{-# LANGUAGE OverloadedStrings #-}

{- | Pins the JSON envelope 'execTry' returns for each verdict class it can
answer with (route\/verdict\/outcome\/reason style fields), driven through
the real dispatch with a fake session backend — no live GHCi or cabal build,
mirroring 'Test.ToolOutcomeWireSpec'. A field rename here is a wire break.
-}
module Test.TryOutcomeWireSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)
import Test.Hspec

import Sabela.AI.Capabilities.Try (execTry)
import Sabela.AI.Capabilities.Try.Payload (disposablePayload)
import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.Server (newApp)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    SkippedCell (..),
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (setHaskellSession)

-- | A fake backend whose pure-eval answer is fixed by the caller.
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

{- | An empty-notebook app (so 'liveFastPathReady' matches trivially) wired
to a fake backend answering every pure-eval request with 'result'.
-}
appWithPureResult :: ST.PureEvalResult -> IO App
appWithPureResult result = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- fakePureBackend result
    setHaskellSession (appSessions app) (Just backend)
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

-- | A disposable result with no failure, overriding only the fields a test names.
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
                    \committed cell may run as many statements as it likes; no code ran"

    {- The trial's containment is isolation plus the non-IO admission proof.
    live_test33_wine: -XSafe additionally rejected Data.Csv and
    Network.HTTP.Simple outright, so it was dropped. -}
    it "disposable: the route reports isolation as its contract" $ do
        let v = disposablePayload disposableSample
        textField "purityAssurance" v `shouldBe` Just "type_only"
        textField "pollutionContract" v `shouldBe` Just "disposable_session"

    it "disposable: skippedCells renders as an array of {cellId, reason}" $ do
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
                            ]
                        )
                    )
                )

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
            -- The attribution names the failing cell and says the candidate was not reached.
            textField "attribution" v `shouldSatisfy` maybe False ("cell 4" `T.isInfixOf`)
            textField "attribution" v
                `shouldSatisfy` maybe False ("never reached" `T.isInfixOf`)
            -- The raw diagnostic is NOT surfaced as the candidate's own stderr.
            textField "stderr" v `shouldBe` Just ""
            -- It is retained, cell-labelled, under the failure field for reference.
            let failureMsg = field "failure" v >>= textField "message"
            failureMsg
                `shouldSatisfy` maybe False ("Expecting two more arguments" `T.isInfixOf`)
