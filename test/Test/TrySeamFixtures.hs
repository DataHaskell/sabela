{-# LANGUAGE OverloadedStrings #-}

{- | Fixtures for the try-seam properties: a contained backend that records what
it is handed, a live backend that answers with a canned probe result, and the
poisoned-candidate generator the localisation property ranges over.
-}
module Test.TrySeamFixtures (
    green,
    red,
    recorder,
    tryWith,
    field,
    textField,
    renderAll,
    executingPieces,
    tierOf,
    firstIOMarker,
    ioMarkers,
    ioTypes,
    ioRejection,
    ioDeclined,
    genContainedAnswer,
    genLiveResult,
    tierAgreesWithExecution,
    recordingBackend,
    appWithLive,
    Poisoned (..),
    genPoisoned,
) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)
import ScriptHs.Parser (ScriptFile (..), parseScript)
import ScriptHs.Render (Kind (..), Piece (..), toPieces)
import Test.QuickCheck

import Sabela.AI.Capabilities.Try (execTryWith)
import Sabela.AI.Capabilities.TryPlan (
    TrialPlan (..),
    TrialTier (..),
    planTrial,
 )
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Server (newApp)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    DisposableVerdict (..),
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (installHaskellSession)
import Test.TryFrontierGen
import Test.TryPayloadRead (
    field,
    renderAll,
    textField,
    tierAgreesWithExecution,
 )

green :: DisposableResult
green =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableOk
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

red :: Text -> DisposableResult
red diagnostic =
    green
        { disposableVerdict = DisposableCompileError
        , disposableStderr = diagnostic
        }

{- | What the disposable session reports for a candidate whose inferred type is
IO: green, typed, and carrying the marker that says it was left unrun.
-}
ioDeclined :: Text -> Text -> DisposableResult
ioDeclined typ marker =
    green{disposableType = Just typ, disposableStderr = marker}

{- | The answers a contained backend can give: a value, silence, a diagnostic,
an IO decline, and the three the run does not settle.
-}
genContainedAnswer :: Gen DisposableResult
genContainedAnswer =
    oneof
        [ pure green
        , (\v -> green{disposableStdout = v}) <$> elements ["42", "\"x\"", "()"]
        , red <$> elements ["error: [GHC-88464] Variable not in scope: q", "parse error"]
        , ioDeclined <$> elements ioTypes <*> elements ioMarkers
        , pure green{disposableVerdict = DisposableTimedOut}
        , pure green{disposableVerdict = DisposableUnavailable}
        , pure
            green
                { disposableVerdict = DisposableRuntimeError
                , disposableStderr = "*** Exception: Prelude.head: empty list"
                }
        ]

{- | A live probe result per verdict the evaluator can return, with the session
invariant intact so the answer is the payload rather than a state complaint.
-}
genLiveResult :: Gen ST.PureEvalResult
genLiveResult = do
    verdict <-
        elements
            [ ST.PureEvalSucceeded
            , ST.PureEvalUnshowable
            , ST.PureEvalRejected
            , ST.PureEvalRuntimeError
            , ST.PureEvalTimedOut
            , ST.PureEvalStale
            , ST.PureEvalInvariantFailed
            , ST.PureEvalUnavailable
            ]
    typ <- elements (["Int", "[Char]"] <> ioTypes)
    out <- elements ["", "7"]
    err <- elements ["", "error: [GHC-83865] Couldn't match type"]
    pure
        (ioRejection typ err)
            { ST.pureEvalVerdict = verdict
            , ST.pureEvalOutput = out
            }

{- | A contained backend that records every spec it is handed and answers with
whatever the caller decided that spec deserves.
-}
recorder ::
    IORef [CandidateSpec] ->
    (CandidateSpec -> DisposableResult) ->
    CandidateSpec ->
    IO DisposableResult
recorder seen answer submitted = do
    modifyIORef' seen (<> [submitted])
    pure (answer submitted)

tryWith ::
    App ->
    (CandidateSpec -> DisposableResult) ->
    Text ->
    IO ([CandidateSpec], Value)
tryWith app answer src = do
    seen <- newIORef []
    outcome <- execTryWith (recorder seen answer) app (object ["code" .= src])
    specs <- readIORef seen
    pure (specs, toolOutcomeValue outcome)

-- | The pieces of a setup that GHCi would perform rather than merely compile.
executingPieces :: Text -> [Piece]
executingPieces setup =
    [ p
    | p <- toPieces (scriptLines (parseScript setup))
    , isExecuting p
    ]
  where
    isExecuting (PUnit KAction _) = True
    isExecuting (PUnit KIOBind _) = True
    isExecuting _ = False

tierOf :: Text -> Maybe TrialTier
tierOf src = case planTrial src of
    Right plan -> Just (trialTier plan)
    Left _ -> Nothing

firstIOMarker :: Text
firstIOMarker = "scratch candidate is IO; it was not executed"

ioMarkers :: [Text]
ioMarkers = [firstIOMarker, "SABELA_UNRESTRICTED_IO: candidate is IO"]

ioRejection :: Text -> Text -> ST.PureEvalResult
ioRejection typ marker =
    ST.PureEvalResult
        { ST.pureEvalVerdict = ST.PureEvalRejected
        , ST.pureEvalGeneration = 0
        , ST.pureEvalInferredType = typ
        , ST.pureEvalOutput = ""
        , ST.pureEvalError = marker
        , ST.pureEvalBindingsUnchanged = True
        , ST.pureEvalItUnchanged = True
        , ST.pureEvalRecovery = ST.PureEvalNoRecovery
        }

{- | A backend that records every block it is asked to run, so "nothing was
performed" is a measurement rather than a reading of the code.
-}
recordingBackend :: IORef [Text] -> ST.PureEvalResult -> IO ST.SessionBackend
recordingBackend ran result = do
    backend <- fakeLiveBackend result
    pure
        backend
            { ST.sbRunBlock = \block -> do
                modifyIORef' ran (<> [block])
                pure ("", "")
            }

fakeLiveBackend :: ST.PureEvalResult -> IO ST.SessionBackend
fakeLiveBackend result = do
    uid <- newUnique
    pure
        ST.SessionBackend
            { ST.sbSessionId = uid
            , ST.sbJsonDiagnostics = False
            , ST.sbRunBlock = \_ -> pure ("", "")
            , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
            , ST.sbClose = pure ()
            , ST.sbReset = fakeLiveBackend result
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

appWithLive :: ST.PureEvalResult -> IO App
appWithLive result = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- fakeLiveBackend result
    nb <- readNotebook (appNotebook app)
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    pure app

-- | Independent bindings, exactly one of which does not compile.
data Poisoned = Poisoned
    { poisonSource :: Text
    , poisonNames :: [Text]
    , poisonIndex :: Int
    }

instance Show Poisoned where
    show p =
        "bad="
            <> T.unpack (poisonNames p !! poisonIndex p)
            <> "\n"
            <> T.unpack (poisonSource p)

genPoisoned :: Gen Poisoned
genPoisoned = do
    n <- choose (2, 5) :: Gen Int
    names <- distinctIdents n
    k <- choose (0, n - 1)
    libs <- vectorOf n (elements libGrid)
    lead <- elements libGrid
    let bodies =
            [ if i == k then "notInScopeAnywhere" else libPure l
            | (i, l) <- zip [0 ..] libs
            ]
        decls = [nm <> " = " <> b | (nm, b) <- zip names bodies]
        src =
            "import " <> libModule lead <> "\n\n" <> T.intercalate "\n\n" decls
    pure (Poisoned src names k)

distinctIdents :: Int -> Gen [Text]
distinctIdents n = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc = do
        i <- genIdent
        if i `elem` acc then go k acc else go (k - 1) (i : acc)

-- | The IO types the probe grid reports back.
ioTypes :: [Text]
ioTypes = ["IO ()", "IO [String]", "IO Zubu.Artefact"]
