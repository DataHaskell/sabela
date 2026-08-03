{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.Edit.ScratchVet (
    ScopeFailure (..),
    ScopeReport (..),
    cellImportLines,
    cellScopeLines,
    sanitizeGoal,
    scopeEstablishLines,
    scopeEstablished,
    scopeFailureOutcome,
    scopeReportOf,
    runScopeLines,
    scratchScopeBackend,
    scratchScopeReport,
    scratchVet,
    splitArrows,
    vetAlias,
    vetImportLine,
    vetInScope,
    vetProbe,
    vetTimeoutMicros,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (object, (.=))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Scratchpad (ensureScratchpad)
import Sabela.AI.Capabilities.Scratchpad.Payload (withVerdict)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.GoalText (sanitizeGoal, splitArrows)
import Sabela.AI.Health (healthOfTypeQuery, isClean, maskKey, normalizeMsg)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ToolOutcome, errOutcome)
import Sabela.AI.Verdict (VerdictClass (..))
import Sabela.Api (errorJsonWith)
import Sabela.Output (displayPrelude)
import Sabela.SessionTypes (CellLang (..), SessionBackend (..))
import Sabela.State (App)

scratchVet ::
    App -> AIStore -> Text -> [Text] -> Text -> Text -> Maybe Text -> IO Bool
scratchVet _ _ _ _ _ _ Nothing = pure True
scratchVet app store src deps modName name (Just goal) = do
    enabled <- featureEnabled "SABELA_SCRATCH_VET"
    if not enabled
        then pure True
        else do
            r <- try (timeout vetTimeoutMicros go)
            let verdict = case r of
                    Right (Just ok) -> ok
                    _ -> False
            debugDumpVet modName name goal r verdict
            pure verdict
  where
    go = do
        (backend, report) <- scratchScopeReport app store deps src
        either (const False) id
            <$> vetInScope backend report modName name goal

vetTimeoutMicros :: Int
vetTimeoutMicros = 60 * 1000 * 1000

debugDumpVet ::
    Text -> Text -> Text -> Either SomeException (Maybe Bool) -> Bool -> IO ()
debugDumpVet modName name goal r verdict = do
    mp <- lookupEnv "SABELA_DEBUG_VET"
    case mp of
        Just p
            | not (null p)
            , p /= "0" ->
                appendFile p . T.unpack $
                    "vet "
                        <> modName
                        <> "."
                        <> name
                        <> " :: "
                        <> goal
                        <> " -> "
                        <> T.pack (show verdict)
                        <> " ("
                        <> outcome
                        <> ")\n"
        _ -> pure ()
  where
    outcome = case r of
        Left e -> "exception: " <> T.take 120 (T.pack (show e))
        Right Nothing -> "timeout"
        Right (Just _) -> "probe"

{- | A scope line that did not establish what it asked for, kept beside the
words the session used to say so, so a caller reports the failure of the line
and never of the expression it was going to ask about next.
-}
data ScopeFailure = ScopeFailure
    { sfLine :: Text
    , sfDiagnostic :: Text
    , sfMasking :: Bool
    }
    deriving (Eq, Show)

newtype ScopeReport = ScopeReport {scopeFailures :: [ScopeFailure]}
    deriving (Eq, Show)

instance Semigroup ScopeReport where
    ScopeReport a <> ScopeReport b = ScopeReport (a <> b)

instance Monoid ScopeReport where
    mempty = ScopeReport []

scopeEstablished :: ScopeReport -> Bool
scopeEstablished = null . scopeFailures

{- | Reads each scope line's own output back as a verdict on that line: an
import failure by 'maskKey', anything else GHC called an error by its health.
-}
scopeReportOf :: [(Text, Text)] -> ScopeReport
scopeReportOf = ScopeReport . mapMaybe (uncurry scopeLineFailure)

scopeLineFailure :: Text -> Text -> Maybe ScopeFailure
scopeLineFailure line out
    | not failed = Nothing
    | otherwise = Just (ScopeFailure line (firstDiagnostic out) masking)
  where
    masking = maskKey out
    failed = masking || not (isClean (healthOfTypeQuery out))

diagnosticChars :: Int
diagnosticChars = 200

firstDiagnostic :: Text -> Text
firstDiagnostic =
    T.take diagnosticChars
        . normalizeMsg
        . T.unlines
        . take 2
        . filter (not . T.null . T.strip)
        . T.lines

runScopeLine :: SessionBackend -> Text -> IO (Text, Text)
runScopeLine backend line = do
    (out, err) <- sbRunBlock backend line
    pure (line, T.strip (out <> "\n" <> err))

{- | Runs scope lines in order and keeps what the session said about each, so
no line's verdict is discarded and every failure is attributable.
-}
runScopeLines :: SessionBackend -> [Text] -> IO ScopeReport
runScopeLines backend = fmap scopeReportOf . mapM (runScopeLine backend)

{- | Establishes a source's scope in the shared scratchpad and reports which of
its lines the session refused.
-}
scratchScopeReport ::
    App -> AIStore -> [Text] -> Text -> IO (SessionBackend, ScopeReport)
scratchScopeReport app store deps src = do
    backend <- ensureScratchpad app store Haskell deps
    report <- runScopeLines backend (scopeEstablishLines src)
    pure (backend, report)

{- | For callers with nowhere to put a scope failure; every caller that can act
on one should take the report from 'scratchScopeReport' instead.
-}
scratchScopeBackend :: App -> AIStore -> [Text] -> Text -> IO SessionBackend
scratchScopeBackend app store deps src =
    fst <$> scratchScopeReport app store deps src

{- | Vets a candidate inside an established scope. Its own qualified import is
part of that scope, so when the import fails the probe is never issued and the
report — not a type answer about the expression — is what comes back.
-}
vetInScope ::
    SessionBackend ->
    ScopeReport ->
    Text ->
    Text ->
    Text ->
    IO (Either ScopeReport Bool)
vetInScope backend scoped modName name goal = do
    aliased <- runScopeLine backend (vetImportLine modName)
    let report = scoped <> scopeReportOf [aliased]
    if scopeEstablished report
        then
            Right . isClean . healthOfTypeQuery
                <$> sbQueryType backend (vetProbe modName name goal)
        else pure (Left report)

vetImportLine :: Text -> Text
vetImportLine m = "import qualified " <> m <> " as " <> vetAlias m

reportedFailures :: Int
reportedFailures = 3

-- | The refusal a failed scope earns, naming the lines that failed.
scopeFailureOutcome :: ScopeReport -> ToolOutcome
scopeFailureOutcome report =
    errOutcome . withVerdict VerdictDiagnostic $
        errorJsonWith
            "the scope this question needed was not established"
            ["failedScope" .= map failurePairs shown]
  where
    shown = take reportedFailures (scopeFailures report)
    failurePairs f =
        object
            [ "line" .= sfLine f
            , "diagnostic" .= sfDiagnostic f
            , "masking" .= sfMasking f
            ]

{- | The blocks that make a shared scratchpad's scope exactly the one a source
asks for: established, not added to, so a module is in scope only because this
source or the scratchpad's own prelude imported it.
-}
scopeEstablishLines :: Text -> [Text]
scopeEstablishLines src =
    scopeReset : scratchpadBaseScope <> cellScopeLines src

-- | Withdraws every module an earlier caller put in the scratchpad's scope.
scopeReset :: Text
scopeReset = ":module"

{- | The scope every scratchpad starts with, read off the prelude it was
initialised with, so withdrawing a caller's imports does not withdraw the
harness's own.
-}
scratchpadBaseScope :: [Text]
scratchpadBaseScope = cellImportLines displayPrelude

vetAlias :: Text -> Text
vetAlias m = "V_" <> T.replace "." "_" m

cellScopeLines :: Text -> [Text]
cellScopeLines src =
    [ T.strip l
    | l <- T.lines src
    , let s = T.stripStart l
    , "import " `T.isPrefixOf` s || "type " `T.isPrefixOf` s
    ]

vetProbe :: Text -> Text -> Text -> Text
vetProbe modName name goal =
    "("
        <> vetAlias modName
        <> "."
        <> name
        <> T.concat [" (undefined :: " <> a <> ")" | a <- args]
        <> ") `asTypeOf` (undefined :: "
        <> res
        <> ")"
  where
    segs = splitArrows (sanitizeGoal goal)
    (args, res) = case reverse segs of
        (r : rest) -> (reverse rest, r)
        [] -> ([], "")

cellImportLines :: Text -> [Text]
cellImportLines src =
    [T.strip l | l <- T.lines src, "import " `T.isPrefixOf` T.stripStart l]
