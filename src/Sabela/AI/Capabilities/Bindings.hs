{-# LANGUAGE OverloadedStrings #-}

{- | @list_bindings@ with the value echo of docs/discover/search-api.md §9.2:
nullary pure hole bindings are evaluated within the pinned size\/time budget,
so the model can cite observed values and @= _@ never reaches it (M14).
-}
module Sabela.AI.Capabilities.Bindings (
    attachWriteEcho,
    execListBindings,
    valueEchoes,
) where

import Control.Monad (join)
import Data.Aeson (Value (..), toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Notebook (cellDefines)
import Sabela.AI.Capabilities.Query (guidedOutcome, withBackend)
import Sabela.AI.Types (ToolOutcome)
import Sabela.AI.ValueEcho (
    definedListing,
    echoListing,
    echoTimeLimitMicros,
    holeLines,
    liveBindingsReport,
    normalizeListing,
    nullaryPureType,
    partitionLive,
 )
import Sabela.AI.VerifierDistill (answerVerdict)
import Sabela.Model (nbCells)
import Sabela.Parse (cellNames)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

{- | @list_bindings@: session bindings reconciled against the CURRENT cells'
defines (the same source @list_cells@/Repair.snapshot read, R6.5) — a replaced
cell's leftover binding is flagged stale, never presented as live. Live values
are echoed.
-}
execListBindings :: App -> Value -> IO ToolOutcome
execListBindings app _ =
    withBackend app $ \backend -> do
        raw <- normalizeListing <$> sbQueryBindings backend
        defined <- concatMap cellDefines . nbCells <$> readNotebook (appNotebook app)
        let (live, _) = partitionLive defined raw
        echoes <- valueEchoes backend live
        let report = liveBindingsReport defined (\n -> join (lookup n echoes)) raw
        pure (guidedOutcome ["verdict" .= answerVerdict report] report)

{- | §9.2 write-ack echo: on a clean run, attach the echoed value lines of
the bindings THIS cell defined (@values@), so a success report can cite an
observed value instead of asserting one blind. Identity on a red run.
-}
attachWriteEcho :: App -> Bool -> Text -> Value -> IO Value
attachWriteEcho app ok src v
    | not ok = pure v
    | otherwise = do
        mBackend <- getHaskellSession (appSessions app)
        case mBackend of
            Nothing -> pure v
            Just backend -> do
                raw <- sbQueryBindings backend
                let defined = Set.toList (fst (cellNames src))
                    mine = definedListing defined raw
                echoes <- valueEchoes backend mine
                pure (attachValues (echoedLines echoes mine) v)
  where
    echoedLines echoes mine =
        T.lines (T.stripEnd (echoListing (\n -> join (lookup n echoes)) mine))
    attachValues vals (Object o)
        | not (null vals) = Object (KM.insert "values" (toJSON vals) o)
    attachValues _ val = val

{- | Evaluate each nullary pure hole binding, bounded by
'echoTimeLimitMicros'; a timeout interrupts the probe and reads as no echo,
as does any diagnostic (e.g. no 'Show' instance).
-}
valueEchoes :: SessionBackend -> Text -> IO [(Text, Maybe Text)]
valueEchoes backend raw =
    mapM probe [n | (n, ty) <- holeLines raw, nullaryPureType ty]
  where
    probe n = do
        r <- timeout echoTimeLimitMicros (sbRunBlock backend n)
        case r of
            Nothing -> sbInterrupt backend >> pure (n, Nothing)
            Just (out, err)
                | not (T.null (T.strip err)) -> pure (n, Nothing)
                | T.null (T.strip out) -> pure (n, Nothing)
                | otherwise -> pure (n, Just (T.strip out))
