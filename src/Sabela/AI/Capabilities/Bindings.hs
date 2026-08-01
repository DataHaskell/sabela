{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Bindings (
    attachWriteEcho,
    execListBindings,
    notebookSource,
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
import Sabela.Model (CellType (..), Notebook, cellSource, cellType, nbCells)
import Sabela.Parse (cellNames)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

execListBindings :: App -> Value -> IO ToolOutcome
execListBindings app _ =
    withBackend app $ \backend -> do
        raw <- normalizeListing <$> sbQueryBindings backend
        nb <- readNotebook (appNotebook app)
        let defined = concatMap cellDefines (nbCells nb)
            (live, _) = partitionLive defined raw
        echoes <- valueEchoes backend live
        let report = liveBindingsReport defined (\n -> join (lookup n echoes)) raw
        pure
            ( guidedOutcome
                (notebookSource nb)
                ["verdict" .= answerVerdict report]
                report
            )

{- | The notebook's code cells, joined: the only source this surface has, and
what guidance about its bindings must not contradict. Prose is left out — a
tutorial quoting a @build-depends:@ line declares nothing.
-}
notebookSource :: Notebook -> Text
notebookSource nb =
    T.unlines [cellSource c | c <- nbCells nb, cellType c == CodeCell]

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
