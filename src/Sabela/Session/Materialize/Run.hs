{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Checked block execution for the disposable materialization route: run a
GHCi block, distinguish a genuine compiler/runtime failure from harmless
Template Haskell chatter or linker noise, and return either the diagnostic or
the captured @(stdout, stderr)@. Extracted from 'Sabela.Session.Materialize'
to keep that module within the size cap; carries no 'MaterializeStage' so the
dependency stays one-directional.
-}
module Sabela.Session.Materialize.Run (
    runChecked,
    runLoadChecked,
    runOptional,
    textualStderrFailure,
) where

import Control.Exception (SomeException, displayException, try)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Bridge (isTemplateHaskellOutput)
import Sabela.Errors (parseErrors)
import qualified Sabela.SessionTypes as ST

runOptional :: ST.SessionBackend -> Text -> IO (Either Text (Text, Text))
runOptional backend source
    | T.null (T.strip source) = pure (Right ("", ""))
    | otherwise = runChecked backend source

runChecked :: ST.SessionBackend -> Text -> IO (Either Text (Text, Text))
runChecked = runCheckedWith False

runLoadChecked :: ST.SessionBackend -> Text -> IO (Either Text (Text, Text))
runLoadChecked = runCheckedWith True

runCheckedWith ::
    Bool ->
    ST.SessionBackend ->
    Text ->
    IO (Either Text (Text, Text))
runCheckedWith checkLoadOutput backend source = do
    outcome <-
        try (ST.sbRunBlock backend source) ::
            IO (Either SomeException (Text, Text))
    pure $ case outcome of
        Left e -> Left (T.pack (displayException e))
        Right pair@(out, err)
            | Just message <- textualStderrFailure err -> Left message
            | checkLoadOutput, loadFailed out -> Left (T.strip out)
            | otherwise -> Right pair

-- Keep the textual scratch path aligned with the normal non-JSON cell engine:
-- compiler diagnostics and runtime exceptions live on stderr; Template
-- Haskell chatter and linker warnings are harmless.  Stdout is user output,
-- except for GHCi's structural @Failed,@ line from a @:load@ command.
textualStderrFailure :: Text -> Maybe Text
textualStderrFailure rawErr
    | T.null cleaned = Nothing
    | isTemplateHaskellOutput rawErr = Nothing
    | not (null errs) = Just cleaned
    | any (`T.isInfixOf` T.toLower cleaned) runtimeFailureSignals = Just cleaned
    | otherwise = Nothing
  where
    errs = parseErrors rawErr
    cleaned =
        T.strip . T.unlines . filter (not . isLinkerNoise) . T.lines $ rawErr
    isLinkerNoise line = "ld: warning:" `T.isPrefixOf` T.strip line
    runtimeFailureSignals =
        [ "*** exception"
        , "execution timed out"
        , "interrupted"
        , "repl failed"
        , "kernel was killed"
        ]

loadFailed :: Text -> Bool
loadFailed =
    any (("failed," `T.isPrefixOf`) . T.toLower . T.stripStart) . T.lines
