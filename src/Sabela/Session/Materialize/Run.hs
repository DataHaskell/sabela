{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Sabela.Errors.Json (parseJsonInteractive)
import Sabela.Model (CellError (..))
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

textualStderrFailure :: Text -> Maybe Text
textualStderrFailure rawErr
    | T.null cleaned = Nothing
    | isTemplateHaskellOutput rawErr = Nothing
    | not (null jsonErrs) = Just (T.strip (T.unlines (map ceMessage jsonErrs)))
    | not (null (parseErrors residual)) = Just cleaned
    | any (`T.isInfixOf` T.toLower cleaned) runtimeFailureSignals = Just cleaned
    | otherwise = Nothing
  where
    (jsonErrs, _warns, residual) = parseJsonInteractive rawErr
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
