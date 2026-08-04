{-# LANGUAGE OverloadedStrings #-}

{- | One 'DisposableResult' and one reading of it, shared by every spec that
asks what `try` and the compile gate say about the same trial.
-}
module Test.DisposableFixtures (
    baseResult,
    gateOf,
    reasonOf,
    messageOf,
    field,
    textField,
    intField,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Sabela.AI.Capabilities.Edit.CompileGate (rejectionJson, submittedOnly)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
 )

baseResult :: DisposableResult
baseResult =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

-- | The gate's own rendering of a result, for the surface-agreement laws.
gateOf :: Text -> DisposableResult -> Value
gateOf src = rejectionJson Nothing Nothing (submittedOnly src) []

-- | The prose a surface offers, wherever that surface puts it.
reasonOf :: Value -> Maybe Text
reasonOf v = case textField "reason" v of
    Just t -> Just t
    Nothing -> textField "error" v

-- | The diagnostic a result carries, blank when it carries none.
messageOf :: DisposableResult -> Text
messageOf = maybe "" failureMessage . disposableFailure

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

intField :: Text -> Value -> Maybe Int
intField k v = case field k v of
    Just (Number d) -> Just (round d)
    _ -> Nothing
