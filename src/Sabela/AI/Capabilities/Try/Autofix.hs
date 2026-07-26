{-# LANGUAGE OverloadedStrings #-}

{- | The pure half of try's mechanical autofix rungs: the diagnostics they
key on and the R7.1 disclosure notes they answer with. The retry driver stays
in "Sabela.AI.Capabilities.Try", which owns the trial run itself.
-}
module Sabela.AI.Capabilities.Try.Autofix (
    autofixNote,
    renameCandidateCap,
    renameNote,
    hiddenPackageOf,
    notFoundModuleOf,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.Diagnose (couldNotFindModule, hiddenPackage)

{- | R7.1: the trial declared the dependency itself, so it owes the caller the
source that carries it. Committing the pre-repair source would fail the gate,
which declares no dependency the model did not write.
-}
autofixNote :: Text -> Text -> Text
autofixNote pkg repairedCode =
    "Declared build-depends: "
        <> pkg
        <> " for this trial (the module was in a hidden package). Commit this \
           \CURRENT source, which carries the dependency line:\n"
        <> repairedCode

{- | R7.1 for the rename rung: name both corrections and hand back the source
that carries them, so a commit never silently differs from the trial.
-}
renameNote :: Text -> Text -> Text -> Text -> Text
renameNote wrong right pkg repairedCode =
    "No module \8216"
        <> wrong
        <> "\8217 exists; the nearest installed module is \8216"
        <> right
        <> "\8217 ("
        <> pkg
        <> "), so this trial ran against it with the dependency declared. \
           \Commit this CURRENT source:\n"
        <> repairedCode

{- | The hidden package named by a rejected trial, read from the fields that
actually carry the compiler's words.
-}
hiddenPackageOf :: ToolOutcome -> Maybe Text
hiddenPackageOf (ToolOk _) = Nothing
hiddenPackageOf (ToolErr value) =
    listToMaybe (mapMaybe hiddenPackage (diagnosticTexts value))

-- | The module a rejected trial could not find, read from its diagnostics.
notFoundModuleOf :: ToolOutcome -> Maybe Text
notFoundModuleOf (ToolOk _) = Nothing
notFoundModuleOf (ToolErr value) =
    listToMaybe (mapMaybe couldNotFindModule (diagnosticTexts value))

{- | Every field the compiler's words reach the outcome through, the nested
@failure.message@ of a disposable rejection included — the shape try's own
candidate-setup failures arrive in.
-}
diagnosticTexts :: Value -> [Text]
diagnosticTexts value =
    [ text
    | key <- ["stderr", "error", "diagnostic"]
    , Just (String text) <- [lookupField key value]
    ]
        ++ [ text
           | Just failure <- [lookupField "failure" value]
           , Just (String text) <- [lookupField "message" failure]
           ]

lookupField :: Text -> Value -> Maybe Value
lookupField key (Object obj) = KM.lookup (Key.fromText key) obj
lookupField _ _ = Nothing

{- | Most nearest-name renames one failed trial may attempt. Each retry is a
disposable run, so the ladder is short; past it the original diagnostic
answers.
-}
renameCandidateCap :: Int
renameCandidateCap = 3
