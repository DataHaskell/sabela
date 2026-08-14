{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Try.Autofix (
    autofixNote,
    missingDepNote,
    ownerCandidateCap,
    renameCandidateCap,
    scopedCandidateCap,
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

autofixNote :: Text -> Text -> Text
autofixNote pkg repairedCode =
    "Declared build-depends: "
        <> pkg
        <> " for this trial (the module was in a hidden package). Commit this \
           \CURRENT source, which carries the dependency line:\n"
        <> repairedCode

{- | The repair for a module that exists on Hackage under exactly its own
name: the dependency line alone, no rename.
-}
missingDepNote :: Text -> Text -> Text -> Text
missingDepNote pkg modName repairedCode =
    "Declared build-depends: "
        <> pkg
        <> " for this trial (\8216"
        <> modName
        <> "\8217 is in no package the notebook depends on). Commit this \
           \CURRENT source, which carries the dependency line:\n"
        <> repairedCode

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

hiddenPackageOf :: ToolOutcome -> Maybe Text
hiddenPackageOf (ToolOk _) = Nothing
hiddenPackageOf (ToolErr value) =
    listToMaybe (mapMaybe hiddenPackage (diagnosticTexts value))

notFoundModuleOf :: ToolOutcome -> Maybe Text
notFoundModuleOf (ToolOk _) = Nothing
notFoundModuleOf (ToolErr value) =
    listToMaybe (mapMaybe couldNotFindModule (diagnosticTexts value))

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

renameCandidateCap :: Int
renameCandidateCap = 3

{- | Candidates the Hackage facts name for the exact module. Uninstalled, so
each costs a fresh disposable build; the cap keeps a contested module cheap.
-}
ownerCandidateCap :: Int
ownerCandidateCap = 3

{- | Candidates drawn from the packages the cell declares. Higher than the
global cap because a scoped pool needs no similarity floor: every candidate is
already in a package the cell asked for, so the trial decides.
-}
scopedCandidateCap :: Int
scopedCandidateCap = 5
