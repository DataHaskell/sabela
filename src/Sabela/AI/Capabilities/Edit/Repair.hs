{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.Repair (
    firstFix,
    dependencySuggestion,
    moduleDepStep,
    goalOfName,
    notInScopeNames,
    resultErrorText,
) where

import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as S
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.DepRepair (addBuildDepend, depFromResult)
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.HoleRepair (goalSpans)
import Sabela.AI.ImportRepair (moduleRenameFix)
import Sabela.AI.ModuleResolve (closestModules)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (couldNotFindModule, misnamedModule, notInScopeName)
import Sabela.Diagnose.Packages (findModulePackage)
import Sabela.Model (CellError (..))
import Sabela.Parse (cellNames)

goalOfName :: Either Text ExecutionResult -> Text -> Maybe Text
goalOfName res name =
    listToMaybe [ty | (w, ty, _) <- goalSpans res, w == name]

moduleFuzzyThreshold :: Double
moduleFuzzyThreshold = 0.2

resultErrorText :: Either Text ExecutionResult -> Text
resultErrorText (Left e) = e
resultErrorText (Right er) =
    T.unlines (maybe [] pure (erError er) ++ map ceMessage (erErrors er))

notInScopeNames :: Text -> Either Text ExecutionResult -> [Text]
notInScopeNames src res@(Right er) =
    nub . filter usable $
        concatMap (mapMaybe notInScopeName . T.lines) errorTexts
            ++ [w | (w, _, _) <- goalSpans res]
  where
    errorTexts = maybe [] pure (erError er) ++ map ceMessage (erErrors er)
    usable w = not (T.null w) && not (w `S.member` defs)
    defs = fst (cellNames src)
notInScopeNames _ (Left _) = []

repairFixers :: [Either Text ExecutionResult -> Text -> Maybe Text]
repairFixers =
    [ \res src -> (`addExtension` src) <$> extFromResult res
    , moduleRenameFix
    ]

dependencySuggestion ::
    Either Text ExecutionResult -> Text -> Maybe (Text, Text)
dependencySuggestion res src = do
    pkg <- depFromResult res
    let src' = addBuildDepend pkg src
    if src' == src then Nothing else Just (pkg, src')

moduleDepStep :: Either Text ExecutionResult -> Text -> IO (Maybe Text)
moduleDepStep res src = do
    enabled <- featureEnabled "SABELA_MODULE_RESOLVE"
    if enabled then moduleDepFix res src else pure Nothing

{- | Declare the package that exposes the module GHC could not find, asking the
installed package db about the module itself rather than fuzzy-matching the
name against a hand-written table.
-}
moduleDepFix :: Either Text ExecutionResult -> Text -> IO (Maybe Text)
moduleDepFix res src
    | Just wrong <- couldNotFindModule errText
    , isNothing (misnamedModule errText) = do
        mPkg <- findModulePackage wrong
        pure $ do
            pkg <- mPkg
            let src' = addBuildDepend pkg src
            guard (src' /= src)
            pure src'
    | otherwise = pure Nothing
  where
    errText = resultErrorText res

firstFix :: Either Text ExecutionResult -> Text -> Maybe Text
firstFix res src =
    listToMaybe [s | fixer <- repairFixers, Just s <- [fixer res src], s /= src]
