{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.Repair.Resolvers (
    moduleResolveCandidates,
    importResolveCandidates,
    qualifiedImportCandidates,
    ambiguousResolveCandidates,
    ambiguousCandidates,
    hoogleCandidates,
    pathNearMissCandidates,
    typeDiscoverCandidates,
) where

import Control.Monad (filterM)
import Data.List (nub)
import Data.Maybe (isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair (
    goalOfName,
    notInScopeNames,
    resultErrorText,
 )
import Sabela.AI.Capabilities.Edit.ScratchVet (scratchVet)
import Sabela.AI.Capabilities.ModuleCard (storeModuleNames)
import Sabela.AI.Capabilities.ModuleSearch (interesting, resolveNameToModules)
import Sabela.AI.Capabilities.Util (featureEnabled, featureOptIn)
import Sabela.AI.Capability (Capability (..))
import Sabela.AI.CellEco (FitCand (..), cellEco, rankFits)
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.HoleRepair (goalSpans, substituteNameAt)
import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQuery, hoogleResolveTopK)
import Sabela.AI.ImportRepair (
    addQualifiedImport,
    addScopedImport,
    importedAliasMisses,
    renameModule,
    unboundAliasUses,
 )
import Sabela.AI.ModuleResolve (closestModules, isOutOfScopePackage)
import Sabela.AI.PathRepair (pathNearMissFix)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (ambiguousOccurrence, couldNotFindModule, misnamedModule)
import Sabela.Diagnose.Packages (table)
import Sabela.Model (CellError (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)
import Sabela.State.Environment (envWorkDir)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

hoogleCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
hoogleCandidates app store res src = do
    enabled <- featureEnabled "SABELA_HOOGLE_RESOLVE"
    if not enabled
        then pure []
        else do
            k <- topKFromEnv
            concat <$> mapM (candidatesFor k) (notInScopeNames src res)
  where
    candidatesFor k name = do
        resolved <- hoogleResolveTopK k name (goalOfName res name)
        vetted <-
            filterM
                ( \(pkg, modul) ->
                    scratchVet app store src [pkg] modul name (goalOfName res name)
                )
                resolved
        pure
            [ src'
            | (pkg, modul) <- vetted
            , let src' = addScopedImport modul name (addBuildDepend pkg src)
            , src' /= src
            ]

typeDiscoverCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
typeDiscoverCandidates app store res src = do
    enabled <- featureOptIn "SABELA_TYPE_RESOLVE"
    if not enabled
        then pure []
        else concat <$> mapM candidatesAt (goalSpans res)
  where
    candidatesAt (_, _, Nothing) = pure []
    candidatesAt (wrong, goal, Just sp) = do
        k <- topKFromEnv
        hits <- hoogleQuery k goal
        let cands =
                [ FitCand (hhName h) (hhType h) (hhModule h) (hhPackage h)
                | h <- hits
                , not (isOutOfScopePackage (hhPackage h))
                ]
        vetted <-
            filterM
                ( \c ->
                    scratchVet
                        app
                        store
                        src
                        [fcPackage c]
                        (fcModule c)
                        (fcName c)
                        (Just goal)
                )
                (rankFits goal (cellEco src) cands)
        pure
            [ src'
            | c <- vetted
            , Just renamed <- [substituteNameAt sp wrong (fcName c) src]
            , let src' =
                    addScopedImport
                        (fcModule c)
                        (fcName c)
                        (addBuildDepend (fcPackage c) renamed)
            , src' /= src
            ]

moduleResolveCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
moduleResolveCandidates app res src = do
    enabled <- featureEnabled "SABELA_MODULE_RESOLVE"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend, noHintModule) of
        (True, Just backend, Just wrong) -> do
            installed <- ST.sbQueryComplete backend "import "
            store <- storeModuleNames
            k <- topKFromEnv
            let pool = nub (map fst table ++ filter interesting (installed ++ store))
            pure
                [ src'
                | cand <- closestModules k moduleFuzzyThreshold wrong pool
                , let src' = renameModule wrong cand src
                , src' /= src
                ]
        _ -> pure []
  where
    errText = resultErrorText res
    noHintModule = case couldNotFindModule errText of
        Just m | isNothing (misnamedModule errText) -> Just m
        _ -> Nothing

moduleFuzzyThreshold :: Double
moduleFuzzyThreshold = 0.2

importResolveCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
importResolveCandidates app store res src = do
    enabled <- featureEnabled "SABELA_IMPORT_RESOLVE"
    if not enabled
        then pure []
        else concat <$> mapM candidatesFor (notInScopeNames src res)
  where
    candidatesFor name = do
        caps <- resolveNameToModules app name
        vetted <-
            filterM
                ( \cap ->
                    scratchVet app store src [] (capModule cap) name (goalOfName res name)
                )
                caps
        pure
            [ src'
            | cap <- vetted
            , let src' = addScopedImport (capModule cap) name src
            , src' /= src
            ]

qualifiedImportCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
qualifiedImportCandidates app store res src = do
    enabled <- featureEnabled "SABELA_IMPORT_RESOLVE"
    if not enabled
        then pure []
        else concat <$> mapM candidatesFor (aliasRepairs (resultErrorText res))
  where
    aliasRepairs err = unboundAliasUses err <> importedAliasMisses err
    candidatesFor (alias, name) = do
        caps <- resolveNameToModules app name
        vetted <-
            filterM
                ( \cap ->
                    scratchVet app store src [] (capModule cap) name (goalOfName res name)
                )
                caps
        pure
            [ src'
            | cap <- vetted
            , let src' = addQualifiedImport (capModule cap) alias src
            , src' /= src
            ]

ambiguousResolveCandidates :: Either Text ExecutionResult -> Text -> IO [Text]
ambiguousResolveCandidates res src = do
    enabled <- featureEnabled "SABELA_AMBIGUOUS_RESOLVE"
    pure (if enabled then ambiguousCandidates res src else [])

ambiguousCandidates :: Either Text ExecutionResult -> Text -> [Text]
ambiguousCandidates res src = case ambiguousOccurrence (resultErrorText res) of
    Nothing -> []
    Just (name, cands) ->
        nub
            [ src'
            | sp <- ambiguousSpans res
            , qual <- cands
            , Just src' <- [substituteNameAt sp name qual src]
            , src' /= src
            ]

ambiguousSpans :: Either Text ExecutionResult -> [(Int, Int)]
ambiguousSpans (Left _) = []
ambiguousSpans (Right er) =
    [ (l, c)
    | ce <- erErrors er
    , "Ambiguous occurrence" `T.isInfixOf` ceMessage ce
    , Just l <- [ceLine ce]
    , Just c <- [ceCol ce]
    ]

pathNearMissCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
pathNearMissCandidates app res src = do
    enabled <- featureEnabled "SABELA_PATH_RESOLVE"
    if not enabled
        then pure []
        else maybeToList <$> pathNearMissFix (envWorkDir (appEnv app)) res src

defaultTopK :: Int
defaultTopK = 3

topKFromEnv :: IO Int
topKFromEnv = do
    mk <- lookupEnv "SABELA_HOOGLE_TOP_K"
    pure (maybe defaultTopK (max 1) (mk >>= readMaybe))
