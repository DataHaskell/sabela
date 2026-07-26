{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Query.IndexAnswer (
    IndexHit (..),
    IndexAnswer (..),
    classifyIndexHit,
    fillSilence,
    renderIndexAnswer,
    looksNotInScope,
    consultedSources,
    viaSessionType,
    viaSessionInfo,
    viaLocalIndex,
    viaVocabulary,
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ScriptHs.Parser (CabalMeta (..))

import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQuery, rankResolveTopK)
import Sabela.Deps (collectMetadata)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)

viaSessionType, viaSessionInfo, viaLocalIndex :: Text
viaSessionType = "session-type"
viaSessionInfo = "session-info"
viaLocalIndex = "local-index"

viaVocabulary :: [Text]
viaVocabulary = [viaSessionType, viaSessionInfo, viaLocalIndex]

data IndexHit = IndexHit
    { ihName :: !Text
    , ihModule :: !Text
    , ihPackage :: !Text
    , ihType :: !Text
    }
    deriving (Eq, Show)

data IndexAnswer
    = NotImported !IndexHit
    | NotInstalled !IndexHit
    | UnknownName ![Text]
    deriving (Eq, Show)

consultedSources :: [Text]
consultedSources = ["live session", "local index"]

classifyIndexHit :: Set Text -> Maybe IndexHit -> IndexAnswer
classifyIndexHit _ Nothing = UnknownName consultedSources
classifyIndexHit available (Just h)
    | ihPackage h `Set.member` available = NotImported h
    | otherwise = NotInstalled h

renderIndexAnswer :: IndexAnswer -> Text
renderIndexAnswer (NotImported h) =
    describe h
        <> "\nIt is not imported in this session yet. Add this import:\n"
        <> "import "
        <> ihModule h
renderIndexAnswer (NotInstalled h) =
    describe h
        <> "\nPackage "
        <> ihPackage h
        <> " is not declared by this notebook. Add this as a cell's FIRST \
           \line, then import the module:\n-- cabal: build-depends: "
        <> ihPackage h
renderIndexAnswer (UnknownName srcs) =
    "not found. Consulted: " <> T.intercalate ", " srcs <> "."

describe :: IndexHit -> Text
describe h =
    ihName h
        <> " is defined in "
        <> ihModule h
        <> " (package "
        <> ihPackage h
        <> ")"
        <> (if T.null (ihType h) then "" else "\n  " <> ihName h <> " :: " <> ihType h)

looksNotInScope :: Text -> Bool
looksNotInScope t =
    let lt = T.toLower t
     in T.null (T.strip t)
            || "not in scope" `T.isInfixOf` lt
            || "no top-level binding" `T.isInfixOf` lt
            || "variable not in scope" `T.isInfixOf` lt

fillSilence :: App -> Text -> Text -> Text -> IO (Text, Text)
fillSilence app expr via result
    | not (looksNotInScope result) = pure (via, result)
    | otherwise = do
        mHit <- indexLookup expr
        available <- availablePackages app
        pure $ case classifyIndexHit available mHit of
            UnknownName srcs -> (via, result <> "\n" <> renderIndexAnswer (UnknownName srcs))
            answer -> (viaLocalIndex, renderIndexAnswer answer)

indexLookup :: Text -> IO (Maybe IndexHit)
indexLookup name = do
    hits <- hoogleQuery indexHitBudget name
    pure $ case rankResolveTopK 1 name Nothing hits of
        ((pkg, modu) : _) -> Just (IndexHit name modu pkg (typeOfHit name hits))
        [] -> Nothing

indexHitBudget :: Int
indexHitBudget = 20

typeOfHit :: Text -> [HoogleHit] -> Text
typeOfHit name hits = case [hhType h | h <- hits, hhName h == name] of
    (t : _) -> t
    [] -> ""

availablePackages :: App -> IO (Set Text)
availablePackages app = do
    nb <- readNotebook (appNotebook app)
    let declared = Set.fromList (metaDeps (collectMetadata nb))
    pure (Set.union declared (envGlobalDeps (appEnv app)))
