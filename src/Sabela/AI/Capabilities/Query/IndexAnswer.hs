{-# LANGUAGE OverloadedStrings #-}

{- | check_type's world half: what the local index knows about a name, kept
apart from what the notebook shows. A package fact never becomes a session
claim — the session clause is composed beside it, from its own source.
-}
module Sabela.AI.Capabilities.Query.IndexAnswer (
    IndexHit (..),
    IndexAnswer (..),
    PackageState (..),
    IndexRetriever,
    answerModule,
    builtinAnswer,
    classifyIndexHit,
    indexAnswerPairs,
    indexLookup,
    indexLookupWith,
    renderIndexAnswer,
    looksNotInScope,
    availablePackages,
    consultedIndex,
    consultedSession,
    consultedNotebook,
    consultedBuiltin,
) where

import Control.Applicative ((<|>))
import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ScriptHs.Parser (CabalMeta (..))

import Sabela.AI.Capabilities.Query.SessionFacts (
    NotebookFacts (..),
    notebookFactPairs,
    renderNotebookFacts,
 )
import Sabela.AI.HoogleClient (statesDeclaration)
import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQuery, rankResolveTopK)
import Sabela.AI.PromptCore (builtinModules, builtinNames, drawingBuiltins)
import Sabela.AI.QualifiedName (QualifiedName (..))
import Sabela.Deps (collectMetadata)
import Sabela.Model (Notebook)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)

consultedIndex, consultedSession, consultedNotebook, consultedBuiltin :: Text
consultedIndex = "local index"
consultedSession = "live session"
consultedNotebook = "notebook source"
consultedBuiltin = "builtin vocabulary"

data IndexHit = IndexHit
    { ihName :: !Text
    , ihModule :: !Text
    , ihPackage :: !Text
    , ihType :: !Text
    }
    deriving (Eq, Show)

-- | Whether the notebook already declares the package a hit comes from.
data PackageState = Declared | Undeclared
    deriving (Eq, Show)

{- | What the world holds. @WorldBuiltin@ carries the module the environment
defines a builtin in, if any; @WorldMiss@ carries the sources actually
consulted, so the answer can name them without a constant.
-}
data IndexAnswer
    = WorldHit !IndexHit !PackageState
    | WorldBuiltin !Text !(Maybe Text)
    | WorldMiss ![Text]
    deriving (Eq, Show)

{- | The sources an answer was built from. Every world answer is composed with
the notebook's own facts, so no trace may claim less than it read.
-}
answerTrace :: IndexAnswer -> [Text]
answerTrace (WorldHit _ _) = [consultedIndex, consultedNotebook]
answerTrace (WorldBuiltin _ _) = [consultedBuiltin, consultedNotebook]
answerTrace (WorldMiss srcs) = srcs

classifyIndexHit :: Set Text -> [Text] -> Maybe IndexHit -> IndexAnswer
classifyIndexHit _ consulted Nothing = WorldMiss consulted
classifyIndexHit available _ (Just h)
    | ihPackage h `Set.member` available = WorldHit h Declared
    | otherwise = WorldHit h Undeclared

{- | The world clause: where the name lives, and whether the notebook has it.
It states nothing about a session — what the notebook shows is composed beside
it, from the notebook.
-}
renderIndexAnswer :: IndexAnswer -> Text
renderIndexAnswer (WorldBuiltin n (Just m)) =
    n <> " is part of Sabela's built-in vocabulary, defined in " <> m <> "."
renderIndexAnswer (WorldBuiltin n Nothing) =
    n
        <> " is part of Sabela's built-in vocabulary, provided by the notebook \
           \environment itself; no import declares it."
renderIndexAnswer (WorldHit h Declared) =
    describe h <> "\nPackage " <> ihPackage h <> " is declared by this notebook."
renderIndexAnswer (WorldHit h Undeclared) =
    describe h
        <> "\nPackage "
        <> ihPackage h
        <> " is not declared by this notebook. Add this as a cell's FIRST \
           \line, then import the module:\n-- cabal: build-depends: "
        <> ihPackage h
renderIndexAnswer (WorldMiss srcs) =
    "not found. Consulted: " <> T.intercalate ", " srcs <> "."

{- | The whole index answer: its payload fields, each backed by a computed
value, and its prose — world clause, notebook clause, outstanding action.
-}
indexAnswerPairs ::
    QualifiedName -> IndexAnswer -> NotebookFacts -> ([Pair], Text)
indexAnswerPairs qn answer facts =
    ( ["consulted" .= answerTrace answer]
        <> resolvedPairs qn
        <> worldPairs answer
        <> notebookFactPairs facts
    , composeAnswer (qnBare qn) (qnModule qn) answer facts
    )

-- | What the qualifier resolved to, emitted only when one was resolved.
resolvedPairs :: QualifiedName -> [Pair]
resolvedPairs qn
    | Nothing <- qnModule qn, T.null (qnNote qn) = []
    | otherwise =
        [ "resolved"
            .= object
                ( ["name" .= qnBare qn]
                    <> ["module" .= m | Just m <- [qnModule qn]]
                    <> ["via" .= qnNote qn | not (T.null (qnNote qn))]
                )
        ]

{- | The module the answer speaks about: the one the hit was found in, else the
one the qualifier named. The notebook facts must be computed for exactly this
module, or the session clause would describe a module nobody looked for.
-}
answerModule :: IndexAnswer -> Maybe Text -> Maybe Text
answerModule (WorldHit h _) _ = Just (ihModule h)
answerModule (WorldBuiltin _ m) asked = m <|> asked
answerModule (WorldMiss _) asked = asked

{- | The module an answer may tell the notebook to import: one the index or the
builtin vocabulary actually placed the name in, never one merely asked about.
-}
importable :: IndexAnswer -> Maybe Text
importable (WorldHit h _) = Just (ihModule h)
importable (WorldBuiltin _ m) = m
importable (WorldMiss _) = Nothing

composeAnswer :: Text -> Maybe Text -> IndexAnswer -> NotebookFacts -> Text
composeAnswer name mModule answer facts =
    T.intercalate "\n" (filter (not . T.null) clauses)
  where
    clauses = [renderIndexAnswer answer, sessionClause, actionClause]
    sessionClause = renderNotebookFacts name (answerModule answer mModule) facts
    actionClause = case (importable answer, nfImports facts) of
        (Just m, []) -> "Add this import:\nimport " <> m
        _ -> ""

worldPairs :: IndexAnswer -> [Pair]
worldPairs (WorldMiss srcs) =
    ["world" .= object ["found" .= False, "consulted" .= srcs]]
worldPairs (WorldBuiltin _ m) =
    [ "world"
        .= object
            ( ["found" .= True, "builtin" .= True]
                <> ["module" .= mm | Just mm <- [m]]
            )
    ]
worldPairs (WorldHit h st) =
    [ "world"
        .= object
            ( [ "found" .= True
              , "module" .= ihModule h
              , "package" .= ihPackage h
              , "packageDeclared" .= (st == Declared)
              ]
                <> typeFact (ihType h)
            )
    ]

{- | How a world fact states an entity's type: a declaration states its own
shape, everything else a signature. The field is named for what it carries, so
nothing announces a signature no source gave.
-}
typeFact :: Text -> [Pair]
typeFact t
    | T.null t = []
    | statesDeclaration t = ["declaration" .= t]
    | otherwise = ["signature" .= t]

describe :: IndexHit -> Text
describe h =
    ihName h
        <> " is defined in "
        <> ihModule h
        <> " (package "
        <> ihPackage h
        <> ")"
        <> typeLine
  where
    typeLine
        | T.null (ihType h) = ""
        | statesDeclaration (ihType h) = "\n  " <> ihType h
        | otherwise = "\n  " <> ihName h <> " :: " <> ihType h

{- | The vocabulary the environment carries, as a world fact: a drawing name
lives in a module the notebook can import, the rest are injected and have none.
What the notebook does with either is the notebook's clause to state.
-}
builtinAnswer :: Text -> Maybe IndexAnswer
builtinAnswer expr
    | expr `elem` drawingBuiltins =
        Just (WorldBuiltin expr (listToMaybe builtinModules))
    | expr `elem` builtinNames = Just (WorldBuiltin expr Nothing)
    | otherwise = Nothing

looksNotInScope :: Text -> Bool
looksNotInScope t =
    let lt = T.toLower t
     in T.null (T.strip t)
            || "not in scope" `T.isInfixOf` lt
            || "no top-level binding" `T.isInfixOf` lt
            || "variable not in scope" `T.isInfixOf` lt

-- | How the index is asked. Named so a test can replay recorded hits.
type IndexRetriever = Int -> Text -> IO [HoogleHit]

indexLookup :: Maybe Text -> Text -> IO (Maybe IndexHit)
indexLookup = indexLookupWith hoogleQuery

{- | Look a bare name up in the index. A known module narrows the candidates
before ranking; when nothing in that module carries the name the search widens
rather than reporting a miss.
-}
indexLookupWith :: IndexRetriever -> Maybe Text -> Text -> IO (Maybe IndexHit)
indexLookupWith retrieve mModule name
    | T.null name = pure Nothing
    | otherwise = do
        hits <- retrieve indexHitBudget name
        pure (best (narrowed hits) `orElse` best hits)
  where
    narrowed hits = case mModule of
        Nothing -> []
        Just m -> [h | h <- hits, inModule m (hhModule h)]
    inModule m modu = m == modu || (m <> ".") `T.isPrefixOf` modu
    best hits = case rankResolveTopK 1 name Nothing hits of
        ((pkg, modu) : _) -> Just (IndexHit name modu pkg (typeOfHit name hits))
        [] -> Nothing
    orElse (Just a) _ = Just a
    orElse Nothing b = b

indexHitBudget :: Int
indexHitBudget = 20

typeOfHit :: Text -> [HoogleHit] -> Text
typeOfHit name hits = case [hhType h | h <- hits, hhName h == name] of
    (t : _) -> t
    [] -> ""

availablePackages :: App -> IO (Set Text)
availablePackages app = do
    nb <- readNotebook (appNotebook app)
    pure (declaredPackages nb (envGlobalDeps (appEnv app)))

declaredPackages :: Notebook -> Set Text -> Set Text
declaredPackages nb = Set.union (Set.fromList (metaDeps (collectMetadata nb)))
