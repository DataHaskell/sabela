{-# LANGUAGE OverloadedStrings #-}

{- | G10: when the live session cannot resolve a name, the local index
answers — as a different KIND of fact, labelled as such. The rule is the
prompt's own and binds the harness too: a compile answer outranks the index;
the index fills silence, it never overrides. A scope miss is classified into
three distinct facts with three distinct texts, so a model can tell "this
does not exist" from "the module that defines it is not imported here yet" —
the distinction live_test9 could not make.
-}
module Sabela.AI.Capabilities.Query.IndexAnswer (
    IndexHit (..),
    IndexAnswer (..),
    classifyIndexHit,
    fillSilence,
    renderIndexAnswer,
    looksNotInScope,
    consultedSources,

    -- * The @via@ provenance vocabulary (G10.1)
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

{- | Which source answered, so triage can attribute every answer and the
caller can weigh it by the prompt's disagreement rule. Producers import
these; the vocabulary cannot drift from what is emitted.
-}
viaSessionType, viaSessionInfo, viaLocalIndex :: Text
viaSessionType = "session-type"
viaSessionInfo = "session-info"
viaLocalIndex = "local-index"

viaVocabulary :: [Text]
viaVocabulary = [viaSessionType, viaSessionInfo, viaLocalIndex]

-- | What the local index knows about a name: where it lives, and its type.
data IndexHit = IndexHit
    { ihName :: !Text
    , ihModule :: !Text
    , ihPackage :: !Text
    , ihType :: !Text
    }
    deriving (Eq, Show)

{- | The three distinct facts a scope miss can be. They are different answers
and never share text: presenting all three as @Not in scope@ is what made
every negative result uninterpretable in live_test9.
-}
data IndexAnswer
    = -- | The index knows it and its package is available here.
      NotImported !IndexHit
    | -- | The index knows it; its package is not declared by this notebook.
      NotInstalled !IndexHit
    | -- | Neither source has it; carries what was consulted.
      UnknownName ![Text]
    deriving (Eq, Show)

-- | The sources a miss consulted, so a miss is evidence rather than silence.
consultedSources :: [Text]
consultedSources = ["live session", "local index"]

{- | Classify an index resolution against the packages available here (the
notebook's declared deps plus the preinstalled/global set).
-}
classifyIndexHit :: Set Text -> Maybe IndexHit -> IndexAnswer
classifyIndexHit _ Nothing = UnknownName consultedSources
classifyIndexHit available (Just h)
    | ihPackage h `Set.member` available = NotImported h
    | otherwise = NotInstalled h

{- | The model-facing answer. Every non-unknown case carries the ONE line
that would make the name available live — the import, or the @-- cabal:@
first line — because the model copies rather than infers (G10.4, the same
discipline as G6's @hidden-package-text@ row).
-}
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

-- | The index's account of a name: where it lives and, when known, its type.
describe :: IndexHit -> Text
describe h =
    ihName h
        <> " is defined in "
        <> ihModule h
        <> " (package "
        <> ihPackage h
        <> ")"
        <> (if T.null (ihType h) then "" else "\n  " <> ihName h <> " :: " <> ihType h)

{- | Did the session fail to resolve the query? Matches GHC's not-in-scope
forms, INCLUDING the data-constructor phrasing a type-level query gets —
that phrasing is a miss to be classified, never an answer to serve (G10.3).
-}
looksNotInScope :: Text -> Bool
looksNotInScope t =
    let lt = T.toLower t
     in T.null (T.strip t)
            || "not in scope" `T.isInfixOf` lt
            || "no top-level binding" `T.isInfixOf` lt
            || "variable not in scope" `T.isInfixOf` lt

{- | G10: the session is authoritative; only when it does NOT resolve does
the local index answer, labelled @local-index@ so the caller can weigh it by
the prompt's own disagreement rule. A cold or absent index degrades to the
session's own answer rather than failing.
-}
fillSilence :: App -> Text -> Text -> Text -> IO (Text, Text)
fillSilence app expr via result
    | not (looksNotInScope result) = pure (via, result)
    | otherwise = do
        mHit <- indexLookup expr
        available <- availablePackages app
        pure $ case classifyIndexHit available mHit of
            UnknownName srcs -> (via, result <> "\n" <> renderIndexAnswer (UnknownName srcs))
            answer -> (viaLocalIndex, renderIndexAnswer answer)

-- | The local Hoogle index's account of a name, or Nothing when it is cold.
indexLookup :: Text -> IO (Maybe IndexHit)
indexLookup name = do
    hits <- hoogleQuery indexHitBudget name
    pure $ case rankResolveTopK 1 name Nothing hits of
        ((pkg, modu) : _) -> Just (IndexHit name modu pkg (typeOfHit name hits))
        [] -> Nothing

-- | Bounded index consult: a lookup, not a search (G10.5).
indexHitBudget :: Int
indexHitBudget = 20

-- | The exact-name hit's signature, when the index carries one.
typeOfHit :: Text -> [HoogleHit] -> Text
typeOfHit name hits = case [hhType h | h <- hits, hhName h == name] of
    (t : _) -> t
    [] -> ""

{- | Packages a name could already be imported from here: the notebook's own
@-- cabal:@ declarations plus the global\/preinstalled set.
-}
availablePackages :: App -> IO (Set Text)
availablePackages app = do
    nb <- readNotebook (appNotebook app)
    let declared = Set.fromList (metaDeps (collectMetadata nb))
    pure (Set.union declared (envGlobalDeps (appEnv app)))
