{-# LANGUAGE OverloadedStrings #-}

{- | Ranked held-fact selection for the nudge\/wrap-up (8.1, R3.9\/R5.6): the
ledger keeps history; the nudge carries only call-ready and deliverable-
relevant facts, keyed on evidence shape — never a package block-list.
-}
module Siza.Agent.Discover.FactSelect (
    FactContext,
    factContext,
    factsByteBudget,
    selectFacts,
) where

import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (usedNames)
import Siza.Agent.Discover.Advice (maxHeldFacts)
import Siza.Agent.Discover.Ledger (installFactKey)
import Siza.Agent.Loop.Support (qualifiedBaseNames)

-- | Everything the relevance key may consult, precomputed once per emission.
data FactContext = FactContext
    { fcRefs :: Set Text
    -- ^ Lowercased name tokens of the goal, cells and miss-clusters.
    , fcBlob :: Text
    -- ^ The same sources as one lowercased text, for package-name mentions.
    }

-- | Serialised byte budget of the whole facts block (R3.9).
factsByteBudget :: Int
factsByteBudget = 800

-- | Build the relevance key from goal text, cell sources and miss-clusters.
factContext :: Text -> [Text] -> [Text] -> FactContext
factContext goal cells clusters =
    FactContext
        { fcRefs =
            Set.fromList
                ( map T.toLower $
                    concatMap tokens (goal : clusters)
                        ++ concatMap usedNames cells
                        ++ concatMap qualifiedBaseNames cells
                )
        , fcBlob = T.toLower (T.unwords (goal : clusters ++ cells))
        }
  where
    tokens = concatMap (T.splitOn ".") . usedNames

{- | The ranked, bounded selection: call-ready signature facts first (the
nudge's own trigger class), then relevant install facts, then other held
notes — never more than 'maxHeldFacts' or 'factsByteBudget' bytes.
-}
selectFacts :: FactContext -> [Text] -> [Text]
selectFacts ctx facts =
    byteBound factsByteBudget . take maxHeldFacts $
        [f | f <- facts, isSigFact f, signatureRelevant ctx f]
            ++ [f | f <- facts, isSigFact f, not (signatureRelevant ctx f)]
            ++ [f | f <- facts, isInstallFact f, installRelevant ctx f]
            ++ [f | f <- facts, not (isSigFact f), not (isInstallFact f)]

-- | A call-ready signature fact, as 'Siza.Agent.Discover.Advice' shapes them.
isSigFact :: Text -> Bool
isSigFact f = "`" `T.isPrefixOf` f && " :: " `T.isInfixOf` f

-- | A compiler-resolved signature named by the deliverable or active cluster.
signatureRelevant :: FactContext -> Text -> Bool
signatureRelevant ctx f =
    T.toLower (T.takeWhile (/= '`') (T.drop 1 f)) `Set.member` fcRefs ctx

isInstallFact :: Text -> Bool
isInstallFact f = isJust (installFactKey f)

{- | An install fact enters the nudge iff it is actionable in-session (an
installed\/hidden\/notebook\/builtin surface whose provided name or package
the episode references) or its package is named by the deliverable\/cluster.
-}
installRelevant :: FactContext -> Text -> Bool
installRelevant ctx f
    | actionableState (factState f) = providesRef || pkgRef
    | otherwise = pkgRef
  where
    providesRef = case providedName f of
        Just n -> T.toLower n `Set.member` fcRefs ctx
        Nothing -> False
    pkgRef = case installFactKey f of
        Just pkg ->
            T.length pkg >= 3 && T.toLower pkg `T.isInfixOf` fcBlob ctx
        Nothing -> False

-- | Install states the session can act on without leaving the notebook.
actionableState :: Text -> Bool
actionableState s = s `elem` ["installed", "hidden", "notebook", "builtin"]

-- | The @(state)@ atom of an install fact's second word.
factState :: Text -> Text
factState f = case T.words f of
    (_ : st : _) -> T.dropAround (`elem` ("():" :: String)) st
    _ -> ""

-- | The name a fact's @— provides `name`@ tag carries, when present.
providedName :: Text -> Maybe Text
providedName f = case T.splitOn "provides `" f of
    (_ : rest : _) ->
        let n = T.takeWhile (/= '`') rest
         in if T.null n then Nothing else Just n
    _ -> Nothing

-- | Keep leading facts within the serialized byte budget.
byteBound :: Int -> [Text] -> [Text]
byteBound _ [] = []
byteBound budget (f : fs)
    | cost > budget = []
    | otherwise = f : byteBound (budget - cost) fs
  where
    cost = T.length f + 1
