{-# LANGUAGE OverloadedStrings #-}

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
import Siza.Agent.Discover.Facts (isCompilerFact)
import Siza.Agent.Discover.Ledger (installFactKey)
import Siza.Agent.Loop.Support (qualifiedBaseNames)

data FactContext = FactContext
    { fcRefs :: Set Text
    , fcBlob :: Text
    }

factsByteBudget :: Int
factsByteBudget = 800

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

selectFacts :: FactContext -> [Text] -> [Text]
selectFacts ctx facts =
    byteBound factsByteBudget . take maxHeldFacts $
        [f | f <- facts, isCompilerFact f]
            ++ [f | f <- facts, notCompiler f, isSigFact f, signatureRelevant ctx f]
            ++ [f | f <- facts, notCompiler f, isSigFact f, not (signatureRelevant ctx f)]
            ++ [f | f <- facts, notCompiler f, isInstallFact f, installRelevant ctx f]
            ++ [ f
               | f <- facts
               , notCompiler f
               , not (isSigFact f)
               , not (isInstallFact f)
               ]
  where
    notCompiler = not . isCompilerFact

isSigFact :: Text -> Bool
isSigFact f = "`" `T.isPrefixOf` f && " :: " `T.isInfixOf` f

signatureRelevant :: FactContext -> Text -> Bool
signatureRelevant ctx f =
    T.toLower (T.takeWhile (/= '`') (T.drop 1 f)) `Set.member` fcRefs ctx

isInstallFact :: Text -> Bool
isInstallFact f = isJust (installFactKey f)

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

actionableState :: Text -> Bool
actionableState s = s `elem` ["installed", "hidden", "notebook", "builtin"]

factState :: Text -> Text
factState f = case T.words f of
    (_ : st : _) -> T.dropAround (`elem` ("():" :: String)) st
    _ -> ""

providedName :: Text -> Maybe Text
providedName f = case T.splitOn "provides `" f of
    (_ : rest : _) ->
        let n = T.takeWhile (/= '`') rest
         in if T.null n then Nothing else Just n
    _ -> Nothing

byteBound :: Int -> [Text] -> [Text]
byteBound _ [] = []
byteBound budget (f : fs)
    | cost > budget = []
    | otherwise = f : byteBound (budget - cost) fs
  where
    cost = T.length f + 1
