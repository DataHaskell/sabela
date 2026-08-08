{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ModuleResolve (
    boundedModules,
    closestModules,
    isNoiseModule,
    isOutOfScopePackage,
    namesFragment,
) where

import Data.List (group, sort, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Similarity (trigramSimilarity)

{- | Whether a module opens one of its distinguishing components with the
fragment, so @Hod@ names @Data.HodaTime@ while @Data@ names nothing. It says
which real names a fragment could have meant, never standing in for one.
-}
namesFragment :: Text -> Text -> Bool
namesFragment fragment = any (fragment `T.isPrefixOf`) . distinguishing

{- | The components that tell a module apart from its neighbours. The leading
component of a qualified name is a namespace thousands of packages share, so a
fragment matching only that has picked out the namespace, not a module.
-}
distinguishing :: Text -> [Text]
distinguishing m = case T.splitOn "." m of
    [only] -> [only]
    (_ : rest) -> rest
    [] -> []

{- | A module list bounded for display: verbatim while it fits, else collapsed to
namespaces with counts. Every module is accounted for either way, so the bound
costs detail and never coverage.
-}
boundedModules :: Int -> [Text] -> [Text]
boundedModules cap ms
    | length ms <= cap = ms
    | otherwise = bucketModulesAt (collapseDepth ms) ms

bucketModulesAt :: Int -> [Text] -> [Text]
bucketModulesAt depth ms =
    [ if n > 1 then p <> " (" <> T.pack (show n) <> ")" else p
    | g@(p : _) <- group (sort (map prefixAt ms))
    , let n = length g
    ]
  where
    prefixAt = T.intercalate "." . take depth . T.splitOn "."

{- | One component below the namespace every module shares. A fixed depth folds a
package rooted in a single namespace — @Data.HodaTime.*@ — into one row naming
that root and nothing else, which states less than the truncation it replaces.
-}
collapseDepth :: [Text] -> Int
collapseDepth ms = max 2 (1 + length (commonComponents (map (T.splitOn ".") ms)))

commonComponents :: [[Text]] -> [Text]
commonComponents [] = []
commonComponents (c : cs) = foldr shared c cs
  where
    shared a b = map fst (takeWhile (uncurry (==)) (zip a b))

closestModules :: Int -> Double -> Text -> [Text] -> [Text]
closestModules k threshold wrong mods =
    take k
        . map fst
        . sortOn rank
        $ scored
  where
    scored =
        [ (m, s)
        | m <- mods
        , m /= wrong
        , let s = trigramSimilarity wrong m
        , s >= threshold
        ]
    rank (m, s) = (Down s, T.length m, m)

isNoiseModule :: Text -> Bool
isNoiseModule m =
    m == "Internal"
        || "Documentation." `T.isPrefixOf` m
        || any (`T.isInfixOf` m) [".Internal", ".Example", ".Demo", ".Tutorial"]

isOutOfScopePackage :: Text -> Bool
isOutOfScopePackage pkg = pkg `elem` compilerToolchainPackages

compilerToolchainPackages :: [Text]
compilerToolchainPackages =
    [ "ghc"
    , "ghc-boot"
    , "ghc-boot-th"
    , "ghci"
    , "ghc-heap"
    , "ghc-internal"
    , "ghc-lib"
    , "ghc-lib-parser"
    , "ghc-lib-parser-ex"
    , "ghc-prim"
    , "ghc-bignum"
    , "rts"
    ]
