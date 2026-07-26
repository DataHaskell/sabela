{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ModuleResolve (
    closestModules,
    isNoiseModule,
    isOutOfScopePackage,
) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Similarity (trigramSimilarity)

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
    , "ghc-prim"
    , "ghc-bignum"
    , "rts"
    ]
