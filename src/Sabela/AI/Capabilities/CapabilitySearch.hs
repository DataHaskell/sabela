{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.CapabilitySearch (
    execSearchCapability,
    capabilityOutcome,
    enrichedOutcome,
    exactOnlyHits,
    packageBuckets,
    semanticWanted,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.CapabilityApi (
    ApiFn (..),
    PackageApi (..),
    enrichPackages,
    hoogleFor,
    usageExample,
 )
import Sabela.AI.Capabilities.CapabilityHelper (
    HelperHit (..),
    runCapabilityHelper,
 )
import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQueryInScope)
import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.Deps (availablePackages)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)

maxHits :: Int
maxHits = 18

enrichPkgs :: Int
enrichPkgs = 5

perPkgApi :: Int
perPkgApi = 6

examplePkgs :: Int
examplePkgs = 3

maxDocsChars :: Int
maxDocsChars = 160

execSearchCapability :: App -> Value -> IO ToolOutcome
execSearchCapability app input
    | T.null (T.strip q) = pure (enrichedOutcome q [])
    | otherwise = do
        helperHits <-
            if semanticWanted input && not exact
                then runCapabilityHelper maxHits q
                else pure []
        nb <- readNotebook (appNotebook app)
        let inScope = availablePackages nb (envGlobalDeps (appEnv app))
        pkgs <-
            if null helperHits
                then packageBuckets . exactFilter <$> searched inScope
                else pure [(hePackage h, heSynopsis h) | h <- helperHits]
        enriched <- enrichPackages enrichPkgs perPkgApi q pkgs
        pure (enrichedOutcome q enriched)
  where
    q = fieldText "query" input
    exact = boolField "exact" input == Just True
    exactFilter = if exact then exactOnlyHits q else id
    {- A scope makes the request precise, so it is asked of Hoogle directly.
    Ranked against all of Hackage the scoped package loses to whatever else
    answers to the same word, and the caller's filter is left nothing to keep. -}
    searched inScope = case scopeTerm of
        Just s -> hoogleFor s q
        Nothing -> hoogleQueryInScope inScope maxHits q
    scopeTerm =
        listToMaybe
            [ s
            | k <- ["package", "module"]
            , let s = T.strip (fieldText k input)
            , not (T.null s)
            ]

semanticWanted :: Value -> Bool
semanticWanted input = boolField "semantic" input /= Just False

boolField :: Text -> Value -> Maybe Bool
boolField k v = case v of
    Object o | Just (Bool b) <- KM.lookup (Key.fromText k) o -> Just b
    _ -> Nothing

exactOnlyHits :: Text -> [HoogleHit] -> [HoogleHit]
exactOnlyHits q = filter match
  where
    lq = T.toLower q
    match h =
        hhName h == q
            || hhModule h == q
            || T.toLower (hhPackage h) == lq

packageBuckets :: [HoogleHit] -> [(Text, Text)]
packageBuckets hits =
    [ (p, synOf p)
    | p <- nub (map hhPackage hits)
    , not (T.null p)
    ]
  where
    synOf p = case filter (\h -> hhPackage h == p) hits of
        (h : _) -> hhDocs h
        [] -> ""

enrichedOutcome :: Text -> [PackageApi] -> ToolOutcome
enrichedOutcome q hits =
    okOutcome $
        object (["query" .= q, "hits" .= zipWith hitJSON [0 ..] hits] <> nameFallback)
  where
    nameFallback
        | not (null hits) = []
        | T.null qs || T.any (== ' ') qs = []
        | otherwise = ["tryCabal" .= ("-- cabal: build-depends: " <> qs)]
    qs = T.strip q
    hitJSON i h =
        object $
            [ "package" .= paPackage h
            , "synopsis" .= truncDocs (paSynopsis h)
            , "cabal" .= ("-- cabal: build-depends: " <> paPackage h)
            , "modules" .= nub (filter (not . T.null) (map afModule api))
            , "api" .= map apiJSON api
            ]
                ++ ["version" .= paVersion h | not (T.null (paVersion h))]
                ++ ["example" .= ex | i < examplePkgs, ex <- [usageExample api], not (T.null ex)]
      where
        api = paApi h
    apiJSON a =
        object
            [ "name" .= afName a
            , "module" .= afModule a
            , "type" .= afType a
            ]
    truncDocs d
        | T.length d <= maxDocsChars = d
        | otherwise = T.take maxDocsChars d <> "…"

capabilityOutcome :: Text -> [HoogleHit] -> ToolOutcome
capabilityOutcome q hits =
    okOutcome $ object ["query" .= q, "hits" .= map hitJSON hits]
  where
    hitJSON h =
        object
            [ "name" .= hhName h
            , "type" .= hhType h
            , "module" .= hhModule h
            , "package" .= hhPackage h
            , "docs" .= truncDocs (hhDocs h)
            ]
    truncDocs d
        | T.length d <= maxDocsChars = d
        | otherwise = T.take maxDocsChars d <> "…"
