{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Facts (
    factPackages,
    foldFacts,
    installFactKey,
    maxHeldFacts,
    compilerFact,
    compilerFactMark,
    isCompilerFact,
) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (InstallState, installText)

maxHeldFacts :: Int
maxHeldFacts = 8

compilerFactMark :: Text
compilerFactMark = " — confirmed by the compiler (check_type)"

compilerFact :: Text -> Text -> Text
compilerFact name sig = "`" <> name <> "` :: " <> sig <> compilerFactMark

isCompilerFact :: Text -> Bool
isCompilerFact = T.isInfixOf compilerFactMark

foldFacts :: [Text] -> [Text] -> [Text]
foldFacts new facts = take maxHeldFacts (foldl addFact facts new)
  where
    addFact acc f
        | f `elem` acc || T.null f = acc
        | Just p <- installFactKey f =
            [g | g <- acc, installFactKey g /= Just p] ++ [f]
        | otherwise = acc ++ [f]

installFactKey :: Text -> Maybe Text
installFactKey f = case T.words f of
    (p : st : _)
        | "(" `T.isPrefixOf` st
        , T.dropAround (`elem` ("():" :: String)) st `elem` states ->
            Just p
    _ -> Nothing
  where
    states = map installText [minBound .. maxBound :: InstallState]

factPackages :: [Text] -> [Text]
factPackages facts =
    nubKeep
        ( mapMaybe installFactKey facts
            <> mapMaybe provenancePackage facts
        )
  where
    provenancePackage f = case T.breakOn " — found in " f of
        (_, rest)
            | T.null rest -> Nothing
            | otherwise -> case T.breakOn "(" rest of
                (_, pkgPart)
                    | T.null pkgPart -> Nothing
                    | otherwise ->
                        Just (T.takeWhile (/= ')') (T.drop 1 pkgPart))
    nubKeep = foldr (\x acc -> x : filter (/= x) acc) []
