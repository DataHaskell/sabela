{-# LANGUAGE OverloadedStrings #-}

module Hub.Gallery.SeedAssets (
    rewriteAssets,
) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T

marker :: Text
marker = "/api/asset?path="

rewriteAssets :: Text -> [Text] -> Text -> Text
rewriteAssets base models = replaceWaterfall base . replaceModels base models

replaceModels :: Text -> [Text] -> Text -> Text
replaceModels base models = go (0 :: Int)
  where
    go i s =
        let (before, rest) = T.breakOn marker s
         in if T.null rest
                then s
                else
                    let afterMarker = T.drop (T.length marker) rest
                     in case matchModel afterMarker of
                            Just (matchLen, _)
                                | Just name <- models `at` i ->
                                    before
                                        <> base
                                        <> "models/"
                                        <> name
                                        <> ".glb"
                                        <> go (i + 1) (T.drop (T.length marker + matchLen) rest)
                                | otherwise ->
                                    before
                                        <> marker
                                        <> T.take matchLen afterMarker
                                        <> go (i + 1) (T.drop (T.length marker + matchLen) rest)
                            Nothing ->
                                before <> marker <> go i afterMarker

matchModel :: Text -> Maybe (Int, Text)
matchModel afterMarker
    | T.null fromModels = Nothing
    | T.any isDelim before = Nothing
    | not (T.null digits) && ".glb" `T.isPrefixOf` afterDigits =
        Just (T.length before + T.length "models/" + T.length digits + 4, digits)
    | otherwise = Nothing
  where
    (before, fromModels) = T.breakOn "models/" afterMarker
    afterSlash = T.drop (T.length "models/") fromModels
    (digits, afterDigits) = T.span isDigit afterSlash

isDelim :: Char -> Bool
isDelim c = c `elem` (" \t\n\r\"'<>" :: String)

replaceWaterfall :: Text -> Text -> Text
replaceWaterfall base =
    T.replace (marker <> "waterfall/") base
        . T.replace (marker <> "examples/data/waterfall/") base

at :: [a] -> Int -> Maybe a
at xs i
    | i < 0 = Nothing
    | otherwise = case drop i xs of
        (x : _) -> Just x
        [] -> Nothing
