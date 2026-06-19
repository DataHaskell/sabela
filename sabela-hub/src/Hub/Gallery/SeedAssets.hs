{-# LANGUAGE OverloadedStrings #-}

{- | Repoint a notebook's @\/api\/asset@ output references (a live-editor
endpoint the static gallery cannot serve) at public committed assets, so the
inlined dashboard outputs render. Model viewers are remapped positionally to a
caller-supplied list; other assets keep their name under a new base. Port of the
@rewrite_assets@ helper from the former @seed-gallery.py@ (CSG-specific).
-}
module Hub.Gallery.SeedAssets (
    rewriteAssets,
) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T

marker :: Text
marker = "/api/asset?path="

-- | Apply the positional model remap, then the generic waterfall-base swap.
rewriteAssets :: Text -> [Text] -> Text -> Text
rewriteAssets base models = replaceWaterfall base . replaceModels base models

{- | Replace each @\/api\/asset?path=…models\/\<digits\>.glb@ (in document
order) with @\<base\>models\/\<name\>.glb@, drawing names positionally from the
list. Matches beyond the list length are left unchanged.
-}
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

{- | If @afterMarker@ begins a no-delimiter run ending in
@models\/\<digits\>.glb@, return @(matched length, digits)@. The run may not
contain whitespace, quotes, or angle brackets (the regex @[^\\s"'\<\>]@).
-}
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

-- | @\/api\/asset?path=[examples\/data\/]waterfall\/@ → the new base URL.
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
