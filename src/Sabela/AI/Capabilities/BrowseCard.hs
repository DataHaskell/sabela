{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.BrowseCard (
    browseCard,
    browseCardFor,
    packageOfUnit,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isDigit)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capability (
    Capability (..),
    defaultSynonyms,
    relevanceScore,
    unqualify,
 )
import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.Errors.Json (parseJsonInteractive)
import Sabela.Model (CellError (..))

browseCard :: Text -> Text -> Value
browseCard = browseCardFor Nothing

browseCardFor :: Maybe Text -> Text -> Text -> Value
browseCardFor mQuery modName raw
    | null msgLines = listingCard mQuery modName raw
    | Just pkg <- hiddenPackage msgLines =
        object (base "hidden-package" <> cabalPairs pkg)
    | not (null suggests) =
        object
            ( base "not-found"
                <> ["suggestions" .= nub (map fst suggests)]
                <> maybe [] cabalPairs (listToMaybe (mapMaybe snd suggests))
            )
    | otherwise =
        object
            ( base "error"
                <> [ "message"
                        .= sanitizeTypeText (fromMaybe "" (listToMaybe msgLines))
                   ]
            )
  where
    base st = ["module" .= modName, "status" .= (st :: Text)]
    (errs, _, _) = parseJsonInteractive raw
    msgLines =
        nub
            [ l
            | e <- errs
            , l <- map T.strip (T.lines (ceMessage e))
            , not (T.null l)
            ]
    suggests = mapMaybe suggestionOf msgLines

listingCard :: Maybe Text -> Text -> Text -> Value
listingCard mQuery modName raw =
    object $
        [ "module" .= modName
        , "status" .= ("ok" :: Text)
        , "exports" .= take cap (rankExports mQuery ls)
        , "total" .= length ls
        ]
            <> ["more" .= (length ls - cap) | length ls > cap]
  where
    cap = 24
    ls =
        filter (not . T.null) (map (T.strip . sanitizeTypeText) (T.lines raw))

rankExports :: Maybe Text -> [Text] -> [Text]
rankExports mQuery ls = map snd (sortOn key (zip [0 :: Int ..] ls))
  where
    key (i, l) = (negate (relevance l), band l, i)
    relevance l = case mQuery of
        Just q | not (T.null (T.strip q)) -> relevanceScore defaultSynonyms q (asCap l)
        _ -> 0
    asCap l
        | isTypeLevel l = case T.words l of
            (_ : n : _) -> Capability "" (unqualify n) (T.unwords (drop 2 (T.words l)))
            _ -> Capability "" "" l
        | (n, rest) <- T.breakOn " :: " l
        , not (T.null rest) =
            Capability "" (unqualify (T.strip n)) (T.drop 4 rest)
        | otherwise = Capability "" "" l
    band l
        | "_" `T.isPrefixOf` l = 2 :: Int
        | isTypeLevel l = 1
        | " :: " `T.isInfixOf` l = 0
        | otherwise = 1

isTypeLevel :: Text -> Bool
isTypeLevel l =
    any (`T.isPrefixOf` l) declKeywords
        || any (`T.isPrefixOf` l) ["=", "|", ","]
        || T.any (`elem` ("{}" :: String)) l
        || "," `T.isSuffixOf` l
  where
    declKeywords =
        [ "type "
        , "data "
        , "newtype "
        , "class "
        , "instance "
        , "pattern "
        , "foreign "
        , "infix"
        ]

suggestionOf :: Text -> Maybe (Text, Maybe Text)
suggestionOf l = case T.words (T.strip l) of
    (m : rest)
        | "(needs" `elem` rest
        , (u : _) <- reverse rest ->
            Just (m, Just (packageOfUnit (T.dropWhileEnd (== ')') u)))
    _ -> Nothing

hiddenPackage :: [Text] -> Maybe Text
hiddenPackage ls =
    listToMaybe
        [ packageOfUnit (T.takeWhile (/= '\'') (T.drop (T.length marker) r))
        | l <- ls
        , let (_, r) = T.breakOn marker l
        , not (T.null r)
        ]
  where
    marker = "hidden package `" :: Text

cabalPairs :: Text -> [Pair]
cabalPairs pkg =
    ["package" .= pkg, "cabal" .= ("-- cabal: build-depends: " <> pkg)]

packageOfUnit :: Text -> Text
packageOfUnit u
    | null kept = u
    | otherwise = T.intercalate "-" kept
  where
    parts = T.splitOn "-" u
    kept = reverse (dropWhile isVer (reverse parts))
    isVer p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p
