{-# LANGUAGE OverloadedStrings #-}

{- | The compact card a browse-route answer distils to. GHC's did-you-mean and
hidden-package diagnostics are complete answers, but on the wire they arrive as
JSON blobs with the load-bearing line repeated many times — the card decodes
them (via "Sabela.Errors.Json") and ships only the distilled fields.
-}
module Sabela.AI.Capabilities.BrowseCard (
    browseCard,
    packageOfUnit,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isDigit)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.Errors.Json (parseJsonInteractive)
import Sabela.Model (CellError (..))

{- | Distil a @:browse@ response: a clean listing becomes a capped structured
card; a GHC diagnostic becomes its one actionable fact (hidden package →
cabal line; not-found → deduped suggestions + cabal line); anything else is
@status:error@ with ONE deduped line so the caller can fall back.
-}
browseCard :: Text -> Text -> Value
browseCard modName raw
    | null msgLines = listingCard modName raw
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

{- | A clean listing: exports ranked public-API-first, capped SMALL, overflow
disclosed (shown implicitly, @more@ + @total@ explicitly) — never a wall and
never a silent cap (R3.4). Every line renders through the ONE R3.10 seam
('sanitizeTypeText'): no version-qualified or compiler-internal token ships.
-}
listingCard :: Text -> Text -> Value
listingCard modName raw =
    object $
        [ "module" .= modName
        , "status" .= ("ok" :: Text)
        , "exports" .= take cap (rankExports ls)
        , "total" .= length ls
        ]
            <> ["more" .= (length ls - cap) | length ls > cap]
  where
    cap = 24
    ls =
        filter (not . T.null) (map (T.strip . sanitizeTypeText) (T.lines raw))

{- | Public-API-first order: value signatures, then type/class declarations,
underscore-prefixed internals last; stable within each band.
-}
rankExports :: [Text] -> [Text]
rankExports ls = map snd (sortOn key (zip [0 :: Int ..] ls))
  where
    key (i, l) = (band l, i)
    band l
        | "_" `T.isPrefixOf` l = 2 :: Int
        | " :: " `T.isInfixOf` l = 0
        | otherwise = 1

{- | The suggested (module, package) of a did-you-mean line like
@DataFrame (needs flag -package-id dataframe-0.7.0.0)@.
-}
suggestionOf :: Text -> Maybe (Text, Maybe Text)
suggestionOf l = case T.words (T.strip l) of
    (m : rest)
        | "(needs" `elem` rest
        , (u : _) <- reverse rest ->
            Just (m, Just (packageOfUnit (T.dropWhileEnd (== ')') u)))
    _ -> Nothing

-- | The hidden package a diagnostic names, if any.
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

-- | The package name plus its paste-able dep line, as card fields.
cabalPairs :: Text -> [Pair]
cabalPairs pkg =
    ["package" .= pkg, "cabal" .= ("-- cabal: build-depends: " <> pkg)]

-- | A versioned unit id (@http-client-0.7.19@) reduced to its package name.
packageOfUnit :: Text -> Text
packageOfUnit u
    | null kept = u
    | otherwise = T.intercalate "-" kept
  where
    parts = T.splitOn "-" u
    kept = reverse (dropWhile isVer (reverse parts))
    isVer p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p
