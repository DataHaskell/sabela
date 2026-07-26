{-# LANGUAGE OverloadedStrings #-}

{- | The compact card a browse-route answer distils to. GHC's did-you-mean and
hidden-package diagnostics are complete answers, but on the wire they arrive as
JSON blobs with the load-bearing line repeated many times — the card decodes
them (via "Sabela.Errors.Json") and ships only the distilled fields.
-}
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

{- | Distil a @:browse@ response: a clean listing becomes a capped structured
card; a GHC diagnostic becomes its one actionable fact (hidden package →
cabal line; not-found → deduped suggestions + cabal line); anything else is
@status:error@ with ONE deduped line so the caller can fall back.
-}
browseCard :: Text -> Text -> Value
browseCard = browseCardFor Nothing

{- | 'browseCard' ranked for the query that asked: the same relevance scale
every search and the hidden-store card rank with, as the LEADING key over the
static band. The live card was the last unranked surface — after the install,
@ReadOptions@ queries hit this path and were answered operator-soup-first
(live_test41) while the store card for the same module answered ranked.
Declarations score too: @type HeaderSpec :: *@ names the thing a
constructor-hunting query is about.
-}
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

{- | A clean listing: exports ranked public-API-first, capped SMALL, overflow
disclosed (shown implicitly, @more@ + @total@ explicitly) — never a wall and
never a silent cap (R3.4). Every line renders through the ONE R3.10 seam
('sanitizeTypeText'): no version-qualified or compiler-internal token ships.
-}
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

{- | Public-API-first order: value signatures, then type/class declarations,
underscore-prefixed internals last; stable within each band.

A type-level line must be banded BEFORE the @" :: "@ test, because @:browse@
renders a declaration as @type Canvas :: *@ — which contains @" :: "@ and would
otherwise rank as a value signature, spending the whole cap on declarations and
their record continuations while the module's verbs never appear.
-}
rankExports :: Maybe Text -> [Text] -> [Text]
rankExports mQuery ls = map snd (sortOn key (zip [0 :: Int ..] ls))
  where
    key (i, l) = (negate (relevance l), band l, i)
    relevance l = case mQuery of
        Just q | not (T.null (T.strip q)) -> relevanceScore defaultSynonyms q (asCap l)
        _ -> 0
    -- A line as a scorable (name, type): a declaration's name is the word
    -- after its keyword; a value signature's is the text before " :: ",
    -- unqualified (the live browse prefixes the notebook's own alias).
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

{- | A declaration or a continuation of one: a leading declaration keyword, a
constructor/alternative continuation, or record-brace syntax (which a value
signature cannot contain).
-}
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
