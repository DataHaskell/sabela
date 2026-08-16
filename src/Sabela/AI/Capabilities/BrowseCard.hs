{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.BrowseCard (
    browseCard,
    browseCardFor,
    packageOfUnit,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as Set
import Sabela.AI.Capability (coalesce, declMembers)
import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.AI.LeakShape (lexicalEntity)
import Sabela.AI.RepairDispatch (DiagClass (ClassHiddenPackage), diagClassText)
import Sabela.AI.Search.Export (
    ExportRow (..),
    rankExportsBy,
 )
import Sabela.AI.Search.Need (parseNeed)
import Sabela.Diagnose (pinnedDep, splitPkgVersion)
import Sabela.Errors.Json (parseJsonInteractive)
import Sabela.Model (CellError (..))

browseCard :: Text -> Text -> Value
browseCard = browseCardFor Nothing

browseCardFor :: Maybe Text -> Text -> Text -> Value
browseCardFor mQuery modName raw
    | null msgLines = listingCard mQuery modName raw
    | Just pkg <- hiddenPackage msgLines =
        object (base (diagClassText ClassHiddenPackage) <> cabalPairs pkg)
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

{- | One @:browse@ declaration is one export, so GHCi's indented continuation
lines are coalesced back before anything counts or ranks them. A row whose head
is not a lexical entity is disclosed as @unparsed@, never emitted as a name.
-}
listingCard :: Maybe Text -> Text -> Text -> Value
listingCard mQuery modName raw =
    object $
        [ "module" .= modName
        , "status" .= ("ok" :: Text)
        , "exports" .= take cap (rankExports mQuery modName entries)
        , "total" .= length entries
        ]
            <> ["more" .= (length entries - cap) | length entries > cap]
            <> ["unparsed" .= unparsed | unparsed > 0]
  where
    cap = 24
    ls =
        concatMap
            (filter (not . T.null) . declRows . T.strip . sanitizeTypeText)
            (coalesce (T.lines raw))
    entries = filter (lexicalEntity . erName . rowOf) ls
    unparsed = length ls - length entries

{- | A declaration is an export and so is every member it declares: coalescing
without this decomposition folds a class's methods and a record's selectors
into their parent's row, where no export names them.
-}
declRows :: Text -> [Text]
declRows ent = ent : [n <> " :: " <> ty | (n, ty) <- declMembers ent]

{- | Order the raw @:browse@ lines by what the caller asked for. Every budget
downstream takes the head of this list, so the order is the answer.
-}
rankExports :: Maybe Text -> Text -> [Text] -> [Text]
rankExports mQuery modName =
    rankExportsBy (parseNeed Set.empty (fromMaybe "" mQuery)) modName rowOf

{- | A declaration keyword names its entity in the second word; anything else
is named by what stands left of its signature. The order matters: a signature
whose type mentions braces (@forall {k}@) is still a signature.
-}
rowOf :: Text -> ExportRow
rowOf l
    | declHeaded l = case T.words l of
        (_ : n : rest) -> ExportRow n (T.unwords rest) ""
        _ -> ExportRow l "" ""
    | (n, rest) <- T.breakOn " :: " l
    , not (T.null rest) =
        ExportRow (T.strip n) (T.drop 4 rest) ""
    | otherwise = ExportRow l "" ""

declHeaded :: Text -> Bool
declHeaded l = any (`T.isPrefixOf` l) (declKeywords ++ ["=", "|", ","])
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
            Just (m, Just (T.dropWhileEnd (== ')') u))
    _ -> Nothing

hiddenPackage :: [Text] -> Maybe Text
hiddenPackage ls =
    listToMaybe
        [ T.takeWhile (/= '\'') (T.drop (T.length marker) r)
        | l <- ls
        , let (_, r) = T.breakOn marker l
        , not (T.null r)
        ]
  where
    marker = "hidden package `" :: Text

-- | A versioned unit id reduced to its package name.
packageOfUnit :: Text -> Text
packageOfUnit = fst . splitPkgVersion

-- | The card names the package; the minted line pins the version GHC named.
cabalPairs :: Text -> [Pair]
cabalPairs unitToken =
    [ "package" .= name
    , "cabal" .= ("-- cabal: build-depends: " <> pinnedDep name version)
    ]
  where
    (name, version) = splitPkgVersion unitToken
