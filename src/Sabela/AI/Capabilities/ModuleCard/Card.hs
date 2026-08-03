{-# LANGUAGE OverloadedStrings #-}

{- | Assembling a module or package card from facts already gathered. Everything
here is pure so the card's claims can be checked without a compiler: what a card
says about a module is decided by this function alone.
-}
module Sabela.AI.Capabilities.ModuleCard.Card (
    moduleCardValue,
    packageCard,
    modulePairs,
    exposedByPairs,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (group, nub, partition, sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capability (
    Capability (..),
    parseCapabilities,
    synonymDecl,
    unqualify,
 )
import Sabela.AI.PackageIndex (
    PackageEntry (..),
    packagesExposingModule,
 )
import Sabela.AI.RepairDispatch (DiagClass (ClassHiddenPackage), diagClassText)
import Sabela.AI.Search.Export (ExportRow (..), rankExportsBy)
import Sabela.AI.Search.Need (parseNeed)

exportCap :: Int
exportCap = 60

{- | The card for one module of one installed package. The browse outcome is an
@Either@ because a probe that failed is not a listing, and the difference is the
difference between a fact and a guess.
-}
moduleCardValue ::
    Maybe Text ->
    [PackageEntry] ->
    PackageEntry ->
    Text ->
    Maybe Text ->
    Text ->
    Either Text Text ->
    Value
moduleCardValue mQuery index pkg modName asked syn browsed =
    object
        ( [ "module" .= modName
          , "package" .= peName pkg
          , "version" .= peVersion pkg
          , "status" .= diagClassText ClassHiddenPackage
          , "cabal" .= ("-- cabal: build-depends: " <> peName pkg)
          ]
            <> ["resolvedFrom" .= a | Just a <- [asked]]
            <> ["synopsis" .= syn | not (T.null syn)]
            <> exposedByPairs index pkg modName
            <> exportAxis mQuery pkg modName browsed
        )

{- | What the module exports, or why the card cannot say. The empty parse is
never promoted to a claim about the module: an unenumerated axis is reported as
unenumerated, beside the package's own module list, which ghc-pkg does back.
-}
exportAxis :: Maybe Text -> PackageEntry -> Text -> Either Text Text -> [Pair]
exportAxis mQuery pkg modName browsed = case browsed of
    Right raw
        | caps@(_ : _) <- parseCapabilities "" raw ->
            exportPairs mQuery modName raw caps
    Right _ -> unenumerated ["note" .= noParse] <> modulePairs (peModules pkg)
    Left reason -> unenumerated ["error" .= reason] <> modulePairs (peModules pkg)
  where
    noParse = ":browse " <> modName <> " returned no exports this parser reads"

unenumerated :: [Pair] -> [Pair]
unenumerated why =
    [ "coverage"
        .= object
            (["axis" .= ("exports" :: Text), "enumerated" .= False] <> why)
    ]

{- | Every other installed package that exposes the module. A module one
installed package re-exports is reachable through it, and a card naming only the
package the dump happened to list first sends the caller after a dependency the
notebook may already hold.
-}
exposedByPairs :: [PackageEntry] -> PackageEntry -> Text -> [Pair]
exposedByPairs index pkg modName
    | null others = []
    | otherwise =
        ["exposedBy" .= take exposedByCap others]
            <> [ "moreExposedBy" .= (length others - exposedByCap)
               | length others > exposedByCap
               ]
  where
    others =
        [ n
        | n <- sort (nub (map peName (packagesExposingModule index modName)))
        , n /= peName pkg
        ]

exposedByCap :: Int
exposedByCap = 4

exportPairs :: Maybe Text -> Text -> Text -> [Capability] -> [Pair]
exportPairs mQuery modName raw caps
    | null shown = []
    | otherwise =
        ["exports" .= map render shown]
            <> ["omitted" .= omitted | omitted > 0]
  where
    ranked =
        rankExportsBy
            (parseNeed Set.empty (fromMaybe "" mQuery))
            modName
            (\c -> ExportRow (capName c) (capType c) "")
            (rankExports modName raw caps)
    kept = take exportCap ranked
    shown = kept ++ mentionedSynonyms ranked kept
    omitted = length ranked - length shown
    render c = fromMaybe (capName c <> " :: " <> capType c) (synonymDecl c)

{- | The synonyms the shown exports name, which the width cap would otherwise
drop: a signature naming a type whose expansion the card cut leaves the caller
with a name and no way to build one.
-}
mentionedSynonyms :: [Capability] -> [Capability] -> [Capability]
mentionedSynonyms ranked shown =
    take
        mentionCap
        [ c
        | c <- ranked
        , c `notElem` shown
        , isJust (synonymDecl c)
        , any (elem (capName c) . typeWords . capType) shown
        ]
  where
    typeWords = T.split (\ch -> not (isAlphaNum ch || ch `elem` ("_'" :: String)))

mentionCap :: Int
mentionCap = 3

rankExports :: Text -> Text -> [Capability] -> [Capability]
rankExports modName raw caps = core ++ named ++ internal ++ ops
  where
    namesake = T.takeWhileEnd (/= '.') modName
    (alpha0, ops) = partition isNamed caps
    (internal, alpha) =
        partition (\c -> capName c `Set.member` internalNames) alpha0
    (core, named) = partition mentionsNamesake alpha
    isNamed c = maybe False (isAlpha . fst) (T.uncons (capName c))
    mentionsNamesake c = namesake `T.isInfixOf` capType c
    internalNames =
        Set.fromList
            [ unqualify w
            | l <- T.lines raw
            , (w : _) <- [T.words l]
            , ".Internal." `T.isInfixOf` w
            ]

packageCard :: PackageEntry -> Text -> Value
packageCard p syn =
    object
        ( [ "package" .= peName p
          , "version" .= peVersion p
          , "status" .= diagClassText ClassHiddenPackage
          , "cabal" .= ("-- cabal: build-depends: " <> peName p)
          ]
            <> modulePairs (peModules p)
            <> ["synopsis" .= syn | not (T.null syn)]
        )

{- | The module list a card carries. Over the cap, names collapse to their
two-component namespace with a count: truncating by name order drops whichever
module sorts late, which is as often as not the one the caller needs.
-}
modulePairs :: [Text] -> [Pair]
modulePairs ms
    | length ms <= packageModuleCap = ["modules" .= ms]
    | otherwise =
        [ "modules" .= shown
        , "moreModules" .= (length ms - length shown)
        ]
  where
    shown = take packageModuleCap (bucketModules ms)

bucketModules :: [Text] -> [Text]
bucketModules ms =
    [ if n > 1 then p <> " (" <> T.pack (show n) <> ")" else p
    | g@(p : _) <- group (sort (map prefix2 ms))
    , let n = length g
    ]
  where
    prefix2 = T.intercalate "." . take 2 . T.splitOn "."

packageModuleCap :: Int
packageModuleCap = 20
