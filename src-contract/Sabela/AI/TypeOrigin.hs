{-# LANGUAGE OverloadedStrings #-}

{- | Type provenance with GHC as the identity authority. An 'OriginId' is the
structured identity GHC printed; a façade claim exists only when a probe in
the same package environment reports the identical defining site.
-}
module Sabela.AI.TypeOrigin (
    Namespace (..),
    OriginId (..),
    annotateExportedLines,
    facadeClaimKey,
    implFlavoured,
    originsFromText,
    probeAccepts,
    rankVerified,
    renderClaim,
) where

import Data.Char (isDigit, isUpper)
import Data.List (nub, sortOn)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleFits (qualifiedNames)

-- | GHC name namespaces this kernel distinguishes.
data Namespace = NsType | NsData
    deriving (Eq, Show)

{- | A defining identity as GHC printed it: unit (version kept, hash
stripped) when stated, defining module verbatim, occurrence name.
-}
data OriginId = OriginId
    { oiUnit :: Maybe Text
    , oiModule :: Text
    , oiName :: Text
    , oiNamespace :: Namespace
    }
    deriving (Eq, Show)

{- | Whether a module NAME looks like an implementation surface (any
@Internal@ segment, or a final @Base@). Gates façade lookups only; it is
never evidence and never a ranking input.
-}
implFlavoured :: Text -> Bool
implFlavoured m =
    "Internal" `elem` segs || listToMaybe (reverse segs) == Just "Base"
  where
    segs = T.splitOn "." m

{- | The impl-flavoured defining identities a raw GHC text states, from both
spellings: @M.C@ tokens, and @‘X’ … is defined in ‘M’@ pairs (which may
wrap across lines). Deduped on (module, name), units merged preferring known.
-}
originsFromText :: Text -> [OriginId]
originsFromText raw = foldl addIn [] (pairOrigins <> tokenOrigins)
  where
    pairs = definedInPairs raw
    pairOrigins =
        [ OriginId unit m (dottedLast name) NsType
        | (name, ref) <- pairs
        , let (unit, m) = splitUnitRef ref
        , implFlavoured m
        , upperHeaded (dottedLast name)
        ]
    definedInModules = [snd (splitUnitRef r) | (_, r) <- pairs]
    tokenOrigins =
        [ OriginId Nothing m c NsType
        | (m, c) <- qualifiedNames raw
        , implFlavoured m
        , upperHeaded c
        , (m <> "." <> c) `notElem` definedInModules
        ]
    addIn acc o = case break (sameKey o) acc of
        (_, []) -> acc <> [o]
        (before, found : after) -> before <> (mergeUnits found o : after)
    sameKey o x = (oiModule x, oiName x) == (oiModule o, oiName o)
    mergeUnits kept o
        | isJust (oiUnit kept) = kept
        | otherwise = kept{oiUnit = oiUnit o}

-- | A quoted token, or the @is defined in@ phrase, in text order.
data Tok = TQuote Text | TMark

{- | Each @is defined in ‘M’@ occurrence paired with the nearest preceding
quoted name, tolerant of GHC's line wrapping between the two.
-}
definedInPairs :: Text -> [(Text, Text)]
definedInPairs raw = go Nothing (tokenStream raw)
  where
    go seen (TMark : rest) = case (seen, [q | TQuote q <- take 1 rest]) of
        (Just name, ref : _) -> (name, ref) : go seen rest
        _ -> go seen rest
    go _ (TQuote t : rest) = go (Just t) rest
    go _ [] = []

tokenStream :: Text -> [Tok]
tokenStream raw = case map (map TQuote . quotedIn) chunks of
    [] -> []
    (h : t) -> h <> concatMap (TMark :) t
  where
    chunks = T.splitOn "is defined in" raw

-- | GHC's unicode-quoted tokens, in order.
quotedIn :: Text -> [Text]
quotedIn t = case T.breakOn "\8216" t of
    (_, r)
        | T.null r -> []
        | otherwise ->
            let (tok, after) = T.breakOn "\8217" (T.drop 1 r)
             in if T.null after then [] else tok : quotedIn (T.drop 1 after)

{- | Whether a probe's @:info@ output proves the candidate module exports the
exact defining Name: a type-namespace declaration of the name whose own
defined-in line (never an instance's) matches module and, when stated, unit.
-}
probeAccepts :: OriginId -> Text -> Bool
probeAccepts o probe = any provedAt headIndexes
  where
    ls = map T.strip (T.lines probe)
    headIndexes =
        [i | (i, l) <- zip [0 :: Int ..] ls, declaredName l == Just (oiName o)]
    provedAt i = case dropWhile (not . stops) (drop (i + 1) ls) of
        (l : _) -> not (isInstanceLine l) && definedInMatches l
        [] -> False
    stops l = isInstanceLine l || "-- Defined in" `T.isInfixOf` l
    isInstanceLine = T.isPrefixOf "instance"
    definedInMatches l = case quotedIn l of
        (ref : _) ->
            let (unit, m) = splitUnitRef ref
             in m == oiModule o && unitsAgree unit (oiUnit o)
        [] -> False
    unitsAgree (Just a) (Just b) = a == b
    unitsAgree _ _ = True

-- | The type-namespace name a decl-head line declares, if any.
declaredName :: Text -> Maybe Text
declaredName l = listToMaybe (mapMaybe nameAfter heads)
  where
    heads = ["data family", "type family", "data", "newtype", "type", "class"]
    nameAfter kw = do
        rest <- T.stripPrefix (kw <> " ") l
        w <- listToMaybe (T.words rest)
        let cleaned = T.takeWhile (`notElem` (":(" :: String)) w
        if upperHeaded cleaned then Just cleaned else Nothing

{- | Deterministic preference over ALREADY VERIFIED façades: longest shared
prefix with the origin, then shortest name, then lexicographic. The origin
itself is silenced — it already is the import surface.
-}
rankVerified :: Text -> [Text] -> [Text]
rankVerified origin = sortOn key . nub . filter (/= origin)
  where
    key m = (Down (sharedSegs m), T.length m, m)
    sharedSegs m =
        length . takeWhile (uncurry (==)) $
            zip (T.splitOn "." origin) (T.splitOn "." m)

-- | The claim value: @pkg:Module@ when the unit is known, bare otherwise.
renderClaim :: OriginId -> Text -> Text
renderClaim o facade =
    maybe facade (\u -> unitPackage u <> ":" <> facade) (oiUnit o)

{- | The claim key: the short name, unless another origin in the batch shares
it with a different defining module — then the qualified spelling verbatim.
-}
facadeClaimKey :: [OriginId] -> OriginId -> Text
facadeClaimKey batch o
    | any clashes batch = oiModule o <> "." <> oiName o
    | otherwise = oiName o
  where
    clashes x = oiName x == oiName o && oiModule x /= oiModule o

{- | The verified facts appended in-line: a line whose defined-in module is a
claimed origin gains @ (exported by pkg:Module)@. Lines without a matching
claim stay byte-identical.
-}
annotateExportedLines :: [(OriginId, Text)] -> Text -> Text
annotateExportedLines [] t = t
annotateExportedLines claims t = T.intercalate "\n" (map annotate (T.splitOn "\n" t))
  where
    annotate l = case claimFor l of
        Just (o, facade) -> l <> " (exported by " <> renderClaim o facade <> ")"
        Nothing -> l
    claimFor l
        | "efined in" `T.isInfixOf` l =
            listToMaybe
                [ c
                | ref <- take 1 (reverse (quotedIn l))
                , c@(o, _) <- claims
                , snd (splitUnitRef ref) == oiModule o
                ]
        | otherwise = Nothing

-- | @vector-0.13.2.0[-hash]:M@ split into its normalized unit and module.
splitUnitRef :: Text -> (Maybe Text, Text)
splitUnitRef ref = case T.breakOnEnd ":" ref of
    (unitColon, m)
        | T.null unitColon -> (Nothing, m)
        | otherwise -> (Just (normalizeUnit (T.dropEnd 1 unitColon)), m)

-- | Unit text with any trailing hash dropped: the name through the version.
normalizeUnit :: Text -> Text
normalizeUnit u = case break versionish (T.splitOn "-" u) of
    (name, v : _) | not (null name) -> T.intercalate "-" (name <> [v])
    _ -> u

unitPackage :: Text -> Text
unitPackage u = case break versionish (T.splitOn "-" u) of
    (name@(_ : _), _) -> T.intercalate "-" name
    _ -> u

versionish :: Text -> Bool
versionish s = not (T.null s) && T.all (\c -> isDigit c || c == '.') s

dottedLast :: Text -> Text
dottedLast = last . T.splitOn "."

upperHeaded :: Text -> Bool
upperHeaded t = maybe False (isUpper . fst) (T.uncons t)
