{-# LANGUAGE OverloadedStrings #-}

{- | The free-text (prose) hoogle query pipeline: denoise, progressive
widening, the action-need stage ("Sabela.AI.HoogleIntent"), and the
package-scoped rescue (never silently drops a term naming a package).
-}
module Sabela.AI.HoogleProse (
    hoogleQuery,
    hoogleQueryInScope,
    hoogleQueryWith,
    packageScopedQueries,
    isPackageRow,
    keywords,
    denoise,
    isTypeOrName,
    isSingleToken,
    bigrams,
    roundRobin,
) where

import Control.Monad (filterM)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Set (Set)
import qualified Data.Set as Set
import Sabela.AI.HoogleClient (HoogleHit (..), queryAllDbs)
import Sabela.AI.HoogleIntent (intentQueries)

import Sabela.AI.HoogleRank (rankHits, rankHitsInScope)

{- | Run a hoogle query (free text, type, or name) against the LOCAL hoogle
CLI only, returning ranked rich hits; empty on any failure, never a throw.
@SABELA_HOOGLE_BIN@ / @SABELA_HOOGLE_DB@ override binary and database.
-}
hoogleQuery :: Int -> Text -> IO [HoogleHit]
hoogleQuery = hoogleQueryInScope Set.empty

{- | 'hoogleQuery' under B2's scope gate: a hit from a package already in
scope outranks one that is not, so an out-of-scope exact name cannot
displace an in-scope alternative. Passing an empty set is the ungated
behaviour.
-}
hoogleQueryInScope :: Set Text -> Int -> Text -> IO [HoogleHit]
hoogleQueryInScope inScope = hoogleQueryWithRank (rankHitsInScope inScope) rawQuery

{- | 'hoogleQuery' over an injectable runner (the ladder tests' seam). Prose
widens progressively: denoised phrase, keywords, the action need, the
package-scoped rescue, bigrams, round-robin singles.
-}
hoogleQueryWith ::
    (Int -> Text -> IO [HoogleHit]) -> Int -> Text -> IO [HoogleHit]
hoogleQueryWith = hoogleQueryWithRank rankHits

hoogleQueryWithRank ::
    ([HoogleHit] -> [HoogleHit]) ->
    (Int -> Text -> IO [HoogleHit]) ->
    Int ->
    Text ->
    IO [HoogleHit]
hoogleQueryWithRank ranker run k query
    | T.null (T.strip query) = pure []
    | isTypeOrName query || isSingleToken query = takeRanked <$> run k query
    | otherwise = firstNonEmpty stages
  where
    cleaned = denoise query
    kws = keywords cleaned
    stages =
        [ run k query
        , if cleaned /= query then run k cleaned else pure []
        , if length kws < length (T.words cleaned)
            then run k (T.unwords kws)
            else pure []
        , actionNeedStage run k query
        , scopedRescue run k kws
        , concat <$> mapM (run k) (bigrams kws)
        , roundRobin <$> mapM (run k) kws
        ]
    takeRanked = take (max 0 k) . ranker
    firstNonEmpty [] = pure []
    firstNonEmpty (s : ss) = do
        hits <- takeRanked <$> s
        if null hits then firstNonEmpty ss else pure hits

{- | The classified action's need queries, run BEFORE the ladder's
bigram\/single-noun stages so a request never widens straight down to an
object-noun-only search. Empty when the request classifies to no action.
-}
actionNeedStage ::
    (Int -> Text -> IO [HoogleHit]) -> Int -> Text -> IO [HoogleHit]
actionNeedStage run k query = case intentQueries query of
    [] -> pure []
    qs -> concat <$> mapM (run k) qs

{- | The rescue stage: keywords a package row answers for (evidence class
"the query names a package", never a name list) scope the remaining terms
with hoogle's own @+package@ filter — every rescue hit honours the package.
-}
scopedRescue ::
    (Int -> Text -> IO [HoogleHit]) -> Int -> [Text] -> IO [HoogleHit]
scopedRescue run k kws = do
    pkgTerms <- filterM isPkgTerm (take maxPkgProbes kws)
    roundRobin <$> mapM (run k) (packageScopedQueries pkgTerms kws)
  where
    isPkgTerm kw = any (isPackageRow kw) <$> run 1 kw

-- | Package-name probes one rescue may fan out to, bounding the cost.
maxPkgProbes :: Int
maxPkgProbes = 4

{- | The scoped spellings for each package-named term: the whole remaining
phrase first, then (when several terms remain) each term singly — the
within-scope widening ladder in query form.
-}
packageScopedQueries :: [Text] -> [Text] -> [Text]
packageScopedQueries pkgTerms kws = concatMap scoped pkgTerms
  where
    scoped p = case filter (/= p) kws of
        [] -> []
        others ->
            ("+" <> p <> " " <> T.unwords others)
                : ["+" <> p <> " " <> o | length others > 1, o <- others]

{- | The hoogle @package NAME@ row shape: name equals package, no module or
signature. Keys on the row's evidence shape, never on which package it is.
-}
isPackageRow :: Text -> HoogleHit -> Bool
isPackageRow kw h =
    hhName h == kw
        && hhPackage h == kw
        && T.null (hhModule h)
        && T.null (hhType h)

-- | The adjacent two-word windows of a keyword list (@[a,b,c] -> ["a b","b c"]@).
bigrams :: [Text] -> [Text]
bigrams ws = zipWith (\a b -> a <> " " <> b) ws (drop 1 ws)

{- | Interleave several result lists one-at-a-time so each source contributes
near the top, instead of concatenating (which lets the first source dominate).
-}
roundRobin :: [[a]] -> [a]
roundRobin xss = case filter (not . null) xss of
    [] -> []
    nonEmpty -> map head nonEmpty ++ roundRobin (map tail nonEmpty)

-- | A query is a type (run verbatim) if it carries @-\>@ or @::@.
isTypeOrName :: Text -> Bool
isTypeOrName q = "->" `T.isInfixOf` q || "::" `T.isInfixOf` q

-- | A single-word query is a bare name/identifier: run verbatim, never cleaned.
isSingleToken :: Text -> Bool
isSingleToken q = length (T.words q) <= 1

{- | Strip language/meta noise from a FREE-TEXT query so the salient domain
words survive (@"Geohash Haskell" -> "geohash"@); NEVER a domain word, and
never empties a query (all-noise queries return as-is).
-}
denoise :: Text -> Text
denoise q =
    let stripped = foldr stripPhrase (T.toLower q) noisePhrases
        kept = filter (`notElem` noiseTokens) (T.words stripped)
     in if null kept then q else T.unwords kept
  where
    stripPhrase p t = T.replace (" " <> p <> " ") " " (pad t)
    pad t = " " <> t <> " "

{- | Whole-word language/meta tokens dropped from a free-text query. These are
the only words 'denoise' removes; nothing here is a plausible domain term.
-}
noiseTokens :: [Text]
noiseTokens =
    [ "haskell"
    , "ghc"
    , "hackage"
    , "library"
    , "libraries"
    , "package"
    , "packages"
    , "function"
    , "functions"
    , "module"
    , "modules"
    ]

-- | Multi-word meta phrases dropped (whole-phrase) before the token pass.
noisePhrases :: [Text]
noisePhrases =
    [ "in haskell"
    , "how do i"
    , "how to"
    , "i want to"
    , "i need to"
    ]

{- | The meaningful keywords of a free-text query: words minus a small English
stop-word set, lower-cased. Keeps order; never empties a query (falls back to
the original words when every word is a stop-word).
-}
keywords :: Text -> [Text]
keywords q =
    let ws = T.words q
        kept = filter (\w -> T.toLower w `notElem` stopWords) ws
     in if null kept then ws else kept

stopWords :: [Text]
stopWords =
    [ "a"
    , "an"
    , "the"
    , "how"
    , "to"
    , "of"
    , "is"
    , "are"
    , "be"
    , "two"
    , "in"
    , "on"
    , "for"
    , "and"
    , "or"
    , "with"
    , "from"
    , "into"
    , "that"
    , "this"
    , "as"
    , "by"
    , "it"
    , "do"
    , "i"
    , "want"
    , "need"
    , "given"
    ]

-- | One raw hoogle search returning the parsed (unranked) hits.
rawQuery :: Int -> Text -> IO [HoogleHit]
rawQuery k q
    | T.null (T.strip q) = pure []
    | otherwise = do
        let cnt = "--count=" ++ show (max 1 k * 3)
        queryAllDbs (["search", cnt, "--json"] ++ [T.unpack q])
