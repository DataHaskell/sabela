{-# LANGUAGE OverloadedStrings #-}

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

hoogleQuery :: Int -> Text -> IO [HoogleHit]
hoogleQuery = hoogleQueryInScope Set.empty

hoogleQueryInScope :: Set Text -> Int -> Text -> IO [HoogleHit]
hoogleQueryInScope inScope = hoogleQueryWithRank (rankHitsInScope inScope) rawQuery

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

actionNeedStage ::
    (Int -> Text -> IO [HoogleHit]) -> Int -> Text -> IO [HoogleHit]
actionNeedStage run k query = case intentQueries query of
    [] -> pure []
    qs -> concat <$> mapM (run k) qs

scopedRescue ::
    (Int -> Text -> IO [HoogleHit]) -> Int -> [Text] -> IO [HoogleHit]
scopedRescue run k kws = do
    pkgTerms <- filterM isPkgTerm (take maxPkgProbes kws)
    roundRobin <$> mapM (run k) (packageScopedQueries pkgTerms kws)
  where
    isPkgTerm kw = any (isPackageRow kw) <$> run 1 kw

maxPkgProbes :: Int
maxPkgProbes = 4

packageScopedQueries :: [Text] -> [Text] -> [Text]
packageScopedQueries pkgTerms kws = concatMap scoped pkgTerms
  where
    scoped p = case filter (/= p) kws of
        [] -> []
        others ->
            ("+" <> p <> " " <> T.unwords others)
                : ["+" <> p <> " " <> o | length others > 1, o <- others]

isPackageRow :: Text -> HoogleHit -> Bool
isPackageRow kw h =
    hhName h == kw
        && hhPackage h == kw
        && T.null (hhModule h)
        && T.null (hhType h)

bigrams :: [Text] -> [Text]
bigrams ws = zipWith (\a b -> a <> " " <> b) ws (drop 1 ws)

roundRobin :: [[a]] -> [a]
roundRobin xss = case filter (not . null) xss of
    [] -> []
    nonEmpty -> map head nonEmpty ++ roundRobin (map tail nonEmpty)

isTypeOrName :: Text -> Bool
isTypeOrName q = "->" `T.isInfixOf` q || "::" `T.isInfixOf` q

isSingleToken :: Text -> Bool
isSingleToken q = length (T.words q) <= 1

denoise :: Text -> Text
denoise q =
    let stripped = foldr stripPhrase (T.toLower q) noisePhrases
        kept = filter (`notElem` noiseTokens) (T.words stripped)
     in if null kept then q else T.unwords kept
  where
    stripPhrase p t = T.replace (" " <> p <> " ") " " (pad t)
    pad t = " " <> t <> " "

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

noisePhrases :: [Text]
noisePhrases =
    [ "in haskell"
    , "how do i"
    , "how to"
    , "i want to"
    , "i need to"
    ]

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

rawQuery :: Int -> Text -> IO [HoogleHit]
rawQuery k q
    | T.null (T.strip q) = pure []
    | otherwise = do
        let cnt = "--count=" ++ show (max 1 k * 3)
        queryAllDbs (["search", cnt, "--json"] ++ [T.unpack q])
