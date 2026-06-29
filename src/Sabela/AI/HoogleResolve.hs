{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Phase-0.2 LEVER: a LOCAL Hoogle name→(package, module) resolver. When GHC
says a name is not in scope, shell out to the local @hoogle@ CLI, parse its JSON,
and rank an exact-name hit to the package and module that provides it — ready for
'Sabela.AI.DepRepair.addBuildDepend' + 'Sabela.AI.ImportRepair.addImport'. The
public hoogle.haskell.org API is NEVER consulted; on any failure (binary absent,
no database, empty/malformed output) the resolver returns Nothing.
-}
module Sabela.AI.HoogleResolve (
    HoogleHit (..),
    parseHoogleBlob,
    rankResolve,
    rankResolveTopK,
    hoogleResolve,
    hoogleResolveTopK,
    hoogleQuery,
    rankHits,
    ecosystemScore,
    keywords,
    denoise,
    isTypeOrName,
    isSingleToken,
    bigrams,
    roundRobin,
) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

{- | One hoogle result: the named item plus the package and module it lives in,
and (for capability search) its type signature and a stripped docs blurb. The
type is the @item@ text after @::@; @hhDocs@ is the @docs@ field with HTML tags
removed. Name-resolution callers ignore the last two fields.
-}
data HoogleHit = HoogleHit
    { hhName :: Text
    , hhPackage :: Text
    , hhModule :: Text
    , hhType :: Text
    , hhDocs :: Text
    }
    deriving (Eq, Show)

{- | Parse the local @hoogle search --json@ blob into hits. Accepts both a single
JSON array and JSONL (one object per line); a hoogle error line or malformed JSON
yields no hits. The leading identifier of each @item@ signature is the name.
-}
parseHoogleBlob :: Text -> [HoogleHit]
parseHoogleBlob blob =
    case A.decode (BL.pack (T.unpack blob)) of
        Just vs -> mapMaybe hitFromValue vs
        Nothing -> mapMaybe (decodeLine . T.unpack) (T.lines blob)
  where
    decodeLine l = A.decode (BL.pack l) >>= hitFromValue

hitFromValue :: A.Value -> Maybe HoogleHit
hitFromValue = A.parseMaybe $ \v -> A.withObject "hit" parse v
  where
    parse o = do
        item <- o A..: "item"
        modObj <- o A..: "module"
        modName <- modObj A..: "name"
        pkgObj <- o A..: "package"
        pkgName <- pkgObj A..: "name"
        docs <- o A..:? "docs" A..!= ""
        pure
            (HoogleHit (itemName item) pkgName modName (itemType item) (stripHtml docs))
    itemName = T.takeWhile isItemChar . T.strip
    isItemChar c = c /= ' ' && c /= ':'

{- | The type signature in an @item@: the text after the first @::@, whitespace
collapsed. Empty when the item has no signature (a type/class/module hit).
-}
itemType :: Text -> Text
itemType item = case T.breakOn "::" item of
    (_, rest)
        | T.null rest -> ""
        | otherwise -> T.unwords (T.words (T.drop 2 rest))

{- | Strip the @\<a\>@/@\<i\>@/@\<tt\>@ markup hoogle wraps docs in, and collapse
whitespace, leaving a plain-text blurb.
-}
stripHtml :: Text -> Text
stripHtml = T.unwords . T.words . go
  where
    go t = case T.breakOn "<" t of
        (before, rest)
            | T.null rest -> before
            | otherwise -> before <> go (T.drop 1 (T.dropWhile (/= '>') rest))

{- | The best (package, module) for an exact-name match, or Nothing when no hit's
name equals the query. A thin head-of-shortlist wrapper over 'rankResolveTopK'.
-}
rankResolve :: Text -> [HoogleHit] -> Maybe (Text, Text)
rankResolve name hits = case rankResolveTopK 1 name hits of
    (c : _) -> Just c
    [] -> Nothing

{- | The top-K (package, module) exact-name candidates, ranked best-first and
deduplicated. @.Internal@ modules are dropped. Without popularity data the order
is (ecosystemScore, module length, package length, module): a well-known
ecosystem package outranks a niche one, then shorter names win as a tie-break.
-}
rankResolveTopK :: Int -> Text -> [HoogleHit] -> [(Text, Text)]
rankResolveTopK k name hits =
    take (max 0 k) (nub (map toPair (sortOn rankKey exact)))
  where
    exact = filter (\h -> hhName h == name && not (isInternal (hhModule h))) hits
    toPair h = (hhPackage h, hhModule h)
    rankKey h =
        ( ecosystemScore (hhPackage h)
        , T.length (hhModule h)
        , T.length (hhPackage h)
        , hhModule h
        )
    isInternal m = ".Internal" `T.isInfixOf` m || m == "Internal"

{- | A no-popularity-data ranking signal: 0 for a curated well-known ecosystem
package, 2 for a clearly niche/odd one (long hyphenated name or a vendor-ish
token), 1 otherwise. Lower sorts first.
-}
ecosystemScore :: Text -> Int
ecosystemScore pkg
    | pkg `elem` ecosystemPackages = 0
    | isNiche pkg = 2
    | otherwise = 1
  where
    isNiche p =
        let segs = T.splitOn "-" p
         in length segs >= 3
                || T.length p >= 22
                || any (`T.isInfixOf` p) vendorTokens
    vendorTokens = ["http-haskell", "client", "sdk", "api-"]

-- | A modest allow-list of well-known ecosystem packages (no popularity data).
ecosystemPackages :: [Text]
ecosystemPackages =
    [ "base"
    , "containers"
    , "text"
    , "bytestring"
    , "vector"
    , "time"
    , "aeson"
    , "conduit"
    , "mtl"
    , "transformers"
    , "unordered-containers"
    , "regex-tdfa"
    , "megaparsec"
    , "fgl"
    , "JuicyPixels"
    , "async"
    , "http-conduit"
    , "directory"
    , "filepath"
    , "process"
    , "stm"
    ]

{- | Resolve a not-in-scope name to its (package, module) via the LOCAL hoogle CLI
only. Env @SABELA_HOOGLE_BIN@ overrides the binary (default @hoogle@) and
@SABELA_HOOGLE_DB@ adds @--database@. Returns Nothing on any failure — there is no
network fallback.
-}
hoogleResolve :: Text -> IO (Maybe (Text, Text))
hoogleResolve name = do
    cands <- hoogleResolveTopK 1 name
    pure $ case cands of
        (c : _) -> Just c
        [] -> Nothing

{- | The top-K (package, module) candidates via the LOCAL hoogle CLI only. Env
@SABELA_HOOGLE_BIN@ overrides the binary (default @hoogle@) and
@SABELA_HOOGLE_DB@ adds @--database@. Empty on any failure — no network fallback.
-}
hoogleResolveTopK :: Int -> Text -> IO [(Text, Text)]
hoogleResolveTopK k name
    | T.null name = pure []
    | otherwise = do
        bin <- fromMaybe "hoogle" <$> lookupEnv "SABELA_HOOGLE_BIN"
        db <- lookupEnv "SABELA_HOOGLE_DB"
        let dbArg = maybe [] (\p -> ["--database=" ++ p]) db
            args = ["search", "--count=20", "--json"] ++ dbArg ++ [T.unpack name]
        out <- runHoogle bin args
        pure $ case out of
            Nothing -> []
            Just blob -> rankResolveTopK k name (parseHoogleBlob blob)

{- | Run an arbitrary hoogle query (free text, a type signature, or a name)
against the LOCAL hoogle CLI and return the parsed rich hits, ranked best-first.
The installed hoogle auto-detects word vs type vs name, so no mode flag is
passed. Env @SABELA_HOOGLE_BIN@ / @SABELA_HOOGLE_DB@ override the binary and
database as elsewhere. Empty on any failure (binary/db absent, empty output) —
never the public API, never an exception.

The local hoogle treats a multi-word free-text query as a strict conjunction
over names+docs, so a plain-language phrase ("compute how different two strings
are") usually misses, and a language/meta word ("Geohash Haskell") drags in
@sbv@/@ghc@ over the package the user means. For a free-text query (no @-\>@ /
@::@, more than one word) we first 'denoise' it — strip the language/meta
stoplist (haskell, library, package, …), NEVER a domain word — then fall back,
widening progressively: the de-noised phrase, the stop-word-stripped keywords,
adjacent salient bigrams (unioned), then single keywords interleaved round-robin
so a generic word ("parse", "compute") can't flood the top over a rarer one
("version"). Type and single-token name queries are run verbatim — no cleaning.
-}
hoogleQuery :: Int -> Text -> IO [HoogleHit]
hoogleQuery k query
    | T.null (T.strip query) = pure []
    | isTypeOrName query || isSingleToken query = takeRanked <$> rawQuery k query
    | otherwise = firstNonEmpty stages
  where
    cleaned = denoise query
    kws = keywords cleaned
    stages =
        [ rawQuery k query
        , if cleaned /= query then rawQuery k cleaned else pure []
        , if length kws < length (T.words cleaned)
            then rawQuery k (T.unwords kws)
            else pure []
        , concat <$> mapM (rawQuery k) (bigrams kws)
        , roundRobin <$> mapM (rawQuery k) kws
        ]
    takeRanked = take (max 0 k) . rankHits
    firstNonEmpty [] = pure []
    firstNonEmpty (s : ss) = do
        hits <- takeRanked <$> s
        if null hits then firstNonEmpty ss else pure hits

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

{- | Strip language/meta noise tokens from a FREE-TEXT query so the salient
domain words survive (@"Geohash Haskell" -> "geohash"@). Case-insensitive,
whole-word; the multi-word phrases ("in haskell", "how to", …) are removed
first. Only the meta stoplist is touched — NEVER a domain word (image, geohash,
qr, yaml, …). Never empties a query: when every word is noise, return it as-is.
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
        bin <- fromMaybe "hoogle" <$> lookupEnv "SABELA_HOOGLE_BIN"
        db <- lookupEnv "SABELA_HOOGLE_DB"
        let dbArg = maybe [] (\p -> ["--database=" ++ p]) db
            cnt = "--count=" ++ show (max 1 k * 3)
            args = ["search", cnt, "--json"] ++ dbArg ++ [T.unpack q]
        out <- runHoogle bin args
        pure (maybe [] parseHoogleBlob out)

{- | Rank rich hits for capability search: keep hoogle's own relevance order
(it leads with the best match) and dedup by (name, module, package). The
'ecosystemScore' allow-list is a TIE-BREAK only — applied within a single
hoogle rank, so it never reshuffles the relevance order. @.Internal@ modules
are dropped.
-}
rankHits :: [HoogleHit] -> [HoogleHit]
rankHits hits =
    map snd (nub' (sortOn rankKey (zip [0 :: Int ..] keep)))
  where
    keep = filter (not . isInternal . hhModule) hits
    rankKey (i, h) = (i, ecosystemScore (hhPackage h))
    isInternal m = ".Internal" `T.isInfixOf` m || m == "Internal"
    nub' = nubOnKey (\(_, h) -> (hhName h, hhModule h, hhPackage h))

-- | Order-preserving dedup keyed by a projection.
nubOnKey :: (Eq b) => (a -> b) -> [a] -> [a]
nubOnKey f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs

-- | Run hoogle, returning its stdout only on a clean, non-empty exit.
runHoogle :: FilePath -> [String] -> IO (Maybe Text)
runHoogle bin args = do
    r <- try (readProcessWithExitCode bin args "")
    pure $ case r of
        Left (_ :: SomeException) -> Nothing
        Right (ExitSuccess, out, _)
            | not (null out) -> Just (T.pack out)
        Right _ -> Nothing
