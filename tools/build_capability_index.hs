#!/usr/bin/env cabal
{- cabal:
build-depends: base, text, bytestring, aeson, containers, tar
             , http-conduit, http-client, directory, filepath
ghc-options: -O2 -Wall
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Offline build of the capability-search index (the SHIP retriever's data).

Reads the LOCAL Hackage index tar, picks each package's latest version, and
produces, under the data dir:

  capability-packages.jsonl   one {"name","synopsis","description"} per line
  capability-vectors.f32      normalized float32 nomic embeddings (name+syn+desc),
                              row i aligns with line i of the sidecar
  capability-vectors.meta.json  {"names":[...],"dim":N,"model":"nomic-embed-text"}
  capability-revdeps.json     {package: reverse-dependency count}

BM25 is computed at query time from the synopsis sidecar (no separate index
persisted) — see tools/capability_search.hs.

Embeddings come from a local ollama (nomic-embed-text, "search_document:" prefix).
Idempotent: rerun to refresh. ollama must be running for the embed phase; the
sidecar + revdeps phases need only the tar.

Usage:
  cabal run tools/build_capability_index.hs -- [--data-dir DIR] [--tar PATH]
                                               [--skip-embed] [--batch N]
-}
module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarE
import Control.Monad (foldM, unless, when)
import Data.Aeson (encode, object, withObject, (.:), (.=))
import qualified Data.Aeson as A
import Data.ByteString.Builder (floatLE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Network.HTTP.Client as HC
import Network.HTTP.Simple (
    getResponseBody,
    httpJSON,
    parseRequest,
    setRequestBodyJSON,
    setRequestResponseTimeout,
 )
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getHomeDirectory,
 )
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- --- args ------------------------------------------------------------------

data Args = Args
    { aDataDir :: FilePath
    , aTar :: Maybe FilePath
    , aSkipEmbed :: Bool
    , aBatch :: Int
    }

parseArgs :: [String] -> Args
parseArgs = go (Args "data" Nothing False 64)
  where
    go acc [] = acc
    go acc ("--data-dir" : d : r) = go acc{aDataDir = d} r
    go acc ("--tar" : p : r) = go acc{aTar = Just p} r
    go acc ("--skip-embed" : r) = go acc{aSkipEmbed = True} r
    go acc ("--batch" : n : r) = go acc{aBatch = readIntDef n (aBatch acc)} r
    go acc (_ : r) = go acc r
    readIntDef s d = case reads s of [(v, "")] -> v; _ -> d

defaultTar :: IO FilePath
defaultTar = do
    me <- lookupEnv "CABAL_INDEX_TAR"
    case me of
        Just p -> pure p
        Nothing -> do
            home <- getHomeDirectory
            pure (home </> ".cabal/packages/hackage.haskell.org/01-index.tar")

logErr :: String -> IO ()
logErr = hPutStrLn stderr

main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    tarPath <- maybe defaultTar pure (aTar args)
    let dataDir = aDataDir args
    createDirectoryIfMissing True dataDir
    ok <- doesFileExist tarPath
    unless ok $ do
        logErr ("no Hackage index at " <> tarPath <> " — run cabal update")
        exitFailure

    let pkgsPath = dataDir </> "capability-packages.jsonl"
        vecPath = dataDir </> "capability-vectors.f32"
        metaPath = dataDir </> "capability-vectors.meta.json"
        revPath = dataDir </> "capability-revdeps.json"

    logErr "==> extracting corpus (name+synopsis+description)"
    corpus <- extractCorpus tarPath
    BL.writeFile pkgsPath (BL.intercalate "\n" (map encode corpus) <> "\n")
    logErr ("   wrote " <> pkgsPath <> " (" <> show (length corpus) <> " packages)")

    logErr "==> computing reverse-dependency popularity"
    revdeps <- computeRevdeps tarPath
    BL.writeFile revPath (encode revdeps)
    logErr ("   wrote " <> revPath <> " (" <> show (M.size revdeps) <> " packages)")

    if aSkipEmbed args
        then logErr "==> skipping embeddings (--skip-embed)"
        else do
            model <- T.pack <$> getEnvDef "CAPABILITY_EMBED_MODEL" "nomic-embed-text"
            url <- getEnvDef "OLLAMA_EMBED_URL" "http://localhost:11434/api/embed"
            logErr ("==> embedding corpus with " <> T.unpack model <> " (ollama)")
            embedCorpus url model corpus vecPath metaPath (aBatch args)
    logErr "==> capability index built"

getEnvDef :: String -> String -> IO String
getEnvDef k d = fromMaybe d <$> lookupEnv k

-- --- corpus ----------------------------------------------------------------

data Pkg = Pkg {pName :: !Text, pSyn :: !Text, pDesc :: !Text}

instance A.ToJSON Pkg where
    toJSON p = object ["name" .= pName p, "synopsis" .= pSyn p, "description" .= pDesc p]

-- | Read all entries; a trailing format error truncates the list.
readEntries :: FilePath -> IO [Tar.Entry]
readEntries p = Tar.foldEntries (:) [] (const []) . Tar.read <$> BL.readFile p

-- | Latest version per package -> sorted-by-name corpus rows.
extractCorpus :: FilePath -> IO [Pkg]
extractCorpus tarPath = do
    entries <- readEntries tarPath
    let m = foldl' (flip step) M.empty entries
    pure
        [ Pkg name syn (T.take 500 desc)
        | (name, (_, syn, desc)) <- sortOn fst (M.toList m)
        ]
  where
    step e acc = case cabalParts e of
        Just (pkg, ver) ->
            let vt = parseVer ver
             in case M.lookup pkg acc of
                    Just (cur, _, _) | vt <= cur -> acc
                    _ ->
                        let txt = entryText e
                         in M.insert pkg (vt, field "synopsis" txt, field "description" txt) acc
        Nothing -> acc

-- | @pkg/ver/pkg.cabal@ (>= 3 path parts) -> @(pkg, ver)@.
cabalParts :: Tar.Entry -> Maybe (Text, Text)
cabalParts e
    | ".cabal" `isSuffixOf` path
    , (a : b : _ : _) <- parts =
        Just (T.pack a, T.pack b)
    | otherwise = Nothing
  where
    path = TarE.entryPath e
    parts = splitSlash path

entryText :: Tar.Entry -> Text
entryText e = case TarE.entryContent e of
    TarE.NormalFile bs _ -> TE.decodeUtf8With TEE.lenientDecode (BL.toStrict bs)
    _ -> ""

parseVer :: Text -> [Int]
parseVer = map (fromMaybe 0 . readMaybeInt) . T.splitOn "."
  where
    readMaybeInt t = case reads (T.unpack t) of [(n, "")] -> Just n; _ -> Nothing

splitSlash :: FilePath -> [String]
splitSlash = foldr f [[]]
  where
    f '/' acc = [] : acc
    f c (cur : rest) = (c : cur) : rest
    f _ [] = [[]]

{- | Extract a cabal field value with line folding: the text after @name:@ plus
subsequent whitespace-led continuation lines, joined; @.@ and blank lines drop.
-}
field :: Text -> Text -> Text
field name txt = T.unwords (filter keep (map T.strip (firstVal : continuation)))
  where
    ls = T.lines txt
    matched = dropWhile (not . isField) ls
    keep l = not (T.null l) && l /= "."
    (firstVal, continuation) = case matched of
        (l : rest) -> (afterColon l, takeWhile isCont rest)
        [] -> ("", [])
    isCont l = not (T.null l) && (T.head l == ' ' || T.head l == '\t')
    isField l =
        let s = T.dropWhile (\c -> c == ' ' || c == '\t') l
            lower = T.toLower s
         in name `T.isPrefixOf` lower
                && ":"
                    `T.isPrefixOf` T.dropWhile (\c -> c == ' ' || c == '\t') (T.drop (T.length name) s)
    afterColon l =
        let s = T.dropWhile (\c -> c == ' ' || c == '\t') l
            afterName = T.dropWhile (\c -> c == ' ' || c == '\t') (T.drop (T.length name) s)
         in T.drop 1 afterName

-- --- reverse dependencies --------------------------------------------------

-- | Count distinct depending packages per target, over latest versions.
computeRevdeps :: FilePath -> IO (M.Map Text Int)
computeRevdeps tarPath = do
    latest <- pickLatest <$> readEntries tarPath
    let chosen = S.fromList [n | (_, n) <- M.elems latest]
        pkgOf = M.fromList [(n, p) | (p, (_, n)) <- M.toList latest]
    entries <- readEntries tarPath
    let revsets = foldl' (flip (step chosen pkgOf)) M.empty entries
    pure (M.map S.size revsets)
  where
    pickLatest = foldl' (flip pl) M.empty
    pl e acc = case cabalParts3 e of
        Just (pkg, ver) ->
            let k = parseVer ver
             in case M.lookup pkg acc of
                    Just (cur, _) | k <= cur -> acc
                    _ -> M.insert pkg (k, TarE.entryPath e) acc
        Nothing -> acc
    step chosen pkgOf e acc
        | TarE.entryPath e `S.member` chosen
        , Just src <- M.lookup (TarE.entryPath e) pkgOf =
            foldl'
                (\m d -> if d == src then m else M.insertWith S.union d (S.singleton src) m)
                acc
                (depsOf (entryText e))
        | otherwise = acc

-- | Like 'cabalParts' but requires exactly 3 path parts (mirrors the Python).
cabalParts3 :: Tar.Entry -> Maybe (Text, Text)
cabalParts3 e = case splitSlash (TarE.entryPath e) of
    [a, b, c] | ".cabal" `isSuffixOf` c -> Just (T.pack a, T.pack b)
    _ -> Nothing

-- | Package names appearing in any @build-depends@ stanza of a cabal file.
depsOf :: Text -> S.Set Text
depsOf txt = go (T.lines txt)
  where
    go [] = S.empty
    go (l : rest)
        | isDepField l =
            let baseIndent = indent l
                (cont, rest') = span (contLine baseIndent) rest
                chunks = T.splitOn "," (T.unwords (afterColon l : cont))
             in S.union (S.fromList (concatMap pkgName chunks)) (go rest')
        | otherwise = go rest
    isDepField l =
        let s = T.dropWhile ws l
         in "build-depends" `T.isPrefixOf` T.toLower s
                && ":" `T.isPrefixOf` T.dropWhile ws (T.drop (T.length "build-depends") s)
    contLine baseIndent l
        | T.null (T.strip l) = True
        | otherwise = not (indent l <= baseIndent && (isFieldStart l || isStanza l))
    afterColon l = T.drop 1 (T.dropWhile (/= ':') l)
    ws c = c == ' ' || c == '\t'
    indent l = T.length (T.takeWhile ws l)
    pkgName chunk = case T.dropWhile ws chunk of
        s | not (T.null s), isAlpha (T.head s) -> [T.takeWhile isPkgChar s]
        _ -> []
    isAlpha c = isAsciiLower c || isAsciiUpper c
    isPkgChar c = isAlpha c || isDigit c || c == '_' || c == '-'
    isFieldStart l =
        let s = T.dropWhile ws l
         in not (T.null s) && isAlpha (T.head s) && hasColonName s
    hasColonName s = ":" `T.isPrefixOf` T.dropWhile isFieldNameChar s
    isFieldNameChar c = isAlpha c || isDigit c || c == '_' || c == '-'
    isStanza l =
        let w = T.toLower (T.takeWhile (not . ws) (T.dropWhile ws l))
         in w
                `elem` [ "library"
                       , "executable"
                       , "test-suite"
                       , "benchmark"
                       , "foreign-library"
                       , "common"
                       , "source-repository"
                       , "flag"
                       ]

-- --- embeddings ------------------------------------------------------------

newtype EmbedResp = EmbedResp [[Double]]
instance A.FromJSON EmbedResp where
    parseJSON = withObject "EmbedResp" $ \o -> EmbedResp <$> o .: "embeddings"

docText :: Pkg -> Text
docText p =
    "search_document: "
        <> T.intercalate
            " \8212 "
            ([pName p] <> nonEmpty (pSyn p) <> nonEmpty (T.take 300 (pDesc p)))
  where
    nonEmpty t = [t | not (T.null t)]

embedCorpus :: String -> Text -> [Pkg] -> FilePath -> FilePath -> Int -> IO ()
embedCorpus url model corpus vecPath metaPath batch = do
    let docs = map docText corpus
        names = map pName corpus
    allv <- embedBatched url model batch docs
    case allv of
        [] -> logErr "   embed: no vectors returned" >> exitFailure
        (v0 : _) -> do
            let dim = length v0
                normalized = [normalize v | v <- allv]
            BL.writeFile
                vecPath
                (toLazyByteString (mconcat [floatLE (realToFrac x) | v <- normalized, x <- v]))
            BL.writeFile
                metaPath
                (encode (object ["names" .= names, "dim" .= dim, "model" .= model]))
            logErr
                ( "   embed: "
                    <> show (length names)
                    <> " vectors (dim "
                    <> show dim
                    <> ")"
                )

normalize :: [Double] -> [Double]
normalize v = map (/ d) v
  where
    n = sqrt (sum (map (\x -> x * x) v))
    d = if n == 0 then 1 else n

embedBatched :: String -> Text -> Int -> [Text] -> IO [[Double]]
embedBatched url model batch docs = concat . reverse <$> foldM step [] (chunksOf batch docs)
  where
    total = length docs
    step acc chunk = do
        EmbedResp vss <- postEmbed url model chunk
        let done = sum (map length acc) + length vss
        when (done `mod` (batch * 40) < length vss) $
            logErr ("   embed: " <> show done <> "/" <> show total)
        pure (vss : acc)

postEmbed :: String -> Text -> [Text] -> IO EmbedResp
postEmbed url model texts = do
    req0 <- parseRequest ("POST " ++ url)
    let req =
            setRequestResponseTimeout (HC.responseTimeoutMicro (600 * 1000000)) $
                setRequestBodyJSON (object ["model" .= model, "input" .= texts]) req0
    getResponseBody <$> httpJSON req

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
