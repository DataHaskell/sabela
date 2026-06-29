#!/usr/bin/env cabal
{- cabal:
build-depends: base, text, bytestring, aeson, containers, vector
             , http-conduit, http-client, directory, filepath
ghc-options: -O2 -Wall
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | SHIP capability-search query helper (the proven high-recall retriever).

Pipeline (reproduces the benchmark winner, R\@10 ~0.90):

  query
   -> gpt-oss:20b expansion (append 5 domain keywords; cached)
   -> nomic-embed-text cosine over the corpus  +  BM25 over name(x2)+synopsis
   -> RRF fuse the two ranked lists
   -> take candidate pool N=100
   -> popularity rerank by (fused_norm + 0.3 * log1p(revdeps))
   -> top-k packages

Reads the offline index from the data dir (see tools\/build_capability_index.hs):
capability-packages.jsonl, capability-vectors.f32(+.meta.json),
capability-revdeps.json. Expansions cache to capability-expansions.json there.

Prints JSON to stdout: @[{"package":..,"synopsis":..,"score":..}]@ (top-k).
Degrades gracefully: if ollama is unreachable or any data file is missing it
prints @[]@ and exits 0 — it never crashes the caller.

Usage:
  cabal run tools/capability_search.hs -- "QUERY" [--top-k N] [--data-dir DIR]
                                                  [--no-expand]
Env: SABELA_CAPABILITY_DATA_DIR overrides the data dir.
-}
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    decodeStrict',
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    (.:),
    (.=),
 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal (toForeignPtr)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isDigit, isSpace, toLower)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Foreign.ForeignPtr (castForeignPtr, plusForeignPtr)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Simple (
    getResponseBody,
    httpJSON,
    parseRequest,
    setRequestBodyJSON,
    setRequestResponseTimeout,
 )
import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- --- config ----------------------------------------------------------------

poolN :: Int
poolN = 100

popW :: Double
popW = 0.3

rrfK :: Int
rrfK = 60

expandPrompt :: Text -> Text
expandPrompt intent =
    "List 5 short keywords or library categories a Haskell programmer would "
        <> "use to find a library for this task. Reply with ONLY a comma-separated "
        <> "list of single words or short phrases, no explanation.\nTask: "
        <> intent

getEnvDef :: String -> String -> IO String
getEnvDef k d = fromMaybe d <$> lookupEnv k

-- --- args ------------------------------------------------------------------

data Args = Args
    { aQuery :: Text
    , aTopK :: Int
    , aDataDir :: Maybe FilePath
    , aExpand :: Bool
    }

parseArgs :: [String] -> Args
parseArgs = go (Args "" 18 Nothing True)
  where
    go acc [] = acc
    go acc ("--top-k" : n : rest) = go acc{aTopK = readIntDef n (aTopK acc)} rest
    go acc ("--data-dir" : d : rest) = go acc{aDataDir = Just d} rest
    go acc ("--no-expand" : rest) = go acc{aExpand = False} rest
    go acc (q : rest)
        | T.null (aQuery acc) = go acc{aQuery = T.pack q} rest
        | otherwise = go acc rest
    readIntDef s d = maybe d fst (BC.readInt (BC.pack s))

main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    let q = T.strip (aQuery args)
    if T.null q
        then putStrLn "[]"
        else do
            dataDir <- resolveDataDir (aDataDir args)
            r <-
                try (search q (aTopK args) dataDir (aExpand args)) ::
                    IO (Either SomeException [Hit])
            case r of
                Right hits -> BL.putStr (encode hits) >> putStrLn ""
                Left e -> do
                    hPutStrLn stderr ("capability_search: degraded, returning []: " <> show e)
                    putStrLn "[]"

resolveDataDir :: Maybe FilePath -> IO FilePath
resolveDataDir (Just d) = pure d
resolveDataDir Nothing = getEnvDef "SABELA_CAPABILITY_DATA_DIR" "data"

-- --- index types -----------------------------------------------------------

data Pkg = Pkg {pName :: !Text, pSyn :: !Text}

instance FromJSON Pkg where
    parseJSON = withObject "Pkg" $ \o ->
        Pkg <$> o .: "name" <*> (fromMaybe "" <$> o .: "synopsis")

data Meta = Meta {mNames :: ![Text], mDim :: !Int}

instance FromJSON Meta where
    parseJSON = withObject "Meta" $ \o -> Meta <$> o .: "names" <*> o .: "dim"

data Hit = Hit {hPkg :: !Text, hSyn :: !Text, hScore :: !Double}

instance ToJSON Hit where
    toJSON h =
        object ["package" .= hPkg h, "synopsis" .= hSyn h, "score" .= hScore h]

newtype EmbedResp = EmbedResp [[Double]]
instance FromJSON EmbedResp where
    parseJSON = withObject "EmbedResp" $ \o -> EmbedResp <$> o .: "embeddings"

newtype GenResp = GenResp Text
instance FromJSON GenResp where
    parseJSON = withObject "GenResp" $ \o -> GenResp <$> o .: "response"

newtype ExpEntry = ExpEntry {eeKeywords :: [Text]}
instance FromJSON ExpEntry where
    parseJSON = withObject "ExpEntry" $ \o -> ExpEntry <$> o .: "keywords"
instance ToJSON ExpEntry where
    toJSON e = object ["keywords" .= eeKeywords e]

-- --- index loading ---------------------------------------------------------

data Index = Index
    { ixCorpus :: ![Pkg]
    , ixNames :: ![Text]
    , ixDim :: !Int
    , ixVecs :: !(VS.Vector Float)
    , ixRevdeps :: !(M.Map Text Int)
    , ixSyn :: !(M.Map Text Text)
    }

loadIndex :: FilePath -> IO Index
loadIndex dataDir = do
    let pkgsPath = dataDir </> "capability-packages.jsonl"
        metaPath = dataDir </> "capability-vectors.meta.json"
        vecPath = dataDir </> "capability-vectors.f32"
        revPath = dataDir </> "capability-revdeps.json"
    forM_ [pkgsPath, metaPath, vecPath, revPath] $ \p -> do
        ok <- doesFileExist p
        unless ok (ioError (userError ("missing index file: " <> p)))
    corpus <-
        mapMaybe (decodeStrict' . dropTrailingCR) . BC.lines <$> BS.readFile pkgsPath
    meta <- decodeOrFail metaPath
    revdeps <- decodeOrFail revPath
    vecs <- bsToFloats <$> BS.readFile vecPath
    pure
        Index
            { ixCorpus = corpus
            , ixNames = mNames meta
            , ixDim = mDim meta
            , ixVecs = vecs
            , ixRevdeps = revdeps
            , ixSyn = M.fromList [(pName p, pSyn p) | p <- corpus]
            }
  where
    dropTrailingCR s = if not (BS.null s) && BS.last s == 13 then BS.init s else s
    decodeOrFail p = do
        bs <- BS.readFile p
        either (ioError . userError . ((p <> ": ") <>)) pure (eitherDecodeStrict' bs)

-- | Reinterpret a little-endian float32 blob as a storable 'Float' vector.
bsToFloats :: BS.ByteString -> VS.Vector Float
bsToFloats bs =
    VS.unsafeFromForeignPtr0
        (castForeignPtr (fp `plusForeignPtr` off))
        (len `div` 4)
  where
    (fp, off, len) = toForeignPtr bs

-- --- tokenizer + BM25 ------------------------------------------------------

tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.split (not . isTok) . T.map toLower
  where
    isTok c = isAsciiLower c || isDigit c

{- | BM25 over name tokens (counted twice) + synopsis tokens, scored at query
time from the sidecar. k1=1.5, b=0.75.
-}
bm25Scores :: [Pkg] -> [Text] -> VU.Vector Double
bm25Scores corpus queryToks = VU.create $ do
    v <- VUM.replicate n 0
    forM_ queryToks $ \t -> case M.lookup t idf of
        Nothing -> pure ()
        Just tIdf -> forM_ (M.findWithDefault [] t postings) $ \(i, c) -> do
            let dl = docLen VU.! i
                denom = c + k1 * (1 - b + b * dl / avgdl)
            VUM.modify v (\cur -> cur + tIdf * (c * (k1 + 1)) / denom) i
    pure v
  where
    k1 = 1.5
    b = 0.75
    docsToks = [tokenize (pName p) <> tokenize (pName p) <> tokenize (pSyn p) | p <- corpus]
    n = length docsToks
    docLen = VU.fromListN n (map (fromIntegral . length) docsToks) :: VU.Vector Double
    avgdl = if n == 0 then 0 else VU.sum docLen / fromIntegral n
    tfs = [M.fromListWith (+) [(t, 1 :: Int) | t <- toks] | toks <- docsToks]
    df = M.fromListWith (+) [(t, 1 :: Int) | tf <- tfs, t <- M.keys tf]
    idf =
        M.map
            ( \nt -> log (1 + (fromIntegral n - fromIntegral nt + 0.5) / (fromIntegral nt + 0.5))
            )
            df
    postings =
        M.fromListWith
            (++)
            [ (t, [(i, fromIntegral c :: Double)])
            | (i, tf) <- zip [0 ..] tfs
            , (t, c) <- M.toList tf
            ]

-- --- semantic --------------------------------------------------------------

semScores :: VS.Vector Float -> Int -> Int -> [Double] -> VU.Vector Double
semScores vecs dim n q =
    VU.generate n $ \i ->
        let row = VS.slice (i * dim) dim vecs
         in realToFrac (VS.sum (VS.zipWith (*) row qf))
  where
    qf = VS.fromList (map realToFrac q) :: VS.Vector Float

-- --- ollama HTTP -----------------------------------------------------------

postJSON :: (FromJSON a) => String -> Value -> Int -> IO a
postJSON url body toSec = do
    req0 <- parseRequest ("POST " ++ url)
    let req =
            setRequestResponseTimeout (HC.responseTimeoutMicro (toSec * 1000000)) $
                setRequestBodyJSON body req0
    getResponseBody <$> httpJSON req

embedQuery :: String -> Text -> Text -> IO [Double]
embedQuery url model text = do
    EmbedResp vss <-
        postJSON
            url
            (object ["model" .= model, "input" .= ["search_query: " <> text]])
            120
    case vss of
        (v : _) ->
            let nrm = sqrt (sum (map (\x -> x * x) v))
                d = if nrm == 0 then 1 else nrm
             in pure (map (/ d) v)
        [] -> ioError (userError "empty embedding response")

genKeywords :: String -> Text -> Text -> IO Text
genKeywords url model intent = do
    GenResp resp <-
        postJSON
            url
            ( object
                [ "model" .= model
                , "prompt" .= expandPrompt intent
                , "stream" .= False
                , "options" .= object ["temperature" .= (0 :: Double)]
                ]
            )
            300
    pure resp

-- --- query expansion (cached) ----------------------------------------------

expandQuery :: String -> Text -> Text -> FilePath -> IO Text
expandQuery url model intent cachePath = do
    cache <- readCache cachePath
    kws <- case M.lookup intent cache of
        Just e -> pure (eeKeywords e)
        Nothing -> do
            ks <- parseKeywords <$> genKeywords url model intent
            writeCache cachePath (M.insert intent (ExpEntry ks) cache)
            pure ks
    pure $ if null kws then intent else intent <> " " <> T.unwords kws

readCache :: FilePath -> IO (M.Map Text ExpEntry)
readCache p = do
    ok <- doesFileExist p
    if not ok
        then pure M.empty
        else do
            r <- try (BS.readFile p) :: IO (Either SomeException BS.ByteString)
            pure $ case r of
                Right bs -> fromMaybe M.empty (decodeStrict' bs)
                Left _ -> M.empty

writeCache :: FilePath -> M.Map Text ExpEntry -> IO ()
writeCache p m = do
    r <- try (BL.writeFile p (encode m)) :: IO (Either SomeException ())
    either (const (pure ())) pure r

{- | Strip @\<think\>...\</think\>@ spans, take the last non-empty line, split on
commas\/semicolons\/newlines, clean each token, keep up to 5 short ones.
-}
parseKeywords :: Text -> [Text]
parseKeywords resp = take 5 (filter ok (map clean (T.split sep lastLine)))
  where
    cleaned = stripThink resp
    lastLine =
        case reverse (filter (not . T.null . T.strip) (T.lines cleaned)) of
            (l : _) -> T.strip l
            [] -> ""
    sep c = c == ',' || c == ';' || c == '\n'
    clean =
        T.dropAround (\c -> c == '`' || c == '\'' || c == '"')
            . T.toLower
            . T.strip
            . T.dropWhile
                (\c -> isSpace c || c == '-' || c == '*' || isDigit c || c == '.' || c == ')')
    ok t = not (T.null t) && T.length t < 40

stripThink :: Text -> Text
stripThink t =
    let (before, rest) = T.breakOn "<think>" t
     in if T.null rest
            then t
            else
                let afterOpen = T.drop (T.length "<think>") rest
                    (_, rest2) = T.breakOn "</think>" afterOpen
                 in before <> " " <> stripThink (T.drop (T.length "</think>") rest2)

-- --- fusion + rerank -------------------------------------------------------

topIndices :: VU.Vector Double -> Int -> Bool -> [Int]
topIndices scores cap requirePos =
    let ranked = take cap (sortBy (comparing (Down . snd)) (zip [0 ..] (VU.toList scores)))
     in [i | (i, s) <- ranked, not requirePos || s > 0]

shipRank ::
    VU.Vector Double ->
    VU.Vector Double ->
    [Text] ->
    M.Map Text Int ->
    Int ->
    [(Int, Double)]
shipRank bm sem names revdeps topK
    | null cand = []
    | otherwise = take topK (sortBy (comparing (Down . snd)) scored)
  where
    bmL = topIndices bm poolN True
    semL = topIndices sem poolN False
    fused = M.fromListWith (+) (rrf bmL ++ rrf semL)
    rrf lst = [(i, 1 / fromIntegral (rrfK + rank + 1)) | (rank, i) <- zip [0 ..] lst]
    cand = dedup (bmL ++ semL)
    base = [fused M.! i | i <- cand]
    lo = minimum base
    hi = maximum base
    spanv = if hi > lo then hi - lo else 1
    namesV = V.fromList names
    scored =
        [ ( i
          , norm + popW * log1p (fromIntegral (M.findWithDefault 0 (namesV V.! i) revdeps))
          )
        | i <- cand
        , let norm = if hi > lo then (fused M.! i - lo) / spanv else 1.0
        ]
    log1p x = log (1 + x)

dedup :: [Int] -> [Int]
dedup = go mempty
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs

-- --- search ----------------------------------------------------------------

search :: Text -> Int -> FilePath -> Bool -> IO [Hit]
search q topK dataDir doExpand = do
    ix <- loadIndex dataDir
    embedUrl <- getEnvDef "OLLAMA_EMBED_URL" "http://localhost:11434/api/embed"
    genUrl <- getEnvDef "OLLAMA_GEN_URL" "http://localhost:11434/api/generate"
    embedModel <- T.pack <$> getEnvDef "CAPABILITY_EMBED_MODEL" "nomic-embed-text"
    expandModel <- T.pack <$> getEnvDef "CAPABILITY_EXPAND_MODEL" "gpt-oss:20b"
    let cachePath = dataDir </> "capability-expansions.json"
    text <- if doExpand then expandQuery genUrl expandModel q cachePath else pure q
    let bm = bm25Scores (ixCorpus ix) (tokenize text)
    qv <- embedQuery embedUrl embedModel text
    let sem = semScores (ixVecs ix) (ixDim ix) (length (ixNames ix)) qv
        ranked = shipRank bm sem (ixNames ix) (ixRevdeps ix) topK
        namesV = V.fromList (ixNames ix)
    pure
        [ Hit nm (M.findWithDefault "" nm (ixSyn ix)) (round6 s)
        | (i, s) <- ranked
        , let nm = namesV V.! i
        ]

round6 :: Double -> Double
round6 x = fromIntegral (round (x * 1e6) :: Integer) / 1e6
