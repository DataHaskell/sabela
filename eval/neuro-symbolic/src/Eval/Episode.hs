{-# LANGUAGE OverloadedStrings #-}

{- | Per-episode measurement plumbing (R8.1-R8.3): the config header every saved
transcript carries, byte-identical arm-pair VOID\/saturated flagging at write
time, and the fresh-seed retry for infra-failed episodes. The report-withholding
guard lives in "Eval.ReportGuard" (run-id scoped).
-}
module Eval.Episode (
    EpisodeMeta (..),
    renderEpisodeMeta,
    parseEpisodeMeta,
    transcriptBody,
    voidPair,
    saveEpisodeIn,
    transcriptName,
    readVoidFlags,
    readNaFlags,
    readSaturatedFlags,
    voidNote,
    naNote,
    saturatedNote,
    excludeFlagged,
    classifyPair,
    discoverClassCalls,
    PairCategory (..),
    rerollSeed,
    retryFreshSeed,
    defaultToolTimeout,
) where

import Control.Monad (when)
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    removeFile,
 )
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import Eval.Applicability (
    PairCategory (..),
    classifyPair,
    discoverClassCalls,
    excludeFlagged,
    naNote,
    readNaFlags,
    readSaturatedFlags,
    readVoidFlags,
    saturatedNote,
    transcriptBody,
    voidNote,
    voidPair,
 )
import Siza.Agent.Transcript (renderTranscript)

-- | The full configuration an episode ran under (R8.1), written as a header.
data EpisodeMeta = EpisodeMeta
    { emTask :: Text
    , emArm :: Text
    , emLevers :: [(Text, Text)]
    , emSeed :: Int
    , emSeedsTried :: [Int]
    -- ^ Every seed the episode ran under, base first (R6.11 retries reroll).
    , emModel :: Text
    , emStopped :: Text
    , emFinal :: Text
    , emLint :: Text
    -- ^ The R8.4 verdict from 'Eval.TranscriptLint.lintLine'.
    , emRunId :: Text
    -- ^ The driver run this episode belongs to (fresh per launch, R8.3).
    , emCommit :: Text
    -- ^ Working-tree HEAD at driver start; empty = unsound, report withheld.
    , emBuildTime :: Text
    -- ^ Server-binary build stamp (ISO); a run predating it is stale.
    , emRunTime :: Text
    -- ^ When this episode was saved (ISO).
    , emEndpoint :: Text
    -- ^ The server base URL the episode actually drove.
    , emRelinkProbe :: Text
    {- ^ Tree-state-vs-binary probe verdict ('Eval.Provenance.relinkProbe');
    empty = never probed, an unsound measurement.
    -}
    }
    deriving (Eq, Show)

headerOpen, headerClose :: Text
headerOpen = "<!-- episode-config"
headerClose = "-->"

renderEpisodeMeta :: EpisodeMeta -> Text
renderEpisodeMeta m =
    T.unlines
        [ headerOpen
        , "task: " <> emTask m
        , "arm: " <> emArm m
        , "levers: " <> T.intercalate "," [k <> "=" <> v | (k, v) <- emLevers m]
        , "seed: " <> tshow (emSeed m)
        , "seeds-tried: " <> T.intercalate "," (map tshow (emSeedsTried m))
        , "model: " <> emModel m
        , "stopped: " <> emStopped m
        , "final: " <> oneLine (emFinal m)
        , "lint: " <> emLint m
        , "run-id: " <> emRunId m
        , "commit: " <> emCommit m
        , "build-time: " <> emBuildTime m
        , "run-time: " <> emRunTime m
        , "endpoint: " <> emEndpoint m
        , "relink-probe: " <> oneLine (emRelinkProbe m)
        , headerClose
        ]

-- | Header values are line-based; a multi-line final distils to one line.
oneLine :: Text -> Text
oneLine = T.take 200 . T.unwords . T.lines

parseEpisodeMeta :: Text -> Maybe EpisodeMeta
parseEpisodeMeta t = case T.lines t of
    (h : rest)
        | h == headerOpen ->
            fromFields (fields (takeWhile (/= headerClose) rest))
    _ -> Nothing
  where
    fields ls =
        Map.fromList [(k, T.drop 2 v) | l <- ls, let (k, v) = T.breakOn ": " l]
    fromFields f =
        EpisodeMeta
            <$> Map.lookup "task" f
            <*> Map.lookup "arm" f
            <*> (parseLevers <$> Map.lookup "levers" f)
            <*> (readIntT =<< Map.lookup "seed" f)
            <*> (mapM readIntT . splitComma =<< Map.lookup "seeds-tried" f)
            <*> Map.lookup "model" f
            <*> Map.lookup "stopped" f
            <*> Map.lookup "final" f
            <*> Map.lookup "lint" f
            <*> Just (Map.findWithDefault "" "run-id" f)
            <*> Just (Map.findWithDefault "" "commit" f)
            <*> Just (Map.findWithDefault "" "build-time" f)
            <*> Just (Map.findWithDefault "" "run-time" f)
            <*> Just (Map.findWithDefault "" "endpoint" f)
            <*> Just (Map.findWithDefault "" "relink-probe" f)
    parseLevers s
        | T.null s = []
        | otherwise =
            [(k, T.drop 1 v) | p <- T.splitOn "," s, let (k, v) = T.breakOn "=" p]
    splitComma s = if T.null s then [] else T.splitOn "," s
    readIntT = readMaybe . T.unpack

transcriptName :: Text -> Int -> Text -> String
transcriptName task seed arm =
    T.unpack task <> "-s" <> show seed <> "-" <> T.unpack arm <> ".md"

{- | Write the episode transcript with its config header, then classify the
(task, seed) pair once both arms exist: byte-identical arms are flagged
@.SATURATED@ when the discover surface answered (lever fired) and @.VOID@ when
it never did (lever dead); a search-free-on-both pair is @.NA@ (section 13);
a sound pair clears any stale flag. Returns the flag path when one is set.
-}
saveEpisodeIn :: FilePath -> EpisodeMeta -> [Value] -> IO (Maybe FilePath)
saveEpisodeIn dir meta msgs = do
    createDirectoryIfMissing True dir
    TIO.writeFile
        (dir </> transcriptName (emTask meta) (emSeed meta) (emArm meta))
        (renderEpisodeMeta meta <> renderTranscript (emTask meta) msgs)
    flagPair dir (emTask meta) (emSeed meta)

flagPair :: FilePath -> Text -> Int -> IO (Maybe FilePath)
flagPair dir task seed = do
    let offP = dir </> transcriptName task seed "off"
        onP = dir </> transcriptName task seed "on"
        stem = dir </> T.unpack task <> "-s" <> show seed
        voidF = stem <> ".VOID"
        naF = stem <> ".NA"
        satF = stem <> ".SATURATED"
    haveBoth <- (&&) <$> doesFileExist offP <*> doesFileExist onP
    if not haveBoth
        then pure Nothing
        else do
            cat <- classifyPair <$> TIO.readFile offP <*> TIO.readFile onP
            mapM_ clearStale [voidF, naF, satF]
            case cat of
                PairVoid -> Just voidF <$ TIO.writeFile voidF voidFlagText
                PairSaturated -> Just satF <$ TIO.writeFile satF saturatedFlagText
                PairNotApplicable -> Just naF <$ TIO.writeFile naF naFlagText
                PairSound -> pure Nothing

clearStale :: FilePath -> IO ()
clearStale f = do
    stale <- doesFileExist f
    when stale (removeFile f)

voidFlagText :: Text
voidFlagText =
    "byte-identical off/on transcript bodies with NO answering discover \
    \surface: the lever never fired (dead) — report this pair as VOID, never \
    \as a measurement.\n"

saturatedFlagText :: Text
saturatedFlagText =
    "lever fired: the discover surface answered, and the off/on bodies are \
    \byte-identical — the lever is SATURATED on this trajectory (post-R5-T1 \
    \the card path is mode-invariant). A decided category: excluded from \
    \lever deltas, never mislabelled VOID.\n"

naFlagText :: Text
naFlagText =
    "zero discover-class calls on both arms: a search-free task the grammar/search \
    \lever cannot move — report the axis as not-applicable, never as a measurement \
    \or a VOID.\n"

-- | Attempt @n@'s seed: the base for the first run, fresh for each retry.
rerollSeed :: Int -> Int -> Int
rerollSeed base n = base + 1000003 * n

{- | Retry a failing episode with a FRESH seed each time (R6.11): a seeded
chat failure replayed bit-identically can never succeed. Returns the last run
plus every seed tried, in order.
-}
retryFreshSeed :: Int -> Int -> (a -> Bool) -> (Int -> IO a) -> IO (a, [Int])
retryFreshSeed maxRetries base ok attempt = go 0 []
  where
    go n tried = do
        let s = rerollSeed base n
        r <- attempt s
        let tried' = tried <> [s]
        if ok r || n >= maxRetries
            then pure (r, tried')
            else go (n + 1) tried'

{- | Pin the client's per-call HTTP timeout generously (300s) when unset, so a
dep-install or kernel restart does not blow the 60s default mid-episode (M7).
Must run before @newConn@, which reads the var; returns the effective value.
-}
defaultToolTimeout :: IO Int
defaultToolTimeout = do
    m <- lookupEnv "SABELA_TOOL_TIMEOUT"
    case m of
        Just s -> pure (fromMaybe 300 (readMaybe s))
        Nothing -> setEnv "SABELA_TOOL_TIMEOUT" "300" >> pure 300

tshow :: (Show a) => a -> Text
tshow = T.pack . show
