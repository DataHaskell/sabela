{- | Run provenance for measurement integrity (R8.1/R8.3): the binary's
commit hash and build time plus a fresh run id, captured ONCE at driver
start and stamped into every transcript header, so a stale directory can
never re-grade as a fresh smoke — 'Eval.Episode.guardReport' withholds
numbers when any of these is missing or the run predates the binary.
-}
module Eval.Provenance (
    RunProvenance (..),
    captureProvenance,
    captureProvenanceChecked,
    captureProvenanceCheckedSelf,
    combinedRelink,
    relinkProbe,
    sabelaProbeRoots,
    driverProbeRoots,
    nowIso,
    isoTime,
    freshRunDirUnder,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
 )
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.Process (readProcess)

-- | What one driver run knows about the binary and itself.
data RunProvenance = RunProvenance
    { rpRunId :: Text
    , rpCommit :: Text
    , rpBuildTime :: Text
    , rpRelink :: Text
    -- ^ The relink-probe verdict; empty = never probed, an unsound header.
    }
    deriving (Eq, Show)

{- | Capture the run's provenance: the working tree's HEAD commit, the server
binary's modification time as the build stamp, and a timestamped run id.
Failures record @\"\"@, which the report guard treats as unsound (withheld) —
never a silently-passing placeholder.
-}
captureProvenance :: FilePath -> IO RunProvenance
captureProvenance binary = do
    now <- getCurrentTime
    commit <- gitHead
    buildT <- binaryBuildTime binary
    let runId = "run-" <> T.pack (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now)
    pure (RunProvenance runId commit buildT "")

{- | The drivers' pre-run gate: capture provenance AND require the relink
probe to pass, aborting loudly on a stale binary — running episodes against
an exe that embeds an old tree would stamp sound-looking headers on unsound
measurements. Records the passing verdict in 'rpRelink'.
-}
captureProvenanceChecked :: FilePath -> [FilePath] -> IO RunProvenance
captureProvenanceChecked binary roots = do
    prov <- captureProvenance binary
    r <- relinkProbe binary roots
    case r of
        Left err -> ioError (userError (T.unpack err))
        Right ok -> pure prov{rpRelink = ok}

{- | The tree the sabela server binary embeds, relative to the repo root the
drivers run from (TH-embedded static pages included; runtime-read data/ not).
-}
sabelaProbeRoots :: [FilePath]
sabelaProbeRoots = ["src", "src-contract", "app", "static", "sabela.cabal"]

{- | The trees a bench/gate driver executable itself embeds (agent loop,
eval harness, shared contract); probed against the RUNNING driver so a stale
bench cannot stamp a sound-looking header on unsound measurements.
-}
driverProbeRoots :: [FilePath]
driverProbeRoots =
    [ "eval/neuro-symbolic/src"
    , "eval/neuro-symbolic/bench"
    , "eval/neuro-symbolic/gate"
    , "eval/neuro-symbolic/siza-eval.cabal"
    , "siza-client/src"
    , "siza-client/siza-client.cabal"
    , "src-contract"
    ]

{- | 'captureProvenanceChecked' extended to the driver's own executable: the
server probe AND the self probe must both pass, and both labeled verdicts are
stamped into 'rpRelink' (the R8-T5 sequencing protocol).
-}
captureProvenanceCheckedSelf :: FilePath -> IO RunProvenance
captureProvenanceCheckedSelf serverBin = do
    prov <- captureProvenance serverBin
    self <- getExecutablePath
    r <-
        combinedRelink
            [ ("server", serverBin, sabelaProbeRoots)
            , ("driver", self, driverProbeRoots)
            ]
    case r of
        Left err -> ioError (userError (T.unpack err))
        Right ok -> pure prov{rpRelink = ok}

{- | Run 'relinkProbe' per (label, binary, roots) member: all must pass and
each verdict is labeled; the first stale member fails the whole probe.
-}
combinedRelink :: [(Text, FilePath, [FilePath])] -> IO (Either Text Text)
combinedRelink members = do
    rs <- mapM probe members
    pure (T.intercalate "; " <$> sequence rs)
  where
    probe (label, bin, roots) = do
        r <- relinkProbe bin roots
        pure (either (Left . ((label <> " ") <>)) (Right . ((label <> " ") <>)) r)

{- | Prove the binary postdates every source file under @roots@ (round-6
finding 5: @cabal build@ can skip the final link, so a "successful" build may
leave an exe embedding a stale tree). Left = stale or unprobeable, run nothing;
Right = the verdict to record in every episode header.
-}
relinkProbe :: FilePath -> [FilePath] -> IO (Either Text Text)
relinkProbe binary roots = do
    present <- doesFileExist binary
    if not present
        then pure (Left ("relink probe: binary missing: " <> T.pack binary))
        else do
            binT <- getModificationTime binary
            newest <- newestUnder roots
            pure $ case newest of
                Nothing ->
                    Right ("ok: binary " <> isoTime binT <> "; probe roots empty")
                Just (srcT, p)
                    | srcT > binT -> Left (staleMsg binT srcT p)
                    | otherwise ->
                        Right
                            ( "ok: binary "
                                <> isoTime binT
                                <> " >= newest source "
                                <> isoTime srcT
                                <> " ("
                                <> T.pack p
                                <> ")"
                            )

staleMsg :: UTCTime -> UTCTime -> FilePath -> Text
staleMsg binT srcT p =
    "relink probe FAILED: "
        <> T.pack p
        <> " ("
        <> isoTime srcT
        <> ") is newer than the binary ("
        <> isoTime binT
        <> ") — the exe embeds a stale tree; cabal build can skip the final "
        <> "link, so remove the exe and rebuild before measuring."

-- | Newest (mtime, path) under the roots; missing roots are skipped.
newestUnder :: [FilePath] -> IO (Maybe (UTCTime, FilePath))
newestUnder roots = foldl' newer Nothing . concat <$> mapM walk roots
  where
    newer acc x = case acc of
        Just a | a >= x -> acc
        _ -> Just x
    walk p = do
        isDir <- doesDirectoryExist p
        if isDir
            then do
                entries <- listDirectory p
                concat <$> mapM (walk . (p </>)) entries
            else do
                isFile <- doesFileExist p
                if isFile
                    then do
                        t <- getModificationTime p
                        pure [(t, p)]
                    else pure []

gitHead :: IO Text
gitHead = do
    r <-
        try (readProcess "git" ["rev-parse", "HEAD"] "") ::
            IO (Either SomeException String)
    pure (either (const "") (T.strip . T.pack) r)

binaryBuildTime :: FilePath -> IO Text
binaryBuildTime binary = do
    present <- doesFileExist binary
    if not present
        then pure ""
        else isoTime <$> getModificationTime binary

-- | The header time format; ISO-8601 UTC, lexicographically ordered.
isoTime :: UTCTime -> Text
isoTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

nowIso :: IO Text
nowIso = isoTime <$> getCurrentTime

{- | A fresh per-run transcript directory under @base@ (R8.3): smoke output
lands in @base/<run-id>@, so re-grading an old directory self-identifies
instead of impersonating the new run.
-}
freshRunDirUnder :: FilePath -> RunProvenance -> FilePath
freshRunDirUnder base prov = base </> T.unpack (rpRunId prov)
