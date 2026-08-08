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
import Data.List (foldl')
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

data RunProvenance = RunProvenance
    { rpRunId :: Text
    , rpCommit :: Text
    , rpBuildTime :: Text
    , rpRelink :: Text
    }
    deriving (Eq, Show)

captureProvenance :: FilePath -> IO RunProvenance
captureProvenance binary = do
    now <- getCurrentTime
    commit <- gitHead
    buildT <- binaryBuildTime binary
    let runId = "run-" <> T.pack (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now)
    pure (RunProvenance runId commit buildT "")

captureProvenanceChecked :: FilePath -> [FilePath] -> IO RunProvenance
captureProvenanceChecked binary roots = do
    prov <- captureProvenance binary
    r <- relinkProbe binary roots
    case r of
        Left err -> ioError (userError (T.unpack err))
        Right ok -> pure prov{rpRelink = ok}

sabelaProbeRoots :: [FilePath]
sabelaProbeRoots = ["src", "src-contract", "app", "static", "sabela.cabal"]

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

combinedRelink :: [(Text, FilePath, [FilePath])] -> IO (Either Text Text)
combinedRelink members = do
    rs <- mapM probe members
    pure (T.intercalate "; " <$> sequence rs)
  where
    probe (label, bin, roots) = do
        r <- relinkProbe bin roots
        pure (either (Left . ((label <> " ") <>)) (Right . ((label <> " ") <>)) r)

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

isoTime :: UTCTime -> Text
isoTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

nowIso :: IO Text
nowIso = isoTime <$> getCurrentTime

freshRunDirUnder :: FilePath -> RunProvenance -> FilePath
freshRunDirUnder base prov = base </> T.unpack (rpRunId prov)
