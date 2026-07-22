{- | Cold-start package resets: a declarative task→packages table plus a
best-effort cabal-store purge, so an out-of-distribution install task
measures a genuinely cold build every episode. Never fails the episode.
-}
module Eval.ColdStart (
    coldStartPackages,
    storeBuildDirsFor,
    purgeColdStart,
    purgeStoreIn,
) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM, unless)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (
    doesDirectoryExist,
    listDirectory,
    removePathForcibly,
 )
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Task-id → packages the episode must build cold (data, not control flow).
coldStartPackages :: Text -> [Text]
coldStartPackages tid = fromMaybe [] (lookup tid table)
  where
    table = [("hggScatter", ["hgg"])]

-- | Store entries that are builds of exactly @pkg@: @pkg-\<digit\>…@.
storeBuildDirsFor :: Text -> [FilePath] -> [FilePath]
storeBuildDirsFor pkg = filter isBuild
  where
    prefix = T.unpack pkg <> "-"
    isBuild d =
        prefix `isPrefixOf` d && case drop (length prefix) d of
            c : _ -> isDigit c
            [] -> False

-- | Purge every cold-start package for @tid@ from the real cabal store.
purgeColdStart :: Text -> IO [FilePath]
purgeColdStart tid = case coldStartPackages tid of
    [] -> pure []
    pkgs -> do
        mroot <- storeRoot
        case mroot of
            Nothing -> pure []
            Just root -> concat <$> mapM (purgeStoreIn root) pkgs

{- | Remove @pkg@'s builds under every @ghc-*@ dir of @root@, unregistering
from that dir's package.db first (failures tolerated: purge is best-effort).
-}
purgeStoreIn :: FilePath -> Text -> IO [FilePath]
purgeStoreIn root pkg = do
    exists <- doesDirectoryExist root
    if not exists
        then pure []
        else do
            entries <- listDirectory root
            ghcDirs <-
                filterM
                    (doesDirectoryExist . (root </>))
                    (filter ("ghc-" `isPrefixOf`) entries)
            fmap concat . forM ghcDirs $ \g -> do
                let dir = root </> g
                victims <- storeBuildDirsFor pkg <$> listDirectory dir
                unless (null victims) (unregister (dir </> "package.db") pkg)
                forM victims $ \v -> do
                    removePathForcibly (dir </> v)
                    pure (dir </> v)

-- | @ghc-pkg unregister@ against a store db; absence and failure are fine.
unregister :: FilePath -> Text -> IO ()
unregister db pkg = do
    r <-
        try
            ( readProcessWithExitCode
                "ghc-pkg"
                ["--package-db", db, "unregister", "--force", T.unpack pkg]
                ""
            ) ::
            IO (Either SomeException (ExitCode, String, String))
    _ <- pure r
    pure ()

-- | The real cabal store root, or Nothing when cabal is unavailable.
storeRoot :: IO (Maybe FilePath)
storeRoot = do
    r <-
        try (readProcessWithExitCode "cabal" ["path", "-v0", "--store-dir"] "") ::
            IO (Either SomeException (ExitCode, String, String))
    pure $ case r of
        Right (_, out, _) | not (null (trim out)) -> Just (trim out)
        _ -> Nothing
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
