{-# LANGUAGE OverloadedStrings #-}

{- | The GHC identity probe, run once per compiler name per process: the
version string and whether it speaks JSON diagnostics, both previously
re-forked on every disposable trial.
-}
module Sabela.Session.GhcProbe (
    jsonDiagnosticsSupported,
    parseVersion,
    resolvedGhcVersion,
    versionAtLeast,
) where

import Control.Exception (SomeException, try)
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

probeCache :: IORef (M.Map String Text)
probeCache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE probeCache #-}

targetCache :: IORef (Maybe String)
targetCache = unsafePerformIO (newIORef Nothing)
{-# NOINLINE targetCache #-}

{- | The compiler cabal will actually launch: $GHC when set (the invocation
passes it as --with-compiler), else cabal's own configured compiler, else a
bare ghc. Probing anything else would describe the wrong compiler.
-}
resolvedGhcTarget :: IO String
resolvedGhcTarget = do
    cached <- readIORef targetCache
    case cached of
        Just t -> pure t
        Nothing -> do
            t <- discover
            atomicModifyIORef' targetCache (const (Just t, ()))
            pure t
  where
    discover = do
        mGhc <- lookupEnv "GHC"
        case mGhc of
            Just g | not (null g) -> pure g
            _ -> fromMaybe "ghc" <$> cabalCompilerPath
    cabalCompilerPath = do
        r <-
            try (readProcessWithExitCode "cabal" ["path", "--compiler-path"] "") ::
                IO (Either SomeException (ExitCode, String, String))
        pure $ case r of
            Right (ExitSuccess, out, _)
                | p <- T.unpack (T.strip (T.pack out)), not (null p) -> Just p
            _ -> Nothing

-- | The active compiler's numeric version, or "unknown" when unprobeable.
resolvedGhcVersion :: IO Text
resolvedGhcVersion = do
    ghc <- resolvedGhcTarget
    cached <- M.lookup ghc <$> readIORef probeCache
    case cached of
        Just v -> pure v
        Nothing -> do
            v <- probe ghc
            atomicModifyIORef' probeCache (\m -> (M.insert ghc v m, ()))
            pure v

probe :: String -> IO Text
probe ghc = do
    res <-
        try (readProcessWithExitCode ghc ["--numeric-version"] "") ::
            IO (Either SomeException (ExitCode, String, String))
    pure $ case res of
        Right (ExitSuccess, out, _) -> T.strip (T.pack out)
        _ -> "unknown"

-- | Whether the active compiler emits -fdiagnostics-as-json (GHC >= 9.10).
jsonDiagnosticsSupported :: IO Bool
jsonDiagnosticsSupported = do
    v <- resolvedGhcVersion
    pure (v /= "unknown" && versionAtLeast [9, 10] (parseVersion v))

parseVersion :: Text -> [Int]
parseVersion s =
    [ n
    | p <- T.splitOn "." (T.takeWhile (\c -> isDigit c || c == '.') s)
    , Right (n, _) <- [TR.decimal p]
    ]

versionAtLeast :: [Int] -> [Int] -> Bool
versionAtLeast req v = take (length req) (v ++ repeat 0) >= req
