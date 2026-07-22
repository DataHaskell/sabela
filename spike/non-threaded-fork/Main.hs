{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Monad (void, when)
import Data.Word (Word64)
import GHC (Ghc, getContext, getSessionDynFlags, handleSourceError, parseName, runGhc, setSessionDynFlags)
import qualified GHC.Clock
import GHC.Driver.Monad (Ghc (..), reflectGhc)
import qualified GHC.Driver.Monad as GM
import GHC.Runtime.Context (InteractiveImport (..))
import GHC.Runtime.Eval (execOptions, execStmt, parseImportDecl, runDecls, setContext)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), Handle, hClose, hGetContents, hPutStrLn, hSetBuffering)
import qualified System.IO
import System.Posix.IO (createPipe, fdToHandle)
import System.Posix.Process (ProcessStatus (..), exitImmediately, forkProcess, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (ProcessHandle, getProcessExitCode, proc, terminateProcess, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

main :: IO ()
main = getArgs >>= \case
    [libdir, scenario] -> runGhc (Just libdir) (proof scenario)
    _ -> hPutStrLn stderrHandle "usage: runner LIBDIR SCENARIO" >> exitWith (ExitFailure 64)

proof :: String -> Ghc ()
proof scenario = do
    flags <- getSessionDynFlags
    void (setSessionDynFlags flags)
    prelude <- parseImportDecl "import Prelude"
    setContext [IIDecl prelude]
    void (runDecls "liveSeed = 41 :: Int")
    case scenario of
        "after-cell" -> void (runDecls "justCommitted = liveSeed + 1")
        _ -> pure ()
    Ghc $ \session -> withWorkload scenario $ \workload -> do
        (readFd, writeFd) <- createPipe
        readHandle <- fdToHandle readFd
        writeHandle <- fdToHandle writeFd
        hSetBuffering writeHandle LineBuffering
        started <- monotonicMicros
        pid <- forkProcess (child scenario session readHandle writeHandle)
        hClose writeHandle
        reportResult <- timeout 500000 $ do
            report <- hGetContents readHandle
            length report `seq` pure report
        when (reportResult == Nothing) (signalProcess sigKILL pid)
        hClose readHandle
        status <- getProcessStatus True False pid
        elapsed <- subtract started <$> monotonicMicros
        workloadStillRunning <-
            maybe (pure True) (fmap (== Nothing) . getProcessExitCode) workload
        parentOk <- try (reflectGhc parentCheck session) :: IO (Either SomeException ())
        putStr (maybe "child=timeout-killed\n" id reportResult)
        putStrLn ("rts_threaded=" ++ show rtsSupportsBoundThreads)
        putStrLn "child=reaped"
        putStrLn ("fork_latency_us=" ++ show elapsed)
        putStrLn ("workload_in_flight=" ++ show workloadStillRunning)
        case (scenario, status, parentOk) of
            ("execute-timeout", Just (Terminated _ _), Right ()) -> healthy
            (_, Just (Exited ExitSuccess), Right ()) -> healthy
            _ -> do
                hPutStrLn stderrHandle ("proof failed: " ++ show status ++ " " ++ either displayException (const "") parentOk)
                exitWith (ExitFailure 1)
      where
        healthy = do
                putStrLn "parent=usable"
                putStrLn "pollution=none"

child :: String -> GM.Session -> Handle -> Handle -> IO ()
child scenario session readHandle writeHandle = do
    hClose readHandle
    let action
            | scenario == "execute-timeout" = executeForever
            | otherwise = childCheck
    result <- try (reflectGhc action session) :: IO (Either SomeException ())
    case result of
        Right () -> hPutStrLn writeHandle "child=typechecked"
        Left err -> hPutStrLn writeHandle ("child=diagnostic " ++ displayException err)
    hClose writeHandle
    exitImmediately (either (const (ExitFailure 1)) (const ExitSuccess) result)

executeForever :: Ghc ()
executeForever = void (execStmt "let loop = loop in loop :: Int" execOptions)

childCheck :: Ghc ()
childCheck = do
    imp <- parseImportDecl "import qualified Data.Map.Strict as M"
    context <- getContext
    setContext (IIDecl imp : context)
    void (runDecls declarations)
  where
    declarations = unlines
        [ "data Candidate = Candidate Int deriving (Eq, Show)"
        , "class HasCandidate a where candidateValue :: a -> Int"
        , "instance HasCandidate Candidate where candidateValue (Candidate n) = n"
        , "candidateMap = M.fromList [(\"answer\", Candidate liveSeed)]"
        , "candidateAnswer = candidateValue (candidateMap M.! \"answer\")"
        ]

parentCheck :: Ghc ()
parentCheck = do
    void (runDecls "parentStillWorks = liveSeed + 1")
    leaked <-
        handleSourceError
            (const (pure False))
            (not . null <$> parseName "Candidate")
    when leaked (fail "child declaration leaked into parent")

withWorkload :: String -> (Maybe ProcessHandle -> IO a) -> IO a
withWorkload "live-busy" action =
    withCreateProcess (proc "sleep" ["2"]) $ \_ _ _ ph -> do
        result <- action (Just ph)
        terminateProcess ph
        void (waitForProcess ph)
        pure result
withWorkload _ action = action Nothing

monotonicMicros :: IO Word64
monotonicMicros = (`div` 1000) <$> GHC.Clock.getMonotonicTimeNSec

stderrHandle :: Handle
stderrHandle = System.IO.stderr
