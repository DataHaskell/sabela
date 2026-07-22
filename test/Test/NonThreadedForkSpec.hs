{-# LANGUAGE NumericUnderscores #-}

module Test.NonThreadedForkSpec (spec) where

import Control.Monad (forM_)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "flagged non-threaded scratch fork feasibility" $ do
    forM_ ["idle", "after-cell", "live-busy"] $ \scenario ->
        it (scenario ++ ": child reports and exits; parent remains usable") $ do
            processEnv <- getEnvironment
            let command =
                    (proc "scripts/non-threaded-fork-spike" [scenario])
                        { env = Just (("SABELA_TYPECHECK_FORK_SCRATCH", "1") : processEnv)
                        }
            result <- timeout 30_000_000 (readCreateProcessWithExitCode command "")
            case result of
                Nothing -> expectationFailure "fork proof exceeded its hard timeout"
                Just (code, out, err) -> do
                    code `shouldBe` ExitSuccess
                    err `shouldBe` ""
                    out `shouldContain` "rts_threaded=False"
                    out `shouldContain` "child=typechecked"
                    out `shouldContain` "child=reaped"
                    out `shouldContain` "parent=usable"
                    out `shouldContain` "pollution=none"

    it "is unavailable, rather than silently running, when the flag is off" $ do
        (code, out, _) <-
            readCreateProcessWithExitCode
                (proc "scripts/non-threaded-fork-spike" ["idle"])
                ""
        code `shouldBe` ExitFailure 78
        out `shouldContain` "unavailable=flag-disabled"

    it "kills and reaps runaway throwaway execution without polluting the parent" $ do
        processEnv <- getEnvironment
        let command =
                (proc "scripts/non-threaded-fork-spike" ["execute-timeout"])
                    { env = Just (("SABELA_TYPECHECK_FORK_SCRATCH", "1") : processEnv)
                    }
        result <- timeout 30_000_000 (readCreateProcessWithExitCode command "")
        case result of
            Nothing -> expectationFailure "throwaway execution wedged"
            Just (code, out, err) -> do
                code `shouldBe` ExitSuccess
                err `shouldBe` ""
                out `shouldContain` "child=timeout-killed"
                out `shouldContain` "child=reaped"
                out `shouldContain` "parent=usable"
                out `shouldContain` "pollution=none"
