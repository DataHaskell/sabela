{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.PureEvalLiveSpec (spec) where

import Control.Exception (bracket)
import qualified Data.Text as T
import Sabela.Handlers (ReplSupport (..), setupReplProject)
import Sabela.Output (displayPrelude, pureValueCap)
import Sabela.Session (Session, mkSessionConfig, runBlock)
import Sabela.Session.Process (closeSession, ghciBackend, newSession)
import Sabela.Session.Query (captureBindingsBaseline, queryBindings)
import qualified Sabela.SessionTypes as ST
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "atomic pure live evaluation"
    $ it
        "pins compiler admission, state preservation, timeout recovery, and output bounds"
    $ withPureSession
    $ \dir sess -> do
        _ <- runBlock sess "let liveSeed = [40 :: Int, 2]"
        _ <- runBlock sess "type AliasIO = IO ()"
        _ <- runBlock sess "let _sabelaTryValue = 73 :: Int"
        _ <- runBlock sess "314 :: Int"
        bindingsBefore <- queryBindings sess
        let backend = ghciBackend sess

        good <- runPure backend 5_000_000 "sum liveSeed"
        ST.pureEvalVerdict good `shouldBe` ST.PureEvalSucceeded
        ST.pureEvalOutput good `shouldBe` "42"
        ST.pureEvalInferredType good `shouldSatisfy` T.isInfixOf "Int"
        assertUnchanged good

        let sentinel = dir </> "pure-live-must-not-write"
            directExpr = "writeFile " <> quoted sentinel <> " \"polluted\""
        _ <- runBlock sess "let _sabelaPureWitness x = x"
        _ <-
            runBlock
                sess
                ( "let _sabelaEvalPure _ = writeFile "
                    <> quoted sentinel
                    <> " \"shadow helper ran\" >> pure \"\""
                )
        direct <- runPure backend 5_000_000 directExpr
        qualified <- runPure backend 5_000_000 "Prelude.putStrLn \"polluted\""
        synonym <-
            runPure
                backend
                5_000_000
                "(Prelude.putStrLn \"polluted\" :: AliasIO)"
        mapM_ assertIORejected [direct, qualified, synonym]
        doesFileExist sentinel `shouldReturn` False
        preserved <- runPure backend 5_000_000 "_sabelaTryValue"
        ST.pureEvalVerdict preserved `shouldBe` ST.PureEvalSucceeded
        ST.pureEvalOutput preserved `shouldBe` "73"
        assertUnchanged preserved

        injected <- runPure backend 5_000_000 "1\n:quit"
        ST.pureEvalVerdict injected `shouldBe` ST.PureEvalRejected
        assertUnchanged injected

        capped <- runPure backend 5_000_000 "replicate 10000 'x'"
        ST.pureEvalVerdict capped `shouldBe` ST.PureEvalSucceeded
        ST.pureEvalOutput capped `shouldSatisfy` T.isSuffixOf "...(truncated)"
        T.length (ST.pureEvalOutput capped) `shouldSatisfy` (<= pureValueCap + 20)
        assertUnchanged capped

        diverged <- runPure backend 250_000 "length ([1..] :: [Integer])"
        ST.pureEvalVerdict diverged `shouldBe` ST.PureEvalTimedOut
        ST.pureEvalRecovery diverged `shouldBe` ST.PureEvalInterrupted
        assertUnchanged diverged

        recovered <- runPure backend 5_000_000 "21 * 2"
        recovered
            `shouldSatisfy` ((== ST.PureEvalSucceeded) . ST.pureEvalVerdict)
        ST.pureEvalOutput recovered `shouldBe` "42"
        itProbe <- runPure backend 5_000_000 "it"
        ST.pureEvalOutput itProbe `shouldBe` "314"
        assertUnchanged itProbe
        queryBindings sess `shouldReturn` bindingsBefore

assertIORejected :: ST.PureEvalResult -> Expectation
assertIORejected result = do
    ST.pureEvalVerdict result `shouldBe` ST.PureEvalRejected
    ST.pureEvalInferredType result `shouldSatisfy` T.isInfixOf "IO"
    ST.pureEvalError result `shouldSatisfy` T.isInfixOf "scratch candidate is IO"
    assertUnchanged result

assertUnchanged :: ST.PureEvalResult -> Expectation
assertUnchanged result = do
    ST.pureEvalBindingsUnchanged result `shouldBe` True
    ST.pureEvalItUnchanged result `shouldBe` True

runPure :: ST.SessionBackend -> Int -> T.Text -> IO ST.PureEvalResult
runPure backend budget expression = do
    gen <- ST.sbSessionGen backend
    ST.sbEvalPureLive
        backend
        ST.PureEvalRequest
            { ST.pureEvalExpectedGeneration = gen
            , ST.pureEvalTimeoutUs = budget
            , ST.pureEvalExpression = expression
            }

quoted :: FilePath -> T.Text
quoted = T.pack . show

withPureSession :: (FilePath -> Session -> IO a) -> IO a
withPureSession action =
    withSystemTempDirectory "sabela-pure-live" $ \dir -> do
        setupReplProject BareRepl [] dir emptyMeta
        cfg <- mkSessionConfig dir dir
        bracket
            (bounded 60_000_000 (newSession cfg))
            (bounded 15_000_000 . closeSession)
            ( \sess -> do
                (_, err) <- bounded 30_000_000 (runBlock sess displayPrelude)
                T.strip err `shouldBe` ""
                captureBindingsBaseline sess
                action dir sess
            )

bounded :: Int -> IO a -> IO a
bounded usec action = do
    result <- timeout usec action
    case result of
        Nothing -> expectationFailure "pure-live integration timed out" >> error "unreachable"
        Just value -> pure value

emptyMeta :: CabalMeta
emptyMeta =
    CabalMeta
        { metaDeps = []
        , metaExts = []
        , metaGhcOptions = []
        , metaExtraLibDirs = []
        , metaExtraIncludeDirs = []
        , metaPackages = []
        , metaSourceRepos = []
        , metaUnknownKeys = []
        }
