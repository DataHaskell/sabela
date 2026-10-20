{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | @reactivity.md@ against a real kernel: the planner's order is fed to GHCi
cell by cell, as the handlers do, and the outputs are checked for the promised
semantics — suspension after an upstream failure, no ghost types, flagged holes.
-}
module Test.ReactivityLiveSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Api (RunResult (..))
import Sabela.Handlers (ReplSupport (..), setupReplProject)
import Sabela.Handlers.Exec (cellBody, parseCellResult)
import Sabela.Model (Cell (..), OutputItem (..))
import Sabela.Output (displayPrelude, incompletePatternsAsErrors)
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeExecutionPlan,
    computeFullExecutionPlan,
 )
import Sabela.Reactivity.Upstream (failedUpstream, upstreamFailureMsg)
import Sabela.Session (Session, mkSessionConfig, runBlock)
import Sabela.Session.Process (closeSession, ghciBackend, newSession)
import qualified Sabela.SessionTypes as ST
import ScriptHs.Parser (CabalMeta (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.CellFixture (mkCell)
import Test.Hspec
import Test.ReactivityDoc.Fixture (nbOf)

spec :: Spec
spec = describe "reactivity.md live" $ do
    describe "upstream failures suspend their consumers" $ do
        let cells =
                [ mkCell 1 "x :: Int\nx = 1"
                , mkCell 2 "y :: Int\ny = x + 1"
                , mkCell 3 "z = y * 2"
                , mkCell 4 "print z"
                ]
            broken = setSource 2 "y :: Int\ny = x + \"string\"" cells
        it "the healthy chain prints the product" $ withLiveSession $ \sess -> do
            rs <- runAll sess cells
            mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
            outputOf 4 rs `shouldBe` "4"
        it "once y breaks, z does not evaluate against y's stale binding" $
            withLiveSession $ \sess -> do
                _ <- runAll sess cells
                rs <- runEdit sess broken 2
                rrError (fst (resultOf 2 rs)) `shouldSatisfy` isJust
                do
                    rrError (fst (resultOf 3 rs)) `shouldSatisfy` isJust
                    outputOf 4 rs `shouldNotBe` "4"
        it "fixing y resumes the chain" $ withLiveSession $ \sess -> do
            _ <- runAll sess cells
            _ <- runEdit sess broken 2
            rs <- runEdit sess (setSource 2 "y :: Int\ny = x + 20" broken) 2
            mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
            outputOf 4 rs `shouldBe` "42"

    describe "ghost type cleansing" $ do
        let positional =
                [ mkCell 1 "data Config = Config { port :: Int }"
                , mkCell 2 "cfg = Config 80"
                , mkCell 3 "getPort (Config p) = p"
                , mkCell 4 "v = getPort cfg"
                , mkCell 5 "print v"
                ]
            widened = setSource 1 "data Config = Config { port :: Integer }"
            recordSyntax = setSource 2 "cfg = Config { port = 80 }" positional
        it "a structural change re-binds every holder of the old type" $
            withLiveSession $ \sess -> do
                _ <- runAll sess positional
                rs <- runEdit sess (widened positional) 1
                map (rrCellId . fst) rs `shouldBe` [1, 2, 3, 4, 5]
                mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
                outputOf 5 rs `shouldBe` "80"
        it "a record-syntax holder of the old type is purged too" $
            withLiveSession $ \sess -> do
                _ <- runAll sess recordSyntax
                rs <- runEdit sess (widened recordSyntax) 1
                do
                    map (rrCellId . fst) rs `shouldBe` [1, 2, 3, 4, 5]
                    mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
                    outputOf 5 rs `shouldBe` "80"

    describe "exhaustiveness invalidation" $ do
        let cells =
                [ mkCell 1 ":set -XGADTs\ndata Expr a where\n  Lit :: Int -> Expr Int"
                , mkCell 2 "eval :: Expr a -> a\neval (Lit n) = n"
                , mkCell 3 "r = eval (Lit 3)"
                , mkCell 4 "print r"
                ]
            grown =
                setSource
                    1
                    ":set -XGADTs\ndata Expr a where\n\
                    \  Lit :: Int -> Expr Int\n\
                    \  Add :: Expr Int -> Expr Int -> Expr Int"
                    cells
        it "the GADT, its evaluator and its users evaluate" $ withLiveSession $ \sess -> do
            rs <- runAll sess cells
            mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
            outputOf 4 rs `shouldBe` "3"
        it "a LANGUAGE pragma inside the cell enables the GADT syntax too" $
            withLiveSession $ \sess -> do
                rs <-
                    runAll
                        sess
                        ( setSource
                            1
                            "{-# LANGUAGE GADTs #-}\ndata Expr a where\n  Lit :: Int -> Expr Int"
                            cells
                        )
                mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
                outputOf 4 rs `shouldBe` "3"
        it "adding a constructor flags the now-partial match and suspends" $
            withLiveSession $ \sess -> do
                _ <- runAll sess cells
                rs <- runEdit sess grown 1
                map (rrCellId . fst) rs `shouldBe` [1, 2, 3, 4]
                snd (resultOf 2 rs) `shouldSatisfy` mentionsNonExhaustive
                rrError (fst (resultOf 2 rs)) `shouldSatisfy` isJust
                rrError (fst (resultOf 3 rs)) `shouldSatisfy` isJust
                rrError (fst (resultOf 4 rs)) `shouldSatisfy` isJust

    describe "class structural changes" $ do
        let cells =
                [ mkCell 1 "class Shape a where\n  area :: a -> Double"
                , mkCell 2 "data Sq = Sq Double"
                , mkCell 3 "instance Shape Sq where\n  area (Sq s) = s * s"
                , mkCell 4 "total = area (Sq 2)"
                , mkCell 5 "print total"
                ]
            grown =
                setSource
                    3
                    "instance Shape Sq where\n  area (Sq s) = s * s\n  perimeter (Sq s) = 4 * s"
                    $ setSource 4 "total = area (Sq 2) + perimeter (Sq 2)"
                    $ setSource
                        1
                        "class Shape a where\n  area :: a -> Double\n  perimeter :: a -> Double"
                        cells
        it "the original class, instance and user evaluate" $ withLiveSession $ \sess -> do
            rs <- runAll sess cells
            mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
            outputOf 5 rs `shouldBe` "4.0"
        it "adding a method rebuilds the instance and the user" $ withLiveSession $ \sess -> do
            _ <- runAll sess cells
            rs <- runEdit sess grown 1
            map (rrCellId . fst) rs `shouldBe` [1, 3, 4, 5]
            mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
            outputOf 5 rs `shouldBe` "12.0"

    describe "instance updates" $ do
        let cells =
                [ mkCell 1 "data Color = Red | Green"
                , mkCell
                    2
                    "instance Show Color where\n  show Red = \"Stop\"\n  show Green = \"Go\""
                , mkCell 3 "label = show Red"
                , mkCell 4 "putStrLn label"
                ]
            halted =
                setSource
                    2
                    "instance Show Color where\n  show Red = \"Halt\"\n  show Green = \"Go\""
                    cells
        it "the kernel accepts a redefined instance" $ withLiveSession $ \sess -> do
            _ <- runAll sess cells
            rs <- mapM (runCell sess) (cellsOf halted [2, 3, 4])
            mapM_ (shouldSatisfy' isNothing . rrError . fst) rs
            outputOf 4 rs `shouldBe` "Halt"
        it "editing the instance re-evaluates the cell using its dictionary" $
            withLiveSession $ \sess -> do
                _ <- runAll sess cells
                rs <- runEdit sess halted 2
                outputOf 4 rs `shouldBe` "Halt"

mentionsNonExhaustive :: Text -> Bool
mentionsNonExhaustive = T.isInfixOf "non-exhaustive" . T.toLower

setSource :: Int -> Text -> [Cell] -> [Cell]
setSource cid src = map (\c -> if cellId c == cid then c{cellSource = src} else c)

cellsOf :: [Cell] -> [Int] -> [Cell]
cellsOf cells ids = [c | i <- ids, c <- cells, cellId c == i]

resultOf :: Int -> [(RunResult, Text)] -> (RunResult, Text)
resultOf cid rs = case [r | r <- rs, rrCellId (fst r) == cid] of
    (r : _) -> r
    [] -> error ("cell " ++ show cid ++ " was not run")

outputOf :: Int -> [(RunResult, Text)] -> Text
outputOf cid rs =
    T.strip (T.concat [outputText o | o <- rrOutputs (fst (resultOf cid rs))])
  where
    outputText (OutputItem _ t) = t

shouldSatisfy' :: (a -> Bool) -> a -> Expectation
shouldSatisfy' p x = p x `shouldBe` True

runAll :: Session -> [Cell] -> IO [(RunResult, Text)]
runAll sess cells = runPlan sess (computeFullExecutionPlan cells (nbOf cells))

runEdit :: Session -> [Cell] -> Int -> IO [(RunResult, Text)]
runEdit sess cells cid = runPlan sess (computeExecutionPlan cid cells (nbOf cells))

-- | Plan order through the kernel, suspending consumers of a failed cell as the runner does.
runPlan :: Session -> ExecutionPlan -> IO [(RunResult, Text)]
runPlan sess plan = reverse . snd <$> foldM step (S.empty, []) (epCellsToRun plan)
  where
    step (failed, acc) cell =
        case failedUpstream (epDeps plan) failed (cellId cell) of
            Just up -> do
                let msg = upstreamFailureMsg (epCellPositions plan) up
                    suspended = RunResult (cellId cell) [] (Just msg) []
                pure (S.insert (cellId cell) failed, (suspended, "") : acc)
            Nothing -> do
                r@(result, _) <- runCell sess cell
                let failed' = if isJust (rrError result) then S.insert (cellId cell) failed else failed
                pure (failed', r : acc)

-- | One cell through the kernel exactly as 'execCellWith' sends it.
runCell :: Session -> Cell -> IO (RunResult, Text)
runCell sess cell = do
    (out, err) <-
        bounded 30_000_000 (runBlock sess (cellBody (cellId cell) (cellSource cell)))
    let jsonOn = ST.sbJsonDiagnostics (ghciBackend sess)
    (result, _) <- parseCellResult jsonOn (cellId cell) out err
    pure (result, err)

withLiveSession :: (Session -> IO a) -> IO a
withLiveSession action =
    withSystemTempDirectory "sabela-reactivity-live" $ \dir -> do
        setupReplProject BareRepl [] dir emptyMeta
        cfg <- mkSessionConfig dir dir
        bracket
            (bounded 60_000_000 (newSession cfg))
            (bounded 15_000_000 . closeSession)
            ( \sess -> do
                (_, err) <- bounded 30_000_000 (runBlock sess displayPrelude)
                T.strip err `shouldBe` ""
                _ <- bounded 30_000_000 (runBlock sess incompletePatternsAsErrors)
                action sess
            )

bounded :: Int -> IO a -> IO a
bounded usec action = do
    result <- timeout usec action
    case result of
        Nothing -> expectationFailure "reactivity live spec timed out" >> error "unreachable"
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
