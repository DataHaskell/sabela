{-# LANGUAGE OverloadedStrings #-}

{- | G2: self_heal demoted to propose-verify-disclose. Against a REAL GHCi
kernel (mirrors 'Test.CompileGateSpec'): a repair candidate never touches the
notebook before "Sabela.AI.Capabilities.Edit.RepairGate" gate-verifies it,
a dependency-only fix is disclosed as a suggestion and never applied, C4's
path-near-miss tier still commits and discloses a genuine fix, and a cell
holding a typed-hole probe is never entered by the cascade at all. Skipped
when cabal is not on PATH.
-}
module Test.RepairGateSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    findExecutable,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.Capabilities (executeTool)
import Sabela.AI.Capabilities.Edit.Cascade (executeWithRepair)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, checkedAppend, setupReactive)
import Sabela.Model
import Sabela.Server (newApp)
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..), atomicEditNotebook, freshCellId, readNotebook)

requireLiveIntegration :: IO ()
requireLiveIntegration = do
    cabal <- findExecutable "cabal"
    case cabal of
        Nothing -> pendingWith "cabal not found on PATH; skipping repair-gate integration"
        Just _ -> pure ()
    supportPresent <-
        doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
    if supportPresent
        then pure ()
        else pendingWith "sabela-notebook support source not on disk; skipping"

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

newFixture :: FilePath -> IO (App, AIStore.AIStore, ReactiveNotebook)
newFixture dir = do
    mgr <- newManager defaultManagerSettings
    app <- newApp dir Set.empty (Just mgr) Nothing [buildTimeSupportDir]
    rn <- setupReactive app
    let cfg =
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
    store <- AIStore.newAIStore cfg mgr
    pure (app, store, rn)

callTool ::
    App -> AIStore.AIStore -> ReactiveNotebook -> Text -> Value -> IO Value
callTool app store rn name input = do
    ct <- newCancelToken
    toolOutcomeValue <$> executeTool app store rn ct name input

insertSrc :: App -> AIStore.AIStore -> ReactiveNotebook -> Text -> IO Value
insertSrc app store rn src =
    callTool app store rn "insert_cell" (object ["source" .= src])

{- | Land a cell WITHOUT G1's compile gate — simulates a cell that already
compiled (or was committed by some other route) and is now failing at real
execution, so the propose-verify-disclose CASCADE (not the initial-commit
gate) can be driven directly and in isolation.
-}
bypassInsert :: App -> Text -> IO Int
bypassInsert app src = do
    nid <- freshCellId (appNotebook app)
    let cell = Cell nid CodeCell Haskell src [] Nothing True
    result <- atomicEditNotebook (appNotebook app) $ \nb ->
        case checkedAppend cell nb of
            Left v -> (nb, Left v)
            Right nb' -> (nb', Right ())
    case result of
        Right () -> pure nid
        Left _ -> error "bypassInsert: checkedAppend rejected the fixture cell"

cellSourceOf :: App -> Int -> IO (Maybe Text)
cellSourceOf app cid = fmap cellSource . lookupCell cid <$> readNotebook (appNotebook app)

spec :: Spec
spec = describe "G2 self_heal propose-verify-disclose" $ do
    it
        "a repair fixable only by a new dependency is disclosed as a suggestion, never applied"
        $ do
            requireLiveIntegration
            withSystemTempDirectory "sabela-repairgate-dep" $ \dir -> do
                (app, store, rn) <- newFixture dir
                _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"
                let brokenSrc =
                        "import qualified Data.Map.Strict as M\n\
                        \x = M.fromList [(1 :: Int, 2 :: Int)]"
                cid <- bypassInsert app brokenSrc
                ct <- newCancelToken
                (_result, suggestions, _mitigations) <- executeWithRepair app store rn cid ct

                -- The dependency fix ("containers") is offered, never committed.
                suggestions `shouldSatisfy` (not . null)
                let sugText = T.concat [T.pack (show s) | s <- suggestions]
                sugText `shouldSatisfy` T.isInfixOf "containers"

                postSrc <- cellSourceOf app cid
                postSrc `shouldBe` Just brokenSrc

    it "hole-guard: self_heal never enters a cell holding a typed-hole probe" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-repairgate-hole" $ \dir -> do
            (app, store, rn) <- newFixture dir
            _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"
            let holeSrc = "import Sabela.Notebook\nline (_ :: Point) (_ :: Point)"
            cid <- bypassInsert app holeSrc
            ct <- newCancelToken
            (_result, suggestions, _mitigations) <- executeWithRepair app store rn cid ct

            suggestions `shouldBe` []
            postSrc <- cellSourceOf app cid
            postSrc `shouldBe` Just holeSrc

    it
        "C4 path-near-miss still commits a genuine fix and discloses it under the gated cascade"
        $ do
            requireLiveIntegration
            withSystemTempDirectory "sabela-repairgate-path" $ \dir -> do
                createDirectoryIfMissing True (dir </> "examples" </> "data")
                writeFile (dir </> "examples" </> "data" </> "housing.csv") "x\n1\n"
                (app, store, rn) <- newFixture dir
                -- Compiles fine (readFile/putStrLn are always in scope), so G1's
                -- gate commits it; it fails only at runtime, on the wrong path —
                -- exactly the class of failure the post-commit cascade exists for.
                ack <-
                    insertSrc app store rn "readFile \"/examples/data/housing.csv\" >>= putStrLn"

                field "error" ack `shouldBe` Nothing
                let mCid = field "cellId" ack
                Just (Number _) <- pure mCid

                -- The gate-checked cascade can outrun the write-ack deadline
                -- (each candidate now pays a real disposable compile), so an
                -- "executing" ack settles later, via await_idle.
                execution <- case textField "status" ack of
                    Just "completed" -> pure (field "execution" ack)
                    _ -> settledExecutionFor app store rn 8 mCid

                case execution >>= field "self_heal" of
                    Just note -> do
                        textField "note" note `shouldSatisfy` (/= Nothing)
                        case field "source" note of
                            Just (String s) ->
                                s `shouldSatisfy` T.isInfixOf "./examples/data/housing.csv"
                            _ -> expectationFailure "self_heal note carries no source"
                    Nothing -> expectationFailure "expected a self_heal disclosure for the path fix"

{- | Poll @await_idle@ (bounded) until the given cell's write settles, and
return its @execution@ value.
-}
settledExecutionFor ::
    App ->
    AIStore.AIStore ->
    ReactiveNotebook ->
    Int ->
    Maybe Value ->
    IO (Maybe Value)
settledExecutionFor _ _ _ 0 _ = pure Nothing
settledExecutionFor app store rn n mCid = do
    v <- callTool app store rn "await_idle" (object [])
    case field "writes" v of
        Just (Array ws)
            | Just w <- findMaybe (matches mCid) (foldr (:) [] ws) ->
                pure (field "execution" w)
        _ -> settledExecutionFor app store rn (n - 1) mCid
  where
    matches cid w = field "cellId" w == cid
    findMaybe p = foldr (\x acc -> if p x then Just x else acc) Nothing
