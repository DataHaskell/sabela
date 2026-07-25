{-# LANGUAGE OverloadedStrings #-}

{- | G3 task 4 against a REAL kernel and cabal build (same fixture discipline
as 'Test.CompileGateSpec'): a hole-bearing candidate the MODEL submits to
@try@ is admitted as typecheck-only — the compiler's fits come back as an
answer, the notebook is untouched, and nothing the candidate would have
printed is ever printed, because nothing is evaluated. Skipped when cabal is
not on PATH.
-}
module Test.TryHoleProbeSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, setupReactive)
import Sabela.Model (Notebook (..))
import Sabela.Server (newApp)
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.State (App (..), forceResetAllSessions, readNotebook)
import Sabela.State.EventBus (EventBus (..))

requireLiveIntegration :: IO ()
requireLiveIntegration = do
    cabal <- findExecutable "cabal"
    case cabal of
        Nothing -> pendingWith "cabal not found on PATH; skipping try hole-probe integration"
        Just _ -> pure ()
    supportPresent <- doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
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

{- | Run one test against a fresh fixture in its own temp dir, releasing the
kernel afterwards so a leaked GHCi cannot hold its nursery for the rest of
the suite.
-}
withFixture ::
    String -> ((App, AIStore.AIStore, ReactiveNotebook) -> IO a) -> IO a
withFixture label action =
    withSystemTempDirectory label $ \dir ->
        bracket (newFixture dir) releaseFixture action

releaseFixture :: (App, AIStore.AIStore, ReactiveNotebook) -> IO ()
releaseFixture (app, _, _) = forceResetAllSessions (appSessions app)

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

tryCode :: App -> AIStore.AIStore -> ReactiveNotebook -> Text -> IO Value
tryCode app store rn src = do
    ct <- newCancelToken
    toolOutcomeValue
        <$> executeTool app store rn ct "try" (object ["code" .= src])

cellCount :: App -> IO Int
cellCount app = length . nbCells <$> readNotebook (appNotebook app)

generationOf :: App -> IO Int
generationOf app = readIORef (ebGeneration (appEvents app))

{- | A hole beside an effect that WOULD announce itself if anything ran. The
hole makes the candidate uncompilable, so the effect is unreachable — the
probe's answer must show that, not the marker.
-}
sideEffectingProbe :: Text
sideEffectingProbe =
    "sabelaProbe = (putStrLn \"SABELA_HOLE_PROBE_RAN\" >> print (_ :: Int))"

spec :: Spec
spec = describe "G3 try admits a hole-bearing candidate as typecheck-only" $ do
    it "returns the compiler's fits as an answer and evaluates nothing" $ do
        requireLiveIntegration
        withFixture "sabela-try-holeprobe" $ \(app, store, rn) -> do
            countBefore <- cellCount app
            genBefore <- generationOf app

            out <- tryCode app store rn sideEffectingProbe

            textField "route" out `shouldBe` Just "typecheck_only"
            textField "verdict" out `shouldBe` Just "ok"
            textField "outcome" out `shouldBe` Just "hole_fits"
            field "evaluated" out `shouldBe` Just (Bool False)
            field "error" out `shouldBe` Nothing

            -- The answer names the goal type; the producers list is whatever
            -- GHC reported for it, stated plainly either way.
            case field "answer" out of
                Just (Array answers) -> do
                    length answers `shouldSatisfy` (> 0)
                    T.concat [a | String a <- foldr (:) [] answers]
                        `shouldSatisfy` T.isInfixOf "Int"
                _ -> expectationFailure "expected an answer array"

            -- Nothing ran: the marker the candidate would have printed never
            -- reaches stdout (the diagnostic quotes the source, which is the
            -- compiler echoing it, not the candidate running).
            textField "stdout" out `shouldBe` Just ""
            textField "diagnostic" out `shouldSatisfy` maybe False (T.isInfixOf "Found hole")

            countAfter <- cellCount app
            genAfter <- generationOf app
            countAfter `shouldBe` countBefore
            genAfter `shouldBe` genBefore

    it "no nudge is needed to get producers: the fits ride the answer" $ do
        requireLiveIntegration
        withFixture "sabela-try-holeprobe-fits" $ \(app, store, rn) -> do
            out <- tryCode app store rn "sabelaProbe = (max (_ :: Int) (_ :: Int))"
            textField "outcome" out `shouldBe` Just "hole_fits"
            case field "holeFits" out of
                Just (Array fits) -> length fits `shouldSatisfy` (> 0)
                _ -> expectationFailure "expected a holeFits array"
