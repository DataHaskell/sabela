{-# LANGUAGE OverloadedStrings #-}

{- | R4-T2 world-change grid (search-api.md R1.3/R1.4): the end-to-end
three-install-state x two-event property. An in-session install event
(dep-declaring write Succeeded, or kernel restart) routes through the ledger's
world-change invalidation, so every subsequent hit's install state matches the
post-install catalogue ground truth, and no pre-install fact is denied without
an announced change.
-}
module Test.DiscoverWorldChangeSpec (discoverWorldChangeSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Discover.Request (discoverQuery)
import Test.CatalogueSim (SimWorld (..), runWorldArgs)
import Test.DiscoverFixtures (
    SynPkg (..),
    hitText,
    hitsOf,
    installNamesFile,
    stateOf,
    synHoogle,
    textField,
 )

-- | Post-install world: everything the universe knows is session-visible.
postWorld :: SimWorld
postWorld = SimWorld (map unhide synHoogle) (map unhide synHoogle)
  where
    unhide p = p{spHidden = False}

-- | Pre-install: nimbus uninstalled, cumulus hidden (per 'synHoogle').
preWorld :: SimWorld
preWorld = SimWorld [p | p <- synHoogle, spName p /= "nimbus"] synHoogle

-- | Dispatch: discover runs against the current world; events flip it.
worldDispatch :: IORef SimWorld -> ToolCall -> IO (Either Text ToolOutcome)
worldDispatch ref tc = case tcName tc of
    "discover" -> do
        w <- readIORef ref
        let q = fromMaybe "" (discoverQuery (tcName tc) (tcArgs tc))
        out <- runWorldArgs w q (tcArgs tc)
        pure (Right (ToolOk out))
    "kernel_restart" -> do
        writeIORef ref postWorld
        pure (Right (ToolOk (object [])))
    "insert_cell" -> do
        writeIORef ref postWorld
        pure (Right (ToolOk (object ["execution" .= object ["ok" .= True]])))
    n -> pure (Left ("unexpected tool " <> n))

-- | The install event of each flavour (dep-declaring write, restart).
events :: [(String, ToolCall)]
events =
    [
        ( "dep-install"
        , ToolCall
            "insert_cell"
            (object ["source" .= ("-- cabal: build-depends: cumulus" :: Text)])
        )
    , ("restart", ToolCall "kernel_restart" (object []))
    ]

-- | (query, pre-event ground truth) per install state of the catalogue.
stateGrid :: [(Text, Text)]
stateGrid =
    [ ("gust", "installed")
    , ("bars", "hidden")
    , ("drizzle", "absent-known")
    ]

discoverCall :: Text -> ToolCall
discoverCall q = ToolCall "discover" (object ["query" .= q])

installOfTop :: Text -> Value -> Text
installOfTop name v = case [h | h <- hitsOf v, hitText "name" h == name] of
    (h : _) -> hitText "install" h
    [] -> ""

discoverWorldChangeSpec :: Spec
discoverWorldChangeSpec =
    describe "world-change grid: three states x two events (R1.3/R1.4)" $
        forM_ events $ \(evName, evCall) ->
            forM_ stateGrid $ \(q, preState) ->
                it (label evName q) $ do
                    installNamesFile
                    ref <- newIORef preWorld
                    ledger <- newSearchLedger
                    let disp = guardDiscover ledger (worldDispatch ref)
                    Right (ToolOk pre) <- disp (discoverCall q)
                    stateOf pre `shouldBe` "found"
                    installOfTop q pre `shouldBe` preState
                    _ <- disp evCall
                    Right (ToolOk post) <- disp (discoverCall q)
                    stateOf post `shouldBe` "found"
                    installOfTop q post `shouldBe` "installed"
                    textField "worldChange" post `shouldSatisfy` (not . T.null)
  where
    label evName q =
        "after " <> evName <> ", '" <> T.unpack q <> "' reads post-install truth"
