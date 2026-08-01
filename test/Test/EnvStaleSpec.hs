{-# LANGUAGE OverloadedStrings #-}

module Test.EnvStaleSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Lifecycle (envStale, neededEnvSig)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), newApp)
import Sabela.State.SessionManager (installHaskellSession, setHaskellSession)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (inertBackend)

nbNeeding :: [Text] -> Notebook
nbNeeding deps =
    Notebook
        { nbTitle = "t"
        , nbCells =
            [ Cell
                { cellId = 0
                , cellType = CodeCell
                , cellLang = ST.Haskell
                , cellSource = "-- cabal: build-depends: " <> commas deps <> "\nx = 1"
                , cellOutputs = []
                , cellError = Nothing
                , cellDirty = False
                }
            ]
        }
  where
    commas = foldr1 (\a b -> a <> ", " <> b)

{- | What a successful rebuild leaves behind: a kernel installed, with the
environment it was built for recorded against it. Mirrors 'injectPrelude'.
-}
rebuildFor :: App -> Notebook -> IO ()
rebuildFor app nb = do
    backend <- inertBackend
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))

{- | An App with a kernel installed and its environment recorded as having been
built for @deps@ — i.e. a healthy, up-to-date session.
-}
appServing :: [Text] -> IO App
appServing deps = do
    app <- newApp "." Set.empty Nothing Nothing []
    rebuildFor app (nbNeeding deps)
    pure app

spec :: Spec
spec = describe "envStale (is the running kernel the one this notebook needs?)" $ do
    it "is stale when no kernel exists at all" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        envStale app (collectMetadata (nbNeeding ["text"])) >>= (`shouldBe` True)

    it "is fresh for the notebook the kernel was built for" $ do
        app <- appServing ["text"]
        envStale app (collectMetadata (nbNeeding ["text"])) >>= (`shouldBe` False)

    it "is stale once a dependency is added" $ do
        app <- appServing ["text"]
        envStale app (collectMetadata (nbNeeding ["text", "aeson"]))
            >>= (`shouldBe` True)

    it "is stale once a dependency is REMOVED, which the old subset test missed" $ do
        app <- appServing ["text", "aeson"]
        envStale app (collectMetadata (nbNeeding ["text"])) >>= (`shouldBe` True)

    it "is stale when the recorded kernel is not the installed one" $ do
        app <- appServing ["text"]
        replacement <- inertBackend
        setHaskellSession (appSessions app) (Just replacement)
        envStale app (collectMetadata (nbNeeding ["text"])) >>= (`shouldBe` True)

    describe "convergence: rebuilding settles, for any notebook" $ do
        let notebooks =
                [ nbNeeding ["text"]
                , nbNeeding ["text", "aeson"]
                , nbNeeding ["containers", "text", "aeson"]
                , Notebook{nbTitle = "empty", nbCells = []}
                ]
        it "recording what a rebuild produced leaves nothing to rebuild" $
            mapM_
                ( \nb -> do
                    app <- newApp "." Set.empty Nothing Nothing []
                    rebuildFor app nb
                    envStale app (collectMetadata nb) >>= (`shouldBe` False)
                )
                notebooks

        it
            "a REMOVED dependency rebuilds once and then settles: the old\
            \ tracker never recorded a shrink, so this respawned forever"
            $ do
                app <- appServing ["text", "aeson"]
                let fewer = nbNeeding ["text"]
                envStale app (collectMetadata fewer) >>= (`shouldBe` True)
                rebuildFor app fewer
                envStale app (collectMetadata fewer) >>= (`shouldBe` False)

        it "an ADDED dependency rebuilds once and then settles" $ do
            app <- appServing ["text"]
            let more = nbNeeding ["text", "aeson"]
            envStale app (collectMetadata more) >>= (`shouldBe` True)
            rebuildFor app more
            envStale app (collectMetadata more) >>= (`shouldBe` False)
