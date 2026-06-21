{-# LANGUAGE OverloadedStrings #-}

{- | Pins the env-cache property: 'setupReplProject' must not rewrite an unchanged
'cabal.project'/'.cabal' (so cabal skips the dependency re-solve on a restart), and
must rewrite when the metadata changes.
-}
module Test.ReplProjectSpec (spec) where

import Sabela.Session.Project (
    ReplSupport (..),
    setupReplProject,
    writeFileIfChanged,
 )
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (getModificationTime)
import System.FilePath ((</>))
import System.IO (readFile')
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

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

spec :: Spec
spec = describe "repl-project env caching" $ do
    describe "writeFileIfChanged" $ do
        it "writes new, skips an identical rewrite, writes on change" $
            withSystemTempDirectory "rp" $ \d -> do
                let p = d </> "f"
                a <- writeFileIfChanged p "x"
                b <- writeFileIfChanged p "x"
                c <- writeFileIfChanged p "y"
                (a, b, c) `shouldBe` (True, False, True)
        it "leaves mtime untouched on an identical rewrite (skips the write)" $
            withSystemTempDirectory "rp" $ \d -> do
                let p = d </> "f"
                _ <- writeFileIfChanged p "stable"
                t1 <- getModificationTime p
                _ <- writeFileIfChanged p "stable"
                t2 <- getModificationTime p
                t2 `shouldBe` t1

    describe "setupReplProject" $ do
        it "does not rewrite cabal.project / .cabal across identical runs" $
            withSystemTempDirectory "rp" $ \d -> do
                setupReplProject BareRepl [] d emptyMeta
                tp1 <- getModificationTime (d </> "cabal.project")
                tc1 <- getModificationTime (d </> "sabela-repl.cabal")
                setupReplProject BareRepl [] d emptyMeta
                tp2 <- getModificationTime (d </> "cabal.project")
                tc2 <- getModificationTime (d </> "sabela-repl.cabal")
                (tp2, tc2) `shouldBe` (tp1, tc1)

        it "rewrites sabela-repl.cabal when the deps change" $
            withSystemTempDirectory "rp" $ \d -> do
                setupReplProject BareRepl [] d emptyMeta
                before <- readFile' (d </> "sabela-repl.cabal")
                setupReplProject BareRepl [] d emptyMeta{metaDeps = ["containers"]}
                after <- readFile' (d </> "sabela-repl.cabal")
                after `shouldNotBe` before
