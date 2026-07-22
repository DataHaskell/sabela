{-# LANGUAGE OverloadedStrings #-}

{- | R8.1 tree-state-vs-binary relink probe (round-6 finding 5): @cabal build@
can leave the exe un-relinked, so the pre-run gate must prove the binary is
newer than every source file it embeds, and the verdict must be recorded in
every episode header where the report guard can see it.
-}
module Test.RelinkProbeSpec (spec) where

import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removePathForcibly,
    setModificationTime,
 )
import System.FilePath ((</>))
import Test.Hspec

import Eval.Episode (EpisodeMeta (..), parseEpisodeMeta, renderEpisodeMeta)
import Eval.Provenance (
    RunProvenance (..),
    captureProvenanceChecked,
    combinedRelink,
    driverProbeRoots,
    relinkProbe,
 )
import Eval.ReportGuard (metaProblems)
import Test.EpisodeSpec (sampleMeta)

stamp :: Int -> Int -> UTCTime
stamp day hour =
    UTCTime
        (fromGregorian 2026 7 day)
        (secondsToDiffTime (fromIntegral hour * 3600))

-- | Lay out a binary and a source tree with controlled mtimes.
withTree :: (FilePath -> FilePath -> FilePath -> IO a) -> IO a
withTree k = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "siza-eval-relink-probe-spec"
        bin = dir </> "sabela"
        root = dir </> "src"
        srcFile = root </> "Deep" </> "Mod.hs"
    removePathForcibly dir
    createDirectoryIfMissing True (root </> "Deep")
    writeFile bin "binary"
    writeFile srcFile "module Deep.Mod where"
    k bin root srcFile

spec :: Spec
spec = describe "relink probe (tree state vs binary, round-6 finding 5)" $ do
    it "fails naming the newer source when the tree postdates the binary" $
        withTree $ \bin root srcFile -> do
            setModificationTime bin (stamp 20 10)
            setModificationTime srcFile (stamp 20 12)
            r <- relinkProbe bin [root]
            case r of
                Right ok -> expectationFailure ("probe passed: " <> T.unpack ok)
                Left err -> do
                    T.unpack err `shouldContain` "Mod.hs"
                    T.unpack err `shouldContain` "relink"

    it "passes with a verdict recording both stamps when the binary is newest" $
        withTree $ \bin root srcFile -> do
            setModificationTime srcFile (stamp 20 10)
            setModificationTime bin (stamp 20 12)
            r <- relinkProbe bin [root]
            case r of
                Left err -> expectationFailure (T.unpack err)
                Right ok -> do
                    T.unpack ok `shouldContain` "ok"
                    T.unpack ok `shouldContain` "2026-07-20T12:00:00Z"
                    T.unpack ok `shouldContain` "2026-07-20T10:00:00Z"

    it "fails on a missing binary rather than passing vacuously" $
        withTree $ \bin root _ -> do
            r <- relinkProbe (bin <> "-absent") [root]
            r `shouldSatisfy` either (const True) (const False)

    it "skips a probe root that does not exist (portable across checkouts)" $
        withTree $ \bin root _ -> do
            r <- relinkProbe bin [root </> "no-such-dir"]
            r `shouldSatisfy` either (const False) (const True)

    it "captureProvenanceChecked records the passing verdict in rpRelink" $
        withTree $ \bin root srcFile -> do
            setModificationTime srcFile (stamp 20 10)
            setModificationTime bin (stamp 20 12)
            prov <- captureProvenanceChecked bin [root]
            T.unpack (rpRelink prov) `shouldContain` "ok"

    describe "multi-binary probe (a stale driver cannot stamp a sound header)" $ do
        it "stamps one labeled verdict per (binary, roots) member" $
            withTree $ \bin root srcFile -> do
                setModificationTime srcFile (stamp 20 10)
                setModificationTime bin (stamp 20 12)
                r <- combinedRelink [("server", bin, [root]), ("driver", bin, [root])]
                case r of
                    Left err -> expectationFailure (T.unpack err)
                    Right ok -> do
                        T.unpack ok `shouldContain` "server ok"
                        T.unpack ok `shouldContain` "driver ok"
        it "fails naming the stale member when its tree postdates the binary" $
            withTree $ \bin root srcFile -> do
                let bin2 = bin <> "-driver"
                writeFile bin2 "driver binary"
                setModificationTime srcFile (stamp 20 10)
                setModificationTime bin (stamp 20 12)
                setModificationTime bin2 (stamp 20 8)
                r <- combinedRelink [("server", bin, [root]), ("driver", bin2, [root])]
                case r of
                    Right ok -> expectationFailure ("probe passed: " <> T.unpack ok)
                    Left err -> do
                        T.unpack err `shouldContain` "driver"
                        T.unpack err `shouldContain` "Mod.hs"
        it "driverProbeRoots covers the trees the bench/gate exes embed" $ do
            driverProbeRoots `shouldContain` ["siza-client/src"]
            driverProbeRoots `shouldContain` ["eval/neuro-symbolic/src"]
            driverProbeRoots `shouldContain` ["src-contract"]

    describe "header recording (R8.1)" $ do
        it "round-trips the relink-probe field through the header" $ do
            let m = sampleMeta{emRelinkProbe = "ok: binary 2026-07-20T12:00:00Z"}
            parseEpisodeMeta (renderEpisodeMeta m) `shouldBe` Just m
        it "renders a relink-probe line check-headers.sh can require" $
            renderEpisodeMeta sampleMeta{emRelinkProbe = "ok: fresh"}
                `shouldSatisfy` T.isInfixOf "\nrelink-probe: ok: fresh\n"
        it "an empty relink-probe verdict is an unsound measurement" $ do
            metaProblems sampleMeta{emRelinkProbe = ""} `shouldSatisfy` (not . null)
            metaProblems sampleMeta `shouldBe` []
