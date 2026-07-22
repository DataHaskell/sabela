{-# LANGUAGE OverloadedStrings #-}

{- | Cold-start purge: an OOD install task must measure a genuinely cold
store every episode, and the purge must never touch neighbouring packages.
-}
module Test.ColdStartSpec (spec) where

import qualified Data.Text as T
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    getTemporaryDirectory,
    removePathForcibly,
 )
import System.FilePath ((</>))
import Test.Hspec

import Eval.ColdStart (coldStartPackages, purgeStoreIn, storeBuildDirsFor)

spec :: Spec
spec = describe "Eval.ColdStart" $ do
    describe "coldStartPackages (declarative table)" $ do
        it "hggScatter cold-starts exactly hgg" $
            coldStartPackages "hggScatter" `shouldBe` ["hgg"]

        it "every other task id cold-starts nothing" $ do
            let others = ["jsonSum", "barChart", "revenueTotal", "", "hgg"]
            map coldStartPackages others `shouldBe` replicate 5 []

    describe "storeBuildDirsFor (exact-name build matching)" $ do
        it "matches only exact-name builds across a generated dir grid" $ do
            let vers = ["0.1.0.0-abc123", "1.2-9f", "10.0-h"]
                builds pkg = [pkg <> "-" <> v | v <- vers]
                store =
                    builds "hgg"
                        <> builds "hgg-core"
                        <> builds "hggx"
                        <> builds "aeson"
                        <> ["hgg", "hgg-", "package.db"]
            storeBuildDirsFor "hgg" store `shouldBe` builds "hgg"

        it "never matches a hyphenated sibling for any name in the grid" $ do
            let names = ["hgg", "hgg-core", "aeson", "granite"]
                store = [n <> "-1.0-h" | n <- names]
            sequence_
                [ storeBuildDirsFor (T.pack n) store `shouldBe` [n <> "-1.0-h"]
                | n <- names
                ]

    describe "purgeStoreIn (best-effort, hermetic fake store)" $ do
        it "removes exactly the package's builds and reports them" $ do
            root <- freshFakeStore
            let ghcDir = root </> "ghc-9.12.2"
                victim = ghcDir </> "hgg-0.1.0.0-abc"
                bystander = ghcDir </> "hgg-core-0.1-xyz"
            removed <- purgeStoreIn root "hgg"
            removed `shouldBe` [victim]
            stillThere <- doesDirectoryExist bystander
            stillThere `shouldBe` True
            gone <- doesDirectoryExist victim
            gone `shouldBe` False

        it "is a silent no-op when the package has no builds" $ do
            root <- freshFakeStore
            removed <- purgeStoreIn root "granite"
            removed `shouldBe` []

        it "tolerates a missing store root entirely" $ do
            removed <- purgeStoreIn "/nonexistent/store/root" "hgg"
            removed `shouldBe` []

-- | A throwaway store shaped like ~/.cabal/store: ghc dir + build dirs + db.
freshFakeStore :: IO FilePath
freshFakeStore = do
    tmp <- getTemporaryDirectory
    let root = tmp </> "siza-coldstart-spec-store"
        ghcDir = root </> "ghc-9.12.2"
    removePathForcibly root
    mapM_
        (createDirectoryIfMissing True)
        [ ghcDir </> "hgg-0.1.0.0-abc"
        , ghcDir </> "hgg-core-0.1-xyz"
        , ghcDir </> "package.db"
        ]
    pure root
