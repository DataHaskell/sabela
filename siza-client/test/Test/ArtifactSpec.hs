{-# LANGUAGE OverloadedStrings #-}

module Test.ArtifactSpec (artifactSpec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Owned (OwnedCell (..), hasArtifact)

cell :: Text -> OwnedCell
cell src =
    OwnedCell
        { ocHealthy = True
        , ocDiagnostic = ""
        , ocSource = src
        , ocInvariantAlarm = False
        }

owned :: [Text] -> Map.Map Int OwnedCell
owned srcs = Map.fromList (zip [0 ..] (map cell srcs))

artifactSpec :: Spec
artifactSpec = describe "an artifact is a cell that does something" $ do
    it "a dependency-only cell is not an artifact" $
        hasArtifact (owned ["-- cabal: build-depends: QuickCheck"]) `shouldBe` False

    it "several dependency-only cells are still not an artifact" $
        hasArtifact
            (owned ["-- cabal: build-depends: text", "-- cabal: build-depends: mtl"])
            `shouldBe` False

    it "a comment-only cell is not an artifact" $
        hasArtifact (owned ["-- we will use this for plotting"]) `shouldBe` False

    it "a pragma-only cell is not an artifact" $
        hasArtifact (owned ["{-# LANGUAGE OverloadedStrings #-}"]) `shouldBe` False

    it "a cell with code is an artifact" $
        hasArtifact (owned ["x = 1 + 1"]) `shouldBe` True

    it "code alongside its dependency line is an artifact" $
        hasArtifact (owned ["-- cabal: build-depends: text\nimport Data.Text\nx = 1"])
            `shouldBe` True

    it "one real cell among dependency lines is an artifact" $
        hasArtifact (owned ["-- cabal: build-depends: text", "main = print 1"])
            `shouldBe` True

    it "an empty owned map is no artifact" $
        hasArtifact (owned []) `shouldBe` False
