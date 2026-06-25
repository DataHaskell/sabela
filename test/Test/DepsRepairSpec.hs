{-# LANGUAGE OverloadedStrings #-}

{- | 'repairDeps' comma-separates build-depends a model wrote space-separated
(@text granite@), without breaking version constraints, whose spaces are not
dependency boundaries (@dataframe == 2.3.0.0@, @text < 4 && < 5@). A space-
mangled line otherwise reaches cabal as one bogus package and the install hangs
to a timeout (observed on the quarterlyBars benchmark task).
-}
module Test.DepsRepairSpec (depsRepairSpec) where

import Sabela.Deps (repairDeps)
import Test.Hspec

depsRepairSpec :: Spec
depsRepairSpec = describe "Sabela.Deps.repairDeps" $ do
    it "splits two space-separated packages into two deps" $
        repairDeps ["text granite"] `shouldBe` ["text", "granite"]

    it "splits three space-separated packages" $
        repairDeps ["text granite dataframe"]
            `shouldBe` ["text", "granite", "dataframe"]

    it "preserves a version constraint (its spaces are not boundaries)" $
        repairDeps ["dataframe == 2.3.0.0"] `shouldBe` ["dataframe == 2.3.0.0"]

    it "splits a bare package off the end of a constrained dep" $
        repairDeps ["dataframe == 2.3.0.0 text"]
            `shouldBe` ["dataframe == 2.3.0.0", "text"]

    it "preserves a compound && version constraint" $
        repairDeps ["text < 4 && < 5"] `shouldBe` ["text < 4 && < 5"]

    it "splits a bare package after a compound constraint" $
        repairDeps ["text >= 1.2 && < 5 granite"]
            `shouldBe` ["text >= 1.2 && < 5", "granite"]

    it "leaves an already comma-split list unchanged (idempotent)" $
        repairDeps ["text", "granite"] `shouldBe` ["text", "granite"]

    it "keeps a hyphenated package name as one dep" $
        repairDeps ["base16-bytestring"] `shouldBe` ["base16-bytestring"]

    it "splits a fused-operator constrained dep off a following package" $
        repairDeps ["dataframe==2.3.0.0 text"]
            `shouldBe` ["dataframe==2.3.0.0", "text"]

    it "treats a digit-initial but lettered name as a package" $
        repairDeps ["3d-graphics vector"] `shouldBe` ["3d-graphics", "vector"]

    it "drops an empty entry" $
        repairDeps [""] `shouldBe` []
