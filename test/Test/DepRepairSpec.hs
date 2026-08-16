{-# LANGUAGE OverloadedStrings #-}

module Test.DepRepairSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.DepRepair (
    addBuildDepend,
    depFromResult,
    depName,
    newDependencies,
    pinnedDep,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Test.Hspec

hiddenPkgErr :: Text
hiddenPkgErr =
    T.unlines
        [ "Could not load module \8216Network.HTTP.Simple\8217."
        , "It is a member of the hidden package \8216http-conduit-2.3.9.1\8217."
        , "You can run \8216:set -package http-conduit\8217 to expose it."
        ]

raising :: Text -> Either Text ExecutionResult
raising msg = Right (ExecutionResult [] (Just msg) [] [])

spec :: Spec
spec = describe "Sabela.AI.DepRepair" $ do
    describe "addBuildDepend" $ do
        it "appends a new package to an existing build-depends line" $
            ( "build-depends: dataframe, http-conduit"
                `T.isInfixOf` addBuildDepend "http-conduit" "-- cabal: build-depends: dataframe\nimport X"
            )
                `shouldBe` True

        it "is a no-op when the package is already declared" $
            addBuildDepend "dataframe" "-- cabal: build-depends: dataframe\nimport X"
                `shouldBe` "-- cabal: build-depends: dataframe\nimport X"

        it "prepends a -- cabal: line when the cell declares none" $ do
            let r = addBuildDepend "dataframe" "import X\nx = 1"
            ("-- cabal: build-depends: dataframe" `T.isPrefixOf` r) `shouldBe` True
            ("import X" `T.isInfixOf` r) `shouldBe` True

        it "keeps the existing packages when adding another" $
            ( "dataframe, vector, text"
                `T.isInfixOf` addBuildDepend "text" "-- cabal: build-depends: dataframe, vector\nimport X"
            )
                `shouldBe` True

        it "does not duplicate a package already in a multi-package line" $
            addBuildDepend "vector" "-- cabal: build-depends: dataframe, vector\nimport X"
                `shouldBe` "-- cabal: build-depends: dataframe, vector\nimport X"

        it "appends a pinned entry verbatim" $
            ( "build-depends: dataframe, text ==2.0.2"
                `T.isInfixOf` addBuildDepend
                    "text ==2.0.2"
                    "-- cabal: build-depends: dataframe\nimport X"
            )
                `shouldBe` True

        it "never re-pins a package the cell already declares, bare or pinned" $ do
            addBuildDepend "text ==2.0.2" "-- cabal: build-depends: text\nimport X"
                `shouldBe` "-- cabal: build-depends: text\nimport X"
            addBuildDepend "text" "-- cabal: build-depends: text ==2.1.4\nimport X"
                `shouldBe` "-- cabal: build-depends: text ==2.1.4\nimport X"

    describe "pinnedDep / depName" $ do
        it "renders the version its evidence names, bare without one" $ do
            pinnedDep "text" (Just "2.0.2") `shouldBe` "text ==2.0.2"
            pinnedDep "text" Nothing `shouldBe` "text"
        it "reads the name back from any constraint spelling" $ do
            depName "text ==2.0.2" `shouldBe` "text"
            depName "dataframe==2.3.0.0" `shouldBe` "dataframe"
            depName "text >= 1.2 && < 5" `shouldBe` "text"
            depName "base16-bytestring" `shouldBe` "base16-bytestring"

    describe "depFromResult" $ do
        it "extracts the package GHC named in a hidden-package failure" $
            depFromResult (raising hiddenPkgErr) `shouldBe` Just "http-conduit"

        it "is Nothing when the run succeeded" $
            depFromResult (Right (ExecutionResult [] Nothing [] [])) `shouldBe` Nothing

        it "is Nothing for an error that names no package" $
            depFromResult (raising "Variable not in scope: foo") `shouldBe` Nothing

        it "is Nothing for an abort (Left)" $
            depFromResult (Left "Cancelled") `shouldBe` Nothing

    describe "newDependencies (G2's dependency-add detector)" $ do
        it "is empty when the candidate declares nothing new" $
            newDependencies
                "-- cabal: build-depends: dataframe\nimport X"
                "-- cabal: build-depends: dataframe\nimport X\ny = 1"
                `shouldBe` []
        it "names the one package a candidate adds" $
            newDependencies
                "import X"
                "-- cabal: build-depends: http-conduit\nimport X"
                `shouldBe` ["http-conduit"]
        it "is empty when neither source declares a dependency" $
            newDependencies "x = 1" "x = 2" `shouldBe` []
        it "names only the packages priorSrc did not already have" $
            newDependencies
                "-- cabal: build-depends: dataframe\nimport X"
                "-- cabal: build-depends: dataframe, vector\nimport X"
                `shouldBe` ["vector"]
        it "a pin on an already-declared package is not a new dependency" $
            newDependencies
                "-- cabal: build-depends: text\nimport X"
                "-- cabal: build-depends: text ==2.0.2\nimport X"
                `shouldBe` []
