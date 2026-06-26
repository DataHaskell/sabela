{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of the server-side missing-dependency repair: read the package
GHC named in a "hidden package" failure, and merge it into the cell's
@-- cabal: build-depends:@ line. The IO loop that re-runs the cell is verified
live; these pin the decisions it makes.
-}
module Test.DepRepairSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.DepRepair (addBuildDepend, depFromResult)
import Sabela.AI.Types (ExecutionResult (..))
import Test.Hspec

-- | A realistic GHC "hidden package" wall (the shape the kernel emits).
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

    describe "depFromResult" $ do
        it "extracts the package GHC named in a hidden-package failure" $
            depFromResult (raising hiddenPkgErr) `shouldBe` Just "http-conduit"

        it "is Nothing when the run succeeded" $
            depFromResult (Right (ExecutionResult [] Nothing [] [])) `shouldBe` Nothing

        it "is Nothing for an error that names no package" $
            depFromResult (raising "Variable not in scope: foo") `shouldBe` Nothing

        it "is Nothing for an abort (Left)" $
            depFromResult (Left "Cancelled") `shouldBe` Nothing
