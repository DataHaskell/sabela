{-# LANGUAGE OverloadedStrings #-}

{- | Deleting the only cell that declares a dependency leaves every surviving
import of it alive live and broken on any rebuild (the mixed-units episode
failure). The gate names the orphan before the state can arise.
-}
module Test.OrphanGateSpec (orphanGateSpec) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.OrphanGate (
    Orphan (..),
    lostDeclarations,
    orphansAmong,
    undeclaredImportNote,
 )
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))

codeCell :: Int -> Text -> Cell
codeCell cid src = Cell cid CodeCell Haskell src [] Nothing False

nbWith :: [Cell] -> Notebook
nbWith = Notebook "t"

splitModules :: [(Text, [Text])]
splitModules = [("split", ["Data.List.Split", "Data.List.Split.Internals"])]

-- | Cell 14 declares split; cell 3 imports it without declaring it.
episodeShape :: Notebook
episodeShape =
    nbWith
        [ codeCell 3 "import Data.List.Split (splitOn)\nx = splitOn \",\" \"a\""
        , codeCell 14 "-- cabal: build-depends: split\ny = 1"
        ]

orphanGateSpec :: Spec
orphanGateSpec = describe "the delete orphan gate (mixed-units episode)" $ do
    it "a delete that strips the last declaration of an imported dep is named" $ do
        lostDeclarations Set.empty episodeShape 14 `shouldBe` ["split"]
        orphansAmong splitModules episodeShape 14
            `shouldBe` [Orphan 3 "Data.List.Split" "split"]

    it "a surviving declaration keeps the delete clean" $ do
        let nb =
                nbWith
                    [ codeCell
                        3
                        "-- cabal: build-depends: split\n\
                        \import Data.List.Split (splitOn)\nx = 1"
                    , codeCell 14 "-- cabal: build-depends: split\ny = 1"
                    ]
        lostDeclarations Set.empty nb 14 `shouldBe` []

    it "a globally provided dep is never lost" $
        lostDeclarations (Set.fromList ["split"]) episodeShape 14
            `shouldBe` []

    it "a lost dep nothing surviving imports is no orphan" $ do
        let nb =
                nbWith
                    [ codeCell 3 "x = 1"
                    , codeCell 14 "-- cabal: build-depends: split\ny = 1"
                    ]
        orphansAmong splitModules nb 14 `shouldBe` []

    it "the doomed cell's own imports do not count" $ do
        let nb =
                nbWith
                    [ codeCell 3 "x = 1"
                    , codeCell
                        14
                        "-- cabal: build-depends: split\n\
                        \import Data.List.Split (splitOn)\ny = 1"
                    ]
        orphansAmong splitModules nb 14 `shouldBe` []

    describe "the divergence note a replay refusal carries" $ do
        it "names the undeclared package, the live masking, and the home" $ do
            let note =
                    undeclaredImportNote [] (Just 3) "Data.List.Split" "split"
            note `shouldSatisfy` maybe False (T.isInfixOf "notebook cell 3")
            note
                `shouldSatisfy` maybe
                    False
                    (T.isInfixOf "build-depends: split")
            note `shouldSatisfy` maybe False (T.isInfixOf "still installed")

        it "says nothing when the package is declared (a build problem)" $
            undeclaredImportNote ["split"] (Just 3) "Data.List.Split" "split"
                `shouldBe` Nothing
