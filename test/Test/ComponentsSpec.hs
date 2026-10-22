{-# LANGUAGE OverloadedStrings #-}

{- | Components: what a module says about itself, the cabal file the
package regenerates, and what extracting cells makes of them.
-}
module Test.ComponentsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.Components (
    Header (..),
    parseHeader,
    replSupportFor,
    validComponentModule,
 )
import Sabela.Components.Extract (Extraction (..), extractModule)
import Sabela.Model (Cell (..), CellType (..))
import Sabela.Session.Project (ReplSupport (..))
import qualified Sabela.SessionTypes as ST

cell :: Int -> T.Text -> Cell
cell i src = Cell i CodeCell ST.Haskell src [] Nothing False

spec :: Spec
spec = do
    describe "component modules" $ do
        it "live under Components with capitalised segments" $ do
            validComponentModule "Components.Retention" `shouldBe` True
            validComponentModule "Components.Charts.Revenue" `shouldBe` True
            validComponentModule "Retention" `shouldBe` False
            validComponentModule "Components.lower" `shouldBe` False
            validComponentModule "Components." `shouldBe` False

        it "read their purpose, kind, deps and exports from the header" $ do
            let h =
                    parseHeader
                        ( T.unlines
                            [ "{- | Retention by cohort. -}"
                            , "-- Sabela: kind=chart"
                            , "-- cabal: build-depends: dataframe, text"
                            , "module Components.Retention ("
                            , "    retention,"
                            , "    Cohort (..),"
                            , ") where"
                            ]
                        )
            hdPurpose h `shouldBe` "Retention by cohort."
            hdKind h `shouldBe` "chart"
            hdDeps h `shouldBe` ["dataframe", "text"]
            hdExports h `shouldBe` ["retention", "Cohort"]

        it "join the session's support when the overlay is among the packages" $ do
            replSupportFor ["/w/components"]
                `shouldBe` WithNotebookAnd ["sabela-components"]
            replSupportFor ["/w/other"] `shouldBe` WithNotebookSupport

    describe "extractModule" $ do
        let cells =
                [ cell
                    1
                    ( T.unlines
                        [ "-- cabal: build-depends: text"
                        , "{-# LANGUAGE ScopedTypeVariables #-}"
                        , "import qualified Data.Text as T"
                        , ""
                        , "shout :: String -> String"
                        , "shout = map succ"
                        , ""
                        , "greeting = shout \"hello\""
                        , ""
                        , "putStrLn greeting"
                        ]
                    )
                ]
            ex = extractModule "Components.Shout" "Shouting." cells

        it "hoists pragmas, imports and deps and exports the definitions" $ do
            let src = exSource ex
            T.isPrefixOf "{-# LANGUAGE ScopedTypeVariables #-}" src `shouldBe` True
            T.isInfixOf "{- | Shouting. -}" src `shouldBe` True
            T.isInfixOf "-- cabal: build-depends: text" src `shouldBe` True
            T.isInfixOf "module Components.Shout (" src `shouldBe` True
            T.isInfixOf "import qualified Data.Text as T" src `shouldBe` True
            exExports ex `shouldMatchList` ["shout", "greeting"]
            T.isInfixOf "putStrLn greeting" src `shouldBe` False

        it "leaves the trailing expression in the notebook behind an import" $
            exReplacement ex `shouldBe` "import Components.Shout\n\nputStrLn greeting\n"

        it "keeps an indented continuation with its declaration" $ do
            let ex2 =
                    extractModule
                        "Components.Rows"
                        ""
                        [ cell
                            1
                            "rows = V.fromRows [\"a\"]\n  [[\"1\"], [\"2\"]]\n\nV.chart rows\n  [V.mark V.Bar]\n"
                        ]
            T.isInfixOf "  [[\"1\"], [\"2\"]]" (exSource ex2) `shouldBe` True
            exReplacement ex2
                `shouldBe` "import Components.Rows\n\nV.chart rows\n  [V.mark V.Bar]\n"

        it "warns about prompt-only names and top-level binds" $ do
            let ex3 =
                    extractModule
                        "Components.W"
                        ""
                        [cell 1 "n <- mkWidget (slider \"n\" 1 0 9)\nf x = x + n\nf 2\n"]
            exWarnings ex3 `shouldSatisfy` any (T.isInfixOf "<-")
            exWarnings ex3 `shouldSatisfy` any (T.isInfixOf "mkWidget")
