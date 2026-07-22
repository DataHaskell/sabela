{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for the DEFERRED P2 discovery scope: finding the right
function BEYOND the cell's current imports, by type, without leaving the cell's
ecosystem.

The two pure decision cores are tested here; the IO glue (the isolated-scratchpad
hole-fit probe that produces the candidates, and hoogle-by-goal-type) is verified
live, not unit-tested.

  1. cellEco  — read the ecosystem the cell has already committed to (its imports
                + its `-- cabal: build-depends:` line), so resolution can prefer it.
  2. rankFits — given the goal type + the cell's ecosystem + candidate fits (each
                a name, type, home module, package), keep the type-compatible ones
                and rank the cell's own ecosystem first. The foreign, type-
                incompatible fit (attoparsec's takeWhile1 :: … -> Parser Text for a
                megaparsec … -> ParsecT goal) is DROPPED, not just demoted.

  Provenance: a candidate's home module comes from the hole-fit's
  "(imported from M)" line (today discarded by 'HoleFits.isProvenance'); the
  parser must capture it so `fcModule`/`fcPackage` are populated. Tested via the
  parser test below.
-}
module Test.TypeDiscoverySpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec

-- Proposed API (to be implemented in Sabela.AI.CellEco + the hole-fit ranker):
--
--   data CellEco = CellEco { ecoModules :: Set Text, ecoPackages :: Set Text }
--   cellEco   :: Text -> CellEco
--   data FitCand = FitCand { fcName, fcType, fcModule, fcPackage :: Text }
--   rankFits  :: Text -> CellEco -> [FitCand] -> [FitCand]
--   fitProvenance :: Text -> [(Text, Text)]   -- hole-fit blob -> [(name, module)]
--
import Sabela.AI.CellEco (
    CellEco (..),
    FitCand (..),
    cellEco,
    fitProvenance,
    rankFits,
 )

spec :: Spec
spec = describe "type-directed discovery beyond in-scope (intention)" $ do
    describe "cellEco — the ecosystem the cell has committed to" $ do
        it "extracts imported modules and declared build-depends" $
            cellEco
                ( T.unlines
                    [ "-- cabal: build-depends: megaparsec, text"
                    , "import Text.Megaparsec"
                    , "import qualified Text.Megaparsec.Char.Lexer as L"
                    , "digits = undefined"
                    ]
                )
                `shouldBe` CellEco
                    (S.fromList ["Text.Megaparsec", "Text.Megaparsec.Char.Lexer"])
                    (S.fromList ["megaparsec", "text"])

    describe "fitProvenance — capture a hole fit's home module" $ do
        it "reads the (imported from M) line the parser currently drops" $
            fitProvenance
                ( T.unlines
                    [ "  Valid hole fits include"
                    , "    takeWhileP :: Maybe String -> (Token s -> Bool) -> m (Tokens s)"
                    , "      (imported from Text.Megaparsec.Char)"
                    , "    some :: Alternative f => f a -> f [a]"
                    , "      (imported from Control.Applicative)"
                    ]
                )
                `shouldBe` [ ("takeWhileP", "Text.Megaparsec.Char")
                           , ("some", "Control.Applicative")
                           ]

        it "reads the real GHC 9.12 form: smart quotes, close paren on the next line" $
            fitProvenance
                ( T.unlines
                    [ "  Valid hole fits include"
                    , "    genericLength :: (Num i) => [a] -> i"
                    , "      (imported from \8216Data.List\8217"
                    , "       (and originally defined in \8216GHC.Internal.Data.List\8217))"
                    ]
                )
                `shouldBe` [("genericLength", "Data.List")]

    describe "rankFits — type-compatible, cell's ecosystem first" $ do
        let goal = "(Char -> Bool) -> ParsecT Void String Identity String"
            eco = CellEco (S.fromList ["Text.Megaparsec.Char"]) (S.fromList ["megaparsec"])
            good =
                FitCand
                    "takeWhileP"
                    "Maybe String -> (Token s -> Bool) -> m (Tokens s)"
                    "Text.Megaparsec.Char"
                    "megaparsec"
            foreign_ =
                FitCand
                    "takeWhile1"
                    "(Char -> Bool) -> Parser Text"
                    "Data.Attoparsec.Text"
                    "attoparsec"

        it "demotes a foreign type-incompatible fit (attoparsec Parser vs ParsecT goal)" $
            rankFits goal eco [foreign_, good] `shouldBe` [good, foreign_]

        it "keeps an in-ecosystem polymorphic fit" $
            rankFits goal eco [good] `shouldBe` [good]

        it "KEEPS a fit whose concrete result head equals the goal's" $ do
            -- The accept quadrant: drop-everything-concrete passes the drop
            -- test; this pins that a matching head is kept.
            let matching =
                    FitCand
                        "takeWhileX"
                        "(Char -> Bool) -> ParsecT Void String Identity String"
                        "Some.Parser.Module"
                        "some-parser"
            rankFits goal eco [matching] `shouldBe` [matching]

        it "ranks the cell's own ecosystem module ahead of an equally-typed outsider" $ do
            let outsider =
                    FitCand
                        "takeWhileP"
                        "Maybe String -> (Token s -> Bool) -> m (Tokens s)"
                        "Some.Other.Parser"
                        "other-parser"
            map fcModule (rankFits goal eco [outsider, good])
                `shouldBe` ["Text.Megaparsec.Char", "Some.Other.Parser"]
