{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for scratch-session candidate vetting (plan v2 Phase D).

Gate transcripts showed resolver candidates being EXECUTED into the live GHCi
scope to vet them — a rejected attoparsec import leaked into both arms'
sessions. The fix: vet in the isolated scratchpad with the compiler as the
oracle. These pin the pure pieces: the collision-free import alias, the cell
import replay, and the goal sanitizer that turns GHC's package-qualified noise
into type variables GHCi can parse (a pkg-qualified name in a @::@ annotation
is a parse error, which would false-decline every candidate).
-}
module Test.ScratchVetSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.ScratchVet (
    cellImportLines,
    sanitizeGoal,
    splitArrows,
    vetAlias,
    vetProbe,
 )

spec :: Spec
spec = describe "scratch-session candidate vetting (intention)" $ do
    describe "vetAlias — a collision-free qualified alias per module" $ do
        it "derives a stable alias from the module path" $
            vetAlias "Data.Attoparsec.Text" `shouldBe` "V_Data_Attoparsec_Text"
        it "distinct modules get distinct aliases" $
            vetAlias "Data.List.Split" `shouldNotBe` vetAlias "Data.List"

    describe "sanitizeGoal — make GHC's printed goal parseable in a :: annotation" $ do
        it "rewrites a package-qualified name to a type variable" $
            sanitizeGoal
                "(Char -> Bool) -> ParsecT Void String ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity String"
                `shouldBe` "(Char -> Bool) -> ParsecT Void String identity String"
        it "rewrites a module-qualified name to a type variable" $
            sanitizeGoal "Data.Text.Internal.Text -> Int"
                `shouldBe` "text -> Int"
        it "leaves a plain goal untouched" $
            sanitizeGoal "Int -> Int -> [a] -> [[a]]"
                `shouldBe` "Int -> Int -> [a] -> [[a]]"
        it "maps the same qualified name to the same variable each time" $ do
            let g =
                    sanitizeGoal
                        "attoparsec-0.14.4:Data.Attoparsec.Text.Internal.Parser Web.Simple.Templates.Types.AST -> attoparsec-0.14.4:Data.Attoparsec.Text.Internal.Parser Web.Simple.Templates.Types.AST"
            let ws = T.words g
            take 1 ws `shouldBe` drop 3 (take 4 ws)

    describe "cellImportLines — replay the cell's own scope into the scratch" $
        it "keeps only the import lines" $
            cellImportLines
                ( T.unlines
                    [ "-- cabal: build-depends: megaparsec"
                    , "import Text.Megaparsec"
                    , "import qualified Text.Megaparsec.Char.Lexer as L"
                    , "digits = undefined"
                    ]
                )
                `shouldBe` [ "import Text.Megaparsec"
                           , "import qualified Text.Megaparsec.Char.Lexer as L"
                           ]

    describe "splitArrows — top-level arrow segments, paren-aware" $ do
        it "splits at the top level only" $
            splitArrows "(Char -> Bool) -> ParsecT Void String m String"
                `shouldBe` ["(Char -> Bool)", "ParsecT Void String m String"]
        it "drops a leading class context" $
            splitArrows "Num i => [a] -> i" `shouldBe` ["[a]", "i"]
        it "a plain type is one segment" $
            splitArrows "[[Int]]" `shouldBe` ["[[Int]]"]

    describe "vetProbe — apply-and-unify, not annotate" $ do
        -- A :: annotation would skolemize the goal's inferred type variables
        -- and demand the candidate be at least that polymorphic — declining
        -- legitimate candidates (measured live: divvy). Applying to typed
        -- undefineds unifies instead, and arity mismatches still decline.
        it "applies the candidate to each goal argument and unifies the result" $
            vetProbe "Data.List.Split" "divvy" "Int -> Int -> [a] -> [[a]]"
                `shouldBe` "(V_Data_List_Split.divvy (undefined :: Int) (undefined :: Int) (undefined :: [a])) `asTypeOf` (undefined :: [[a]])"
        it "keeps inferred type variables flexible" $
            vetProbe "Data.List.Split" "divvy" "t0 -> t1 -> [Int] -> t"
                `shouldBe` "(V_Data_List_Split.divvy (undefined :: t0) (undefined :: t1) (undefined :: [Int])) `asTypeOf` (undefined :: t)"
        it "a zero-argument goal only unifies the result" $
            vetProbe "Data.Map" "empty" "Map k a"
                `shouldBe` "(V_Data_Map.empty) `asTypeOf` (undefined :: Map k a)"
