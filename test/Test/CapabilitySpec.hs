{-# LANGUAGE OverloadedStrings #-}

module Test.CapabilitySpec (spec) where

import Sabela.AI.Capabilities.ModuleCard (importLineFor)
import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    defaultSynonyms,
    parseCapabilities,
    relevanceScore,
    searchCapabilities,
 )
import Test.CapabilitySpec.Fixtures (cap)
import Test.CapabilitySpec.Search (searchSpec)
import Test.Hspec

spec :: Spec
spec =
    unqualifySpec >> relevanceSpec >> typeIndexSpec >> importLineSpec >> searchSpec

relevanceSpec :: Spec
relevanceSpec = describe "one relevance scale for search and cards" $ do
    let summarize = cap "DataFrame" "summarize" "DataFrame -> DataFrame"
        columns = cap "DataFrame" "columns" "DataFrame -> Vector Column"
        score = relevanceScore defaultSynonyms
    it "a near-spelling query outranks an unrelated export" $
        score "summary" summarize `shouldSatisfy` (> score "summary" columns)
    it "an exact query outranks a near spelling" $
        score "summarize" summarize `shouldSatisfy` (> score "summary" summarize)
    it "no relation scores zero" $
        score "zzznope" columns `shouldBe` 0

unqualifySpec :: Spec
unqualifySpec = describe "browse names unqualify structurally" $ do
    it "a qualified operator keeps its whole name" $
        parseCapabilities "M" "(DataFrame..&&.) :: Expr Bool -> Expr Bool -> Expr Bool"
            `shouldBe` [cap "M" "(.&&.)" "Expr Bool -> Expr Bool -> Expr Bool"]
    it "a unit-prefixed re-export drops the unit and the qualifier" $
        parseCapabilities
            "M"
            "dataframe-core-2.1.0.0:DataFrame.Internal.DataFrame.columns :: DataFrame -> Vector Column"
            `shouldBe` [cap "M" "columns" "DataFrame -> Vector Column"]
    it "a plain qualified name still unqualifies" $
        parseCapabilities "M" "DataFrame.readCsv :: FilePath -> IO DataFrame"
            `shouldBe` [cap "M" "readCsv" "FilePath -> IO DataFrame"]

typeIndexSpec :: Spec
typeIndexSpec = describe "a type declaration is indexed like any other name" $ do
    let browse =
            "type TBQueue :: * -> *\n\
            \data TBQueue a = TBQueue {-# UNPACK #-}(TVar Natural)\n\
            \type CharBuffer = Buffer Char\n\
            \newtype Down a = Down {getDown :: a}\n\
            \class Eq a => Ord a where\n\
            \  compare :: a -> a -> Ordering\n\
            \atomically :: STM a -> IO a\n"
        caps = parseCapabilities "M" browse
        named n = [c | c <- caps, capName c == n]
    it "indexes a data declaration's type" $
        map capName (named "TBQueue") `shouldBe` ["TBQueue"]
    it "indexes a type synonym" $
        map capName (named "CharBuffer") `shouldBe` ["CharBuffer"]
    it "indexes a newtype" $
        map capName (named "Down") `shouldBe` ["Down"]
    it "indexes a class, past its context" $
        map capName (named "Ord") `shouldBe` ["Ord"]
    it "does not double-index a standalone kind signature" $
        length (named "TBQueue") `shouldBe` 1
    it "still indexes value bindings" $
        map capName (named "atomically") `shouldBe` ["atomically"]
    it "still indexes a record selector" $
        map capName (named "getDown") `shouldBe` ["getDown"]
    it "still indexes a class method" $
        map capName (named "compare") `shouldBe` ["compare"]
    it "a type is findable by name, which is what the error names" $
        (capName . hitCap <$> take 1 (searchCapabilities defaultSynonyms caps "TBQueue"))
            `shouldBe` ["TBQueue"]

importLineSpec :: Spec
importLineSpec = describe "a hit carries the import that uses it" $ do
    let impOf = importLineFor
    it "a type imports scoped to itself" $
        impOf (cap "Control.Concurrent.STM" "TBQueue" "TBQueue a")
            `shouldBe` Just "import Control.Concurrent.STM (TBQueue)"
    it "a value imports the same way" $
        impOf (cap "Control.Monad.STM" "atomically" "STM a -> IO a")
            `shouldBe` Just "import Control.Monad.STM (atomically)"
    it "an operator is wrapped in parens" $
        impOf (cap "Data.Csv" "!" "Record -> Int -> Parser a")
            `shouldBe` Just "import Data.Csv ((!))"
    it "a hit with no module carries no import" $
        impOf (cap "" "orphan" "Int") `shouldBe` Nothing
