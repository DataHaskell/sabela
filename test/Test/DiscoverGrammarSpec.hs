{-# LANGUAGE OverloadedStrings #-}

{- | Live-grammar discovery: parsing imports, deciding when to re-browse, and
synthesising a grammar over an injected (here, pure 'Identity') browse function.
-}
module Test.DiscoverGrammarSpec (spec) where

import Data.Functor.Identity (Identity (..))
import Data.Maybe (isJust)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Discover (discoverGrammar, importedModules, rediscoverModules)
import Sabela.AI.Grammar (ImportStyle (..))

spec :: Spec
spec = describe "Sabela.AI.Discover" $ do
    describe "importedModules" $ do
        it "parses qualified and unqualified imports, ignoring code" $
            map
                fst
                (importedModules "import qualified DataFrame as D\nimport Granite.Svg\nx = 1")
                `shouldBe` ["DataFrame", "Granite.Svg"]
        it "reads the alias of a qualified import" $
            importedModules "import qualified DataFrame as D"
                `shouldBe` [("DataFrame", QualifiedAs "D")]

    describe "rediscoverModules" $ do
        it "re-browses the cell's imports on a grammar-implicated error" $
            map fst (rediscoverModules "import Granite.Svg" "Variable not in scope: foo")
                `shouldBe` ["Granite.Svg"]
        it "re-browses nothing on an unrelated error" $
            rediscoverModules "import Granite.Svg" "execution timed out" `shouldBe` []

    describe "discoverGrammar" $ do
        it "synthesises a grammar from a module that browses to a surface" $ do
            let browse m = Identity (if m == "Granite.Svg" then "lineGraph :: Bars -> Svg" else "")
                r = runIdentity (discoverGrammar browse [("Granite.Svg", Unqualified)])
            isJust r `shouldBe` True
            fmap (T.isInfixOf "lineGraph") r `shouldBe` Just True
        it "is Nothing when nothing browses to a usable surface" $
            runIdentity (discoverGrammar (const (Identity "")) [("X", Unqualified)])
                `shouldBe` Nothing
