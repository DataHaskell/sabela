{-# LANGUAGE OverloadedStrings #-}

{- | Qualified-name handling across every type-parsing seam, pinned against the
VERBATIM strings from the fixed-lever gate.

GHC prints goals with package-qualified names
(@ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity@,
@attoparsec-0.14.4:Data.Attoparsec.Text.Internal.Parser@). That spelling is not
valid Haskell source, so every consumer must either PRESERVE it verbatim (the
extraction layer) or SANITIZE it into something GHCi can parse (the query
layer). A seam that does neither goes silently inert on exactly the errors
that matter.
-}
module Test.QualifiedNameSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.ScratchVet (
    sanitizeGoal,
    splitArrows,
    vetProbe,
 )
import Sabela.AI.CellEco (concreteHead, resultHead)
import Sabela.AI.HoleRepair (goalFromError)

-- | The verbatim gemma goal for takeWhile1.
gemmaGoal :: Text
gemmaGoal =
    "(Char -> Bool) -> ParsecT Void String ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity String"

-- | The verbatim cross-package pollution type from the earlier gate.
attoGoal :: Text
attoGoal =
    "attoparsec-0.14.4:Data.Attoparsec.Text.Internal.Parser Web.Simple.Templates.Types.AST -> ParsecT Void String ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity Double"

spec :: Spec
spec = describe "qualified names across the type-parsing seams" $ do
    describe "extraction preserves the qualified spelling verbatim" $ do
        it "goalFromError keeps the package-qualified tail intact" $ do
            let err =
                    "Variable not in scope:\n\
                    \  takeWhile1\n\
                    \    :: (Char -> Bool)\n\
                    \       -> ParsecT\n\
                    \            Void\n\
                    \            String\n\
                    \            ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity\n\
                    \            String"
            (snd <$> goalFromError err)
                `shouldBe` Just gemmaGoal

    describe "sanitizeGoal rewrites every qualified shape GHC actually prints" $ do
        it "handles the full attoparsec pollution line" $
            sanitizeGoal attoGoal
                `shouldBe` "parser ast -> ParsecT Void String identity Double"
        it "handles a bracket-wrapped qualified name" $
            sanitizeGoal "[text-2.1.2:Data.Text.Internal.Text] -> Int"
                `shouldBe` "[text] -> Int"
        it "leaks no version digits into the result" $ do
            sanitizeGoal gemmaGoal `shouldSatisfy` (not . T.isInfixOf "1202")
            sanitizeGoal attoGoal `shouldSatisfy` (not . T.isInfixOf "0.14.4")

    describe "splitArrows never splits inside a qualified name" $
        it "keeps the qualified token whole in its segment" $
            splitArrows gemmaGoal
                `shouldBe` [ "(Char -> Bool)"
                           , "ParsecT Void String ghc-internal-9.1202.0:GHC.Internal.Data.Functor.Identity.Identity String"
                           ]

    describe "resultHead reads through the package qualifier" $ do
        it "the attoparsec result head is Parser" $
            resultHead
                "(Char -> Bool) -> attoparsec-0.14.4:Data.Attoparsec.Text.Internal.Parser Text"
                `shouldBe` "Parser"
        it "and it counts as a concrete head" $
            concreteHead
                ( resultHead
                    "(Char -> Bool) -> attoparsec-0.14.4:Data.Attoparsec.Text.Internal.Parser Text"
                )
                `shouldBe` True

    describe "the query layer emits only parseable spellings" $
        it "vetProbe on the verbatim gemma goal carries no qualifier" $ do
            let probe = vetProbe "Text.Megaparsec" "takeWhileP" gemmaGoal
            probe `shouldSatisfy` (not . T.isInfixOf "ghc-internal")
            probe `shouldSatisfy` (not . T.isInfixOf ":GHC.")
