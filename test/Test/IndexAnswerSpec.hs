{-# LANGUAGE OverloadedStrings #-}

{- | G10: a scope miss is classified, never served raw. The live_test9 bill:
@check_type@ on @Sabela.Notebook.render@, @toSVG@ and @displayPicture@
returned identical not-in-scope text, so the model could not tell which kind
of answer it held. These cases pin that the three answers differ and that
each actionable one carries the line that would fix it.
-}
module Test.IndexAnswerSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Query.IndexAnswer (
    IndexAnswer (..),
    IndexHit (..),
    classifyIndexHit,
    consultedSources,
    looksNotInScope,
    renderIndexAnswer,
    viaLocalIndex,
    viaSessionInfo,
    viaSessionType,
    viaVocabulary,
 )

picture :: IndexHit
picture = IndexHit "Picture" "Sabela.Notebook" "sabela-notebook" ""

hidden :: IndexHit
hidden = IndexHit "pack" "Data.Text" "text" "String -> Text"

spec :: Spec
spec = describe "check_type index answers (G10)" $ do
    describe "classification" $ do
        it "unimported-type: an available package is a not-imported fact" $
            classifyIndexHit (Set.fromList ["sabela-notebook"]) (Just picture)
                `shouldBe` NotImported picture

        it "an undeclared package is a not-installed fact" $
            classifyIndexHit (Set.fromList ["base"]) (Just hidden)
                `shouldBe` NotInstalled hidden

        it "no index hit is an unknown, naming what was consulted" $
            classifyIndexHit (Set.fromList ["base"]) Nothing
                `shouldBe` UnknownName consultedSources

    describe "rendering carries the actionable line (G10.4)" $ do
        it "a not-imported answer hands over the import, not a description" $ do
            let t = renderIndexAnswer (NotImported picture)
            t `shouldSatisfy` T.isInfixOf "import Sabela.Notebook"
            t `shouldSatisfy` T.isInfixOf "sabela-notebook"

        it "a not-installed answer hands over the cabal first line" $ do
            let t = renderIndexAnswer (NotInstalled hidden)
            t `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: text"

        it "an unknown answer says what was consulted, never invents a fix" $ do
            let t = renderIndexAnswer (UnknownName consultedSources)
            t `shouldSatisfy` T.isInfixOf "local index"
            t `shouldSatisfy` T.isInfixOf "live session"
            t `shouldNotSatisfy` T.isInfixOf "-- cabal:"
            t `shouldNotSatisfy` T.isInfixOf "import "

        it "the three answers are never the same text (the live_test9 bug)" $ do
            let a = renderIndexAnswer (NotImported picture)
                b = renderIndexAnswer (NotInstalled hidden)
                c = renderIndexAnswer (UnknownName consultedSources)
            a `shouldNotBe` b
            b `shouldNotBe` c
            a `shouldNotBe` c

    describe "the via provenance vocabulary (G10.1)" $ do
        it "names the answering source, one closed set" $ do
            viaVocabulary
                `shouldBe` ["session-type", "session-info", "local-index"]
            viaSessionType `shouldNotBe` viaLocalIndex
            viaSessionInfo `shouldNotBe` viaLocalIndex

    describe "looksNotInScope" $ do
        it "treats GHC's data-constructor phrasing as a miss, not an answer" $
            looksNotInScope "error: Not in scope: data constructor 'Picture'"
                `shouldBe` True

        it "treats an empty answer as a miss" $
            looksNotInScope "   " `shouldBe` True

        it "leaves a real signature alone" $
            looksNotInScope "plot :: [(Double, Double)] -> Picture"
                `shouldBe` False
