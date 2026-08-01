{-# LANGUAGE OverloadedStrings #-}

module Test.IndexAnswerSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.AnswerSource (
    AnswerSource (..),
    compilerBacked,
    sourceTag,
    viaVocabulary,
 )
import Sabela.AI.Capabilities.Query.IndexAnswer (
    IndexAnswer (..),
    IndexHit (..),
    PackageState (..),
    builtinAnswer,
    classifyIndexHit,
    looksNotInScope,
    renderIndexAnswer,
 )
import Sabela.AI.PromptCore (builtinNames)

picture :: IndexHit
picture = IndexHit "Picture" "Sabela.Notebook" "sabela-notebook" ""

hidden :: IndexHit
hidden = IndexHit "pack" "Data.Text" "text" "String -> Text"

consulted :: [Text]
consulted = ["local index", "notebook source"]

newtype Ident = Ident Text
    deriving (Show)

instance Arbitrary Ident where
    arbitrary =
        Ident . T.pack <$> resize 8 (listOf1 (elements (['a' .. 'z'] <> "0123456789")))

newtype Modul = Modul Text
    deriving (Show)

instance Arbitrary Modul where
    arbitrary = Modul . T.intercalate "." <$> resize 3 (listOf1 genUpperWord)
      where
        genUpperWord = do
            c <- elements ['A' .. 'Z']
            rest <- resize 6 (listOf (elements (['a' .. 'z'] <> "0123456789")))
            pure (T.pack (c : rest))

{- | Words that state what a compiler holds. A world clause may name where a
name lives and what was consulted; it may never say what is in scope.
-}
sessionClaims :: [Text]
sessionClaims =
    ["in scope", "in this session", "session", "imported", "loaded", "available"]

noSessionClaim :: Text -> Property
noSessionClaim t =
    conjoin
        [ counterexample
            (T.unpack (c <> " in: " <> t))
            (property (not (c `T.isInfixOf` lt)))
        | c <- sessionClaims
        ]
  where
    lt = T.toLower t

spec :: Spec
spec = describe "check_type index answers (G10)" $ do
    describe "classification" $ do
        it "an available package is a declared-package world fact" $
            classifyIndexHit (Set.fromList ["sabela-notebook"]) consulted (Just picture)
                `shouldBe` WorldHit picture Declared

        it "an undeclared package is an undeclared-package world fact" $
            classifyIndexHit (Set.fromList ["base"]) consulted (Just hidden)
                `shouldBe` WorldHit hidden Undeclared

        it "no index hit is a miss naming what was actually consulted" $
            classifyIndexHit (Set.fromList ["base"]) consulted Nothing
                `shouldBe` WorldMiss consulted

    describe "rendering carries the actionable line (G10.4)" $ do
        it "a declared package is stated as a package fact, not a session one" $ do
            let t = renderIndexAnswer (WorldHit picture Declared)
            t `shouldSatisfy` T.isInfixOf "Sabela.Notebook"
            t `shouldSatisfy` T.isInfixOf "sabela-notebook"
            T.toLower t `shouldNotSatisfy` T.isInfixOf "in this session"

        it "an undeclared package hands over the cabal first line" $ do
            let t = renderIndexAnswer (WorldHit hidden Undeclared)
            t `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: text"

        it "a miss says what was consulted, never invents a fix" $ do
            let t = renderIndexAnswer (WorldMiss consulted)
            t `shouldSatisfy` T.isInfixOf "local index"
            t `shouldSatisfy` T.isInfixOf "notebook source"
            t `shouldNotSatisfy` T.isInfixOf "-- cabal:"
            t `shouldNotSatisfy` T.isInfixOf "import "

        it "the three answers are never the same text (the live_test9 bug)" $ do
            let a = renderIndexAnswer (WorldHit picture Declared)
                b = renderIndexAnswer (WorldHit hidden Undeclared)
                c = renderIndexAnswer (WorldMiss consulted)
            a `shouldNotBe` b
            b `shouldNotBe` c
            a `shouldNotBe` c

    describe "builtin tier answers before the Hackage index (live_gemma)" $ do
        it "a drawing builtin is placed in the module that defines it" $
            case fmap renderIndexAnswer (builtinAnswer "displayPicture") of
                Nothing -> expectationFailure "no builtin answer"
                Just t -> do
                    t `shouldSatisfy` T.isInfixOf "Sabela.Notebook"
                    t `shouldNotSatisfy` T.isInfixOf "-- cabal:"
                    t `shouldNotSatisfy` T.isInfixOf "gloss"

        it "an injected builtin claims no module to import" $
            builtinAnswer "displayHtml" `shouldBe` Just (WorldBuiltin "displayHtml" Nothing)

        it "knows nothing about arbitrary names" $
            builtinAnswer "frobnicate" `shouldBe` Nothing

    describe "a world clause never states a session fact" $ do
        it "no rendered hit claims scope, for arbitrary hits" $
            property $ \(Ident n) (Modul m) (Ident p) st ->
                let a = WorldHit (IndexHit n m p "") (if st then Declared else Undeclared)
                 in noSessionClaim (renderIndexAnswer a)

        it "no answer for any builtin in the vocabulary claims scope" $
            conjoin
                ( counterexample "empty vocabulary" (property (not (null builtinNames)))
                    : [ counterexample (T.unpack b) (noSessionClaim (renderIndexAnswer a))
                      | b <- builtinNames
                      , Just a <- [builtinAnswer b]
                      ]
                )

    describe "the via provenance vocabulary (G10.1)" $ do
        it "names every answering source, one closed set" $
            viaVocabulary
                `shouldBe` [ "session-type"
                           , "session-info"
                           , "local-index"
                           , "notebook"
                           , "builtin"
                           ]

        it "only a compiler-asked source is compiler-backed" $
            map
                compilerBacked
                [SessionType, SessionInfo, LocalIndex, NotebookFacts, BuiltinVocab]
                `shouldBe` [True, True, False, False, False]

        it "the tags are distinct" $
            length (map sourceTag [minBound .. maxBound :: AnswerSource])
                `shouldBe` length viaVocabulary

    describe "looksNotInScope" $ do
        it "treats GHC's data-constructor phrasing as a miss, not an answer" $
            looksNotInScope "error: Not in scope: data constructor 'Picture'"
                `shouldBe` True

        it "treats an empty answer as a miss" $
            looksNotInScope "   " `shouldBe` True

        it "leaves a real signature alone" $
            looksNotInScope "plot :: [(Double, Double)] -> Picture"
                `shouldBe` False
