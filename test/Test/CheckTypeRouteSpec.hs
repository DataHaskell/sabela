{-# LANGUAGE OverloadedStrings #-}

{- | D1: check_type's dispatch. A name is a question about the world and must
never need a session; an expression is a question for a compiler. Stated over
arbitrary identifiers, so no library or function name is load-bearing.
-}
module Test.CheckTypeRouteSpec (spec) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Query.Route (
    Plan (..),
    QueryShape (..),
    classifyQuery,
    lookupText,
    planFor,
    refinePlan,
    typeShaped,
 )

newtype Ident = Ident Text
    deriving (Show)

instance Arbitrary Ident where
    arbitrary = Ident <$> genIdent

newtype Modul = Modul Text
    deriving (Show)

instance Arbitrary Modul where
    arbitrary = do
        parts <- resize 3 (listOf1 genUpperWord)
        pure (Modul (T.intercalate "." parts))

-- | A name a Haskell parser would accept, so no reserved word is generated.
genIdent :: Gen Text
genIdent = genWord `suchThat` (`notElem` reservedWords)
  where
    genWord = do
        c <- elements (['a' .. 'z'] <> ['_'])
        rest <-
            resize
                8
                (listOf (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])))
        pure (T.pack (c : rest))

reservedWords :: [Text]
reservedWords =
    T.words
        "case class data default deriving do else foreign if import in infix \
        \infixl infixr instance let module newtype of then type where"

genUpperWord :: Gen Text
genUpperWord = do
    c <- elements ['A' .. 'Z']
    rest <- resize 6 (listOf (elements (['a' .. 'z'] <> ['0' .. '9'])))
    pure (T.pack (c : rest))

isName :: Text -> Bool
isName t = case classifyQuery t of
    NameQuery _ -> True
    ExpressionQuery -> False

isIndexPlan :: Plan -> Bool
isIndexPlan (IndexPlan _) = True
isIndexPlan SessionPlan = False

spec :: Spec
spec = describe "check_type route (D1)" $ do
    describe "a name is answered from the index, never from a session" $ do
        it "any bare identifier plans an index lookup" $
            property $ \(Ident n) ->
                isIndexPlan (planFor [] (classifyQuery n))

        it "any qualified name plans an index lookup" $
            property $ \(Modul m) (Ident n) ->
                isIndexPlan (planFor [] (classifyQuery (m <> "." <> n)))

        it "any module-shaped or type-shaped name plans an index lookup" $
            property $ \(Modul m) ->
                isIndexPlan (planFor [] (classifyQuery m))

        it "an operator section is a name, not an expression" $
            map isName ["(<+>)", "(.)", "(M.|>)"] `shouldBe` [True, True, True]

    describe "an expression is answered by a compiler" $ do
        it "two juxtaposed names are an expression" $
            property $ \(Ident f) (Ident x) ->
                planFor [] (classifyQuery (f <> " " <> x)) === SessionPlan

        it "a lambda binder is an expression" $
            property $ \(Ident x) ->
                planFor [] (classifyQuery ("\\" <> x <> " -> " <> x))
                    === SessionPlan

        it "an equation binder is an expression" $
            property $ \(Ident f) (Ident x) ->
                planFor [] (classifyQuery (f <> "=" <> x)) === SessionPlan

        it "a signature is an expression" $
            property $ \(Ident f) (Modul m) ->
                planFor [] (classifyQuery (f <> " :: " <> m)) === SessionPlan

    describe "the caller's own scope stays a compiler question" $
        it "any asked-for import forces the session plan" $
            property $ \(Ident n) (Modul m) ->
                planFor ["import " <> m] (classifyQuery n) === SessionPlan

    describe "a name the notebook defines is a session question" $ do
        it "ownership moves an index plan to the session" $
            property $ \(Ident n) ->
                refinePlan True (planFor [] (classifyQuery n)) === SessionPlan

        it "ownership never moves a session plan" $
            property $ \(Ident f) (Ident x) ->
                refinePlan True (planFor [] (classifyQuery (f <> " " <> x)))
                    === SessionPlan

    describe "check_type never issues a type query" $ do
        it "no planned lookup is type-shaped, for arbitrary input" $
            property $ \(NonEmpty cs) ->
                let t = T.pack (take 40 cs)
                 in case planFor [] (classifyQuery t) of
                        IndexPlan n -> not (typeShaped (lookupText n))
                        SessionPlan -> True

        it "a lookup name is a single lexeme of name characters" $
            property $ \(NonEmpty cs) ->
                let t = T.pack (take 40 cs)
                 in case planFor [] (classifyQuery t) of
                        IndexPlan n -> nameLike (lookupText n)
                        SessionPlan -> True

nameLike :: Text -> Bool
nameLike t =
    not (T.null t)
        && ( T.all (\c -> isAlphaNum c || c `elem` ("_'." :: String)) t
                || ("(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t)
           )
        && maybe False (\(c, _) -> isAlpha c || c `elem` ("_(" :: String)) (T.uncons t)
