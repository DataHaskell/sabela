{-# LANGUAGE OverloadedStrings #-}

{- | D3: a claim about this session comes from this session. check_type's world
half may say a package exists; only the notebook may say what it imports, and
the trace names what was actually consulted. Stated over arbitrary modules,
identifiers and cell ids.
-}
module Test.SessionFactsSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Query.IndexAnswer (
    IndexAnswer (..),
    IndexHit (..),
    PackageState (..),
    answerModule,
    indexAnswerPairs,
    indexLookupWith,
 )
import Sabela.AI.Capabilities.Query.SessionFacts (
    CellFact (..),
    CellRan (..),
    NotebookFacts (..),
    cellRan,
    notebookAliasEnv,
    notebookFacts,
    renderNotebookFacts,
 )
import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.HoogleResolve (rankResolveTopK)
import Sabela.AI.QualifiedName (QualifiedName (..), resolveSpelling)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))

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

newtype Pkg = Pkg Text
    deriving (Show)

instance Arbitrary Pkg where
    arbitrary = Pkg <$> genIdent

{- | An identifier a Haskell parser will accept: this spec feeds its output to
one, and a reserved word is not a name.
-}
genIdent :: Gen Text
genIdent = genWord `suchThat` (`notElem` reservedWords)
  where
    genWord = do
        c <- elements ['a' .. 'z']
        rest <- resize 8 (listOf (elements (['a' .. 'z'] <> ['0' .. '9'])))
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

code :: Int -> Text -> Cell
code cid src = Cell cid CodeCell Haskell src [] Nothing False

nb :: [Cell] -> Notebook
nb = Notebook "t"

trace :: [Text]
trace = ["local index", "notebook source"]

pairsFor ::
    Text -> Maybe Text -> IndexAnswer -> NotebookFacts -> ([(K.Key, Value)], Text)
pairsFor name mModule = indexAnswerPairs (QualifiedName name mModule "")

answerFor :: Text -> Maybe Text -> IndexAnswer -> NotebookFacts -> Text
answerFor name mModule a facts = snd (pairsFor name mModule a facts)

hitIn :: Text -> Text -> Text -> IndexHit
hitIn name modu pkg = IndexHit name modu pkg ""

spec :: Spec
spec = describe "world facts versus session facts (D3)" $ do
    describe "an import claim comes from the notebook, not from a package" $ do
        it "a cell that imports the module is named, with how it ran" $
            property $ \(Modul m) (Ident n) (Pkg p) (Positive cid) ->
                let notebook = nb [code cid ("import " <> m)]
                    facts = notebookFacts notebook n (Just m)
                    txt = answerFor n (Just m) (WorldHit (hitIn n m p) Declared) facts
                 in txt `shouldSatisfy` T.isInfixOf ("cell " <> T.pack (show cid))

        it "no import instruction is offered when a cell already imports it" $
            property $ \(Modul m) (Ident n) (Pkg p) (Positive cid) ->
                let notebook = nb [code cid ("import " <> m)]
                    facts = notebookFacts notebook n (Just m)
                    txt = answerFor n (Just m) (WorldHit (hitIn n m p) Declared) facts
                 in txt `shouldSatisfy` (not . T.isInfixOf "Add this import")

        it "an absent import is stated as the notebook's fact and acted on" $
            property $ \(Modul m) (Ident n) (Pkg p) ->
                let facts = notebookFacts (nb []) n (Just m)
                    txt = answerFor n (Just m) (WorldHit (hitIn n m p) Declared) facts
                 in txt `shouldSatisfy` T.isInfixOf ("import " <> m)

        it "a builtin's import is withheld when a cell already imports it" $
            property $ \(Modul m) (Ident n) (Positive cid) ->
                let facts = notebookFacts (nb [code cid ("import " <> m)]) n (Just m)
                    txt = answerFor n (Just m) (WorldBuiltin n (Just m)) facts
                 in txt `shouldSatisfy` (not . T.isInfixOf "Add this import")

        it "a builtin's import is offered when no cell imports it" $
            property $ \(Modul m) (Ident n) ->
                let facts = notebookFacts (nb []) n (Just m)
                    txt = answerFor n (Just m) (WorldBuiltin n (Just m)) facts
                 in txt `shouldSatisfy` T.isInfixOf ("import " <> m)

        it "a qualified import counts as importing its module" $
            property $ \(Modul m) (Ident n) (Positive cid) ->
                let src = "import qualified " <> m <> " as Q"
                    facts = notebookFacts (nb [code cid src]) n (Just m)
                 in map cfCell (nfImports facts) === [cid]

        it "the clause speaks about the module the hit was found in" $
            property $ \(Modul asked) (Modul found) (Ident n) (Pkg p) ->
                asked /= found ==>
                    let a = WorldHit (hitIn n found p) Declared
                     in answerModule a (Just asked) === Just found

        it "an unqualified query is answered against the hit's own module" $
            property $ \(Modul m) (Ident n) (Pkg p) (Positive cid) ->
                let notebook = nb [code cid ("import " <> m)]
                    a = WorldHit (hitIn n m p) Declared
                    facts = notebookFacts notebook n (answerModule a Nothing)
                    txt = answerFor n Nothing a facts
                 in txt `shouldSatisfy` T.isInfixOf ("cell " <> T.pack (show cid))

    describe "the notebook never claims a name is in scope" $ do
        it "no rendered clause says 'in scope'" $
            property $ \(Modul m) (Ident n) (Positive cid) ->
                let facts = notebookFacts (nb [code cid ("import " <> m)]) n (Just m)
                 in T.toLower (renderNotebookFacts n (Just m) facts)
                        `shouldSatisfy` (not . T.isInfixOf "in scope")

        it "a cell edited since its last run is reported as not run" $
            property $ \(Modul m) (Ident n) (Positive cid) ->
                let c = (code cid ("import " <> m)){cellDirty = True}
                    facts = notebookFacts (nb [c]) n (Just m)
                 in map cfRan (nfImports facts) === [NotRun]

        it "a cell that errored is reported as failed" $
            property $ \(Modul m) (Ident n) (Positive cid) ->
                let c = (code cid ("import " <> m)){cellError = Just "boom"}
                    facts = notebookFacts (nb [c]) n (Just m)
                 in map cfRan (nfImports facts) === [RanFailed]

        it "cellRan reads only the cell it is given" $
            cellRan (code 1 "x = 1") `shouldBe` RanClean

    describe "the notebook's own definitions are found" $
        it "a top-level binding names its cell" $
            property $ \(Ident n) (Positive cid) ->
                let facts = notebookFacts (nb [code cid (n <> " = 1")]) n Nothing
                 in map cfCell (nfDefines facts) === [cid]

    describe "consulted is the trace of what ran, never a constant" $ do
        it "a miss names exactly the sources it was given" $
            property $ \(Ident n) (NonEmpty srcs) ->
                let names = map (T.pack . take 6) (take 3 srcs) :: [Text]
                    facts = notebookFacts (nb []) n Nothing
                    (ps, _) =
                        indexAnswerPairs
                            (QualifiedName n Nothing "")
                            (WorldMiss names)
                            facts
                 in listAt "consulted" ps === Just names

        it "every answer composed from the notebook says it read it" $
            property $ \(Modul m) (Ident n) (Pkg p) ->
                let facts = notebookFacts (nb []) n (Just m)
                    readNotebook' a =
                        maybe
                            False
                            (elem "notebook source")
                            (listAt "consulted" (fst (pairsFor n (Just m) a facts)))
                 in conjoin
                        [ counterexample (show a) (property (readNotebook' a))
                        | a <-
                            [ WorldHit (hitIn n m p) Declared
                            , WorldHit (hitIn n m p) Undeclared
                            , WorldBuiltin n (Just m)
                            , WorldBuiltin n Nothing
                            ]
                        ]

        it "the miss prose names the same sources the field does" $
            property $ \(Ident n) ->
                let facts = notebookFacts (nb []) n Nothing
                    txt = answerFor n Nothing (WorldMiss trace) facts
                 in conjoin [property (T.isInfixOf s txt) | s <- trace]

    describe "the world field is emitted only from a computed lookup" $ do
        it "a miss says so rather than going silent" $
            property $ \(Ident n) ->
                let facts = notebookFacts (nb []) n Nothing
                    (ps, _) = pairsFor n Nothing (WorldMiss trace) facts
                 in boolAt "world" "found" ps === Just False

        it "a hit carries the module and package it was found in" $
            property $ \(Modul m) (Ident n) (Pkg p) ->
                let facts = notebookFacts (nb []) n (Just m)
                    a = WorldHit (hitIn n m p) Undeclared
                    (ps, _) = pairsFor n (Just m) a facts
                 in textAt "world" "package" ps === Just p

        it "a builtin's module is a fact, and no session claim rides on it" $
            property $ \(Modul m) (Ident n) ->
                let facts = notebookFacts (nb []) n (Just m)
                    (ps, txt) = pairsFor n (Just m) (WorldBuiltin n (Just m)) facts
                 in textAt "world" "module" ps === Just m
                        .&&. property (not ("in scope" `T.isInfixOf` T.toLower txt))

    describe "the index is asked for the bare name (D2 at the lookup seam)" $ do
        it "a qualified spelling finds what the bare spelling finds" $ do
            let env = notebookAliasEnv (nb [code 1 "import qualified Zz.Yy as Q"])
                qn = resolveSpelling env "Q.widget"
                hit = HoogleHit "widget" "zzpkg" "Zz.Yy" "Int -> Int" "" ""
            found <- indexLookupWith (\_ _ -> pure [hit]) (qnModule qn) (qnBare qn)
            fmap ihName found `shouldBe` Just "widget"

        it "the module the qualifier named narrows a shared name" $ do
            let here = HoogleHit "widget" "here" "Aa.Bb" "Int" "" ""
                there = HoogleHit "widget" "there" "Cc.Dd" "Int" "" ""
            found <- indexLookupWith (\_ _ -> pure [there, here]) (Just "Aa.Bb") "widget"
            fmap ihPackage found `shouldBe` Just "here"

        it "a module with nothing of that name widens rather than lying" $ do
            let there = HoogleHit "widget" "there" "Cc.Dd" "Int" "" ""
            found <- indexLookupWith (\_ _ -> pure [there]) (Just "Aa.Bb") "widget"
            fmap ihPackage found `shouldBe` Just "there"

        it "the qualifier's module beats the hit the ranker prefers unaided" $
            property $ \(Modul outer) (Modul inner) (Pkg p) (Ident n) ->
                let scoped = outer <> "." <> inner
                    hIn = HoogleHit n (p <> p) scoped "" "" ""
                    hOut = HoogleHit n p outer "" "" ""
                 in rankResolveTopK 1 n Nothing [hIn, hOut] == [(p, outer)] ==>
                        ioProperty $ do
                            found <-
                                indexLookupWith
                                    (\_ _ -> pure [hIn, hOut])
                                    (Just scoped)
                                    n
                            pure (fmap ihPackage found === Just (p <> p))

listAt :: Text -> [(K.Key, Value)] -> Maybe [Text]
listAt k ps = case lookup (K.fromText k) ps of
    Just (Array xs) -> Just [t | String t <- foldr (:) [] xs]
    _ -> Nothing

boolAt :: Text -> Text -> [(K.Key, Value)] -> Maybe Bool
boolAt outer inner ps = case nested outer inner ps of
    Just (Bool b) -> Just b
    _ -> Nothing

textAt :: Text -> Text -> [(K.Key, Value)] -> Maybe Text
textAt outer inner ps = case nested outer inner ps of
    Just (String s) -> Just s
    _ -> Nothing

nested :: Text -> Text -> [(K.Key, Value)] -> Maybe Value
nested outer inner ps = case lookup (K.fromText outer) ps of
    Just (Object o) -> KM.lookup (K.fromText inner) o
    _ -> Nothing
