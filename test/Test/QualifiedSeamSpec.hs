{-# LANGUAGE OverloadedStrings #-}

{- | D2: one qualifier/alias seam. A qualified spelling resolves to the bare
name the index is keyed on, and the module the qualifier named. Stated over
arbitrary aliases, modules and identifiers.
-}
module Test.QualifiedSeamSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.QualifiedName (
    AliasEnv (..),
    QualifiedName (..),
    aliasEnvFromSource,
    emptyAliasEnv,
    lookupSpelling,
    resolveLookup,
    resolveName,
    resolveSpelling,
 )

newtype Ident = Ident Text
    deriving (Show)

instance Arbitrary Ident where
    arbitrary = Ident <$> genIdent

{- | A name whose last component is upper-headed: a type, class or data
constructor, the shape a module path is ambiguous with.
-}
newtype TypeIdent = TypeIdent Text
    deriving (Show)

instance Arbitrary TypeIdent where
    arbitrary = TypeIdent <$> genUpperWord

newtype Modul = Modul Text
    deriving (Show)

instance Arbitrary Modul where
    arbitrary = do
        parts <- resize 3 (listOf1 genUpperWord)
        pure (Modul (T.intercalate "." parts))

newtype Alias = Alias Text
    deriving (Show)

instance Arbitrary Alias where
    arbitrary = Alias <$> genUpperWord

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

envWith :: Text -> Text -> AliasEnv
envWith alias modu = AliasEnv [(alias, modu)] [modu]

spec :: Spec
spec = describe "the shared qualifier seam (D2)" $ do
    describe "an alias resolves to the bare name and its module" $ do
        it "the index is asked for the bare name, never the spelling" $
            property $ \(Alias a) (Modul m) (Ident n) ->
                a /= m ==>
                    qnBare (resolveName (envWith a m) (a <> "." <> n)) === n

        it "the module the alias names is carried on the answer" $
            property $ \(Alias a) (Modul m) (Ident n) ->
                a /= m ==>
                    qnModule (resolveName (envWith a m) (a <> "." <> n)) === Just m

        it "the resolution is disclosed, never silent" $
            property $ \(Alias a) (Modul m) (Ident n) ->
                a /= m ==>
                    qnNote (resolveName (envWith a m) (a <> "." <> n))
                        `shouldSatisfy` T.isInfixOf a

    describe "an unqualified name is left alone" $
        it "no module is invented for a bare identifier" $
            property $ \(Ident n) ->
                resolveName emptyAliasEnv n === QualifiedName n Nothing ""

    describe "an unrecognised qualifier" $ do
        it "still yields the bare name the index can answer" $
            property $ \(Alias a) (Ident n) ->
                qnBare (resolveName emptyAliasEnv (a <> "." <> n)) === n

        it "claims no module and says why" $
            property $ \(Alias a) (Ident n) ->
                let qn = resolveName emptyAliasEnv (a <> "." <> n)
                 in qnModule qn === Nothing
                        .&&. property (not (T.null (qnNote qn)))

    describe "a dotted qualifier is a module in its own right" $
        it "is taken as the module when the notebook did not alias it" $
            property $ \(Modul m) (Ident n) ->
                T.any (== '.') m ==>
                    qnModule (resolveName emptyAliasEnv (m <> "." <> n)) === Just m

    describe "a qualified type, class or constructor name" $ do
        it "the parity-bearing split leaves the ambiguous spelling alone" $
            property $ \(Alias a) (Modul m) (TypeIdent n) ->
                a /= m ==>
                    let t = a <> "." <> n
                     in resolveName (envWith a m) t === QualifiedName t Nothing ""

        it "the lookup seam splits it when the notebook aliased the qualifier" $
            property $ \(Alias a) (Modul m) (TypeIdent n) ->
                a /= m ==>
                    let qn = resolveLookup (envWith a m) (a <> "." <> n)
                     in (qnBare qn, qnModule qn) === (n, Just m)

        it "a module imported under its own name settles it too" $
            property $ \(Modul m) (TypeIdent n) ->
                let qn = resolveLookup (AliasEnv [] [m]) (m <> "." <> n)
                 in (qnBare qn, qnModule qn) === (n, Just m)

        it "the split is disclosed, never silent" $
            property $ \(Alias a) (Modul m) (TypeIdent n) ->
                a /= m ==>
                    qnNote (resolveLookup (envWith a m) (a <> "." <> n))
                        `shouldSatisfy` T.isInfixOf a

        it "a qualifier the notebook never imported stays unsplit" $
            property $ \(Alias a) (TypeIdent n) ->
                let t = a <> "." <> n
                 in resolveLookup emptyAliasEnv t === QualifiedName t Nothing ""

        it "the lookup seam never contradicts the parity-bearing one" $
            property $ \(Alias a) (Modul m) (Ident n) ->
                a /= m ==>
                    let t = a <> "." <> n
                     in resolveLookup (envWith a m) t === resolveName (envWith a m) t

        it "an operator section resolves through the lookup seam as well" $
            lookupSpelling (envWith "M" "Some.Module") "(M.<+>)"
                `shouldBe` resolveSpelling (envWith "M" "Some.Module") "(M.<+>)"

    describe "written spellings resolve the same as bare ones" $ do
        it "an operator section resolves through its qualifier" $
            resolveSpelling (envWith "M" "Some.Module") "(M.<+>)"
                `shouldBe` QualifiedName
                    "(<+>)"
                    (Just "Some.Module")
                    "alias M = Some.Module (notebook import)"

        it "a unit-id prefix is not part of the name" $
            property $ \(Ident n) ->
                qnBare (resolveSpelling emptyAliasEnv ("pkg-1.0-abc:" <> n)) === n

        it "agrees with resolveName on undecorated spellings" $
            property $ \(Alias a) (Modul m) (Ident n) ->
                let env = envWith a m
                    t = a <> "." <> n
                 in resolveSpelling env t === resolveName env t

    describe "the alias environment is read from the notebook's own imports" $ do
        it "a qualified import establishes its alias" $
            property $ \(Alias a) (Modul m) ->
                a /= m ==>
                    aeAliases
                        (aliasEnvFromSource ("import qualified " <> m <> " as " <> a))
                        === [(a, m)]

        it "a plain import establishes its module" $
            property $ \(Modul m) ->
                aeModules (aliasEnvFromSource ("import " <> m)) === [m]

        it "a name qualified by an imported module resolves end to end" $
            property $ \(Alias a) (Modul m) (Ident n) ->
                a /= m ==>
                    let env =
                            aliasEnvFromSource
                                ("import qualified " <> m <> " as " <> a)
                     in resolveName env (a <> "." <> n)
                            === QualifiedName
                                n
                                (Just m)
                                ("alias " <> a <> " = " <> m <> " (notebook import)")
