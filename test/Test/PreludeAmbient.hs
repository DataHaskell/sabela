{-# LANGUAGE OverloadedStrings #-}

{- | A module a user's cell might import unqualified, generated rather than
named: the property is over any module whose exports collide with the
harness's, not over the one library an episode happened to use.
-}
module Test.PreludeAmbient (
    AmbientModule (..),
    ambientSource,
    collidingNames,
    genIdent,
    keywords,
) where

import qualified Data.Char as Char
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Sabela.Output (displayPrelude)
import Test.PreludeScope (
    capturableNames,
    definedNames,
    identifiersIn,
    qualifiedReferencesIn,
    unparseableMarker,
 )

data AmbientModule = AmbientModule
    { ambientName :: Text
    , ambientExports :: [Text]
    }
    deriving (Show)

{- | Exports are drawn from the identifiers the harness's own injected code
mentions as well as from fresh ones, so the generator reaches the colliding
shape rather than only names nothing could clash with.
-}
instance Arbitrary AmbientModule where
    arbitrary =
        AmbientModule
            <$> genModuleName
            <*> (nub <$> listOf (oneof [elements pool, genIdent]))
      where
        pool = [t | t <- identifiersIn displayPrelude, t `notElem` keywords]

{- | The ambient names the harness's own injected code depends on, counted the
same way whichever way the code reaches them: through a qualifier, or bare
without binding them. A module exporting these is the module that meets the
injection in scope, so the corpus does not shrink when the injection stops
qualifying — which is what makes a probe over it able to fail.
-}
collidingNames :: Text -> [Text]
collidingNames src =
    [ n
    | n <- nub (qualifiedReferencesIn src <> capturableNames src)
    , startsLower n
    , n `notElem` keywords
    , n `notElem` definedNames src
    , n /= unparseableMarker
    ]

startsLower :: Text -> Bool
startsLower t = case T.uncons t of
    Just (c, _) -> Char.isLower c
    Nothing -> False

{- | Compilable source for the module. @NoImplicitPrelude@ so that a name it
exports to collide with the Prelude does not first collide inside it.
-}
ambientSource :: AmbientModule -> Text
ambientSource m =
    T.unlines
        ( [ "{-# LANGUAGE NoImplicitPrelude #-}"
          , "module " <> ambientName m <> " (" <> T.intercalate ", " names <> ") where"
          , "import qualified Data.Int"
          ]
            <> concat
                [ [n <> " :: Data.Int.Int", n <> " = 1"]
                | n <- names
                ]
        )
  where
    names = nub (ambientExports m)

genModuleName :: Gen Text
genModuleName = do
    n <- genIdent
    pure ("M" <> T.toUpper (T.take 1 n) <> T.drop 1 n)

genIdent :: Gen Text
genIdent =
    (T.pack <$> ((:) <$> elements ['a' .. 'z'] <*> listOf identChar))
        `suchThat` (`notElem` keywords)
  where
    identChar = elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")

keywords :: [Text]
keywords =
    [ "case"
    , "class"
    , "data"
    , "deriving"
    , "do"
    , "else"
    , "foreign"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "then"
    , "type"
    , "where"
    ]
