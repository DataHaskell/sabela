{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ImportRepair (
    moduleRenameFix,
    renameModule,
    addImport,
    addScopedImport,
    addQualifiedImport,
    qualifiedAliases,
    qualifiedImports,
    importedAliasMisses,
    unboundAliasUses,
) where

import Data.Char (isAlphaNum)
import Data.List (findIndex)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (misnamedModule)
import Sabela.Model (CellError (..))

moduleRenameFix :: Either Text ExecutionResult -> Text -> Maybe Text
moduleRenameFix res src = do
    (wrong, right) <- misnamedFromResult res
    let src' = renameModule wrong right src
    if src' == src then Nothing else Just src'

misnamedFromResult :: Either Text ExecutionResult -> Maybe (Text, Text)
misnamedFromResult (Left _) = Nothing
misnamedFromResult (Right er) =
    listToMaybe (mapMaybe misnamedModule errorTexts)
  where
    errorTexts = maybeToList (erError er) ++ map ceMessage (erErrors er)

renameModule :: Text -> Text -> Text -> Text
renameModule wrong right src
    | T.null wrong = src
    | otherwise = T.concat (map sub (T.groupBy sameClass src))
  where
    sub tok = if tok == wrong then right else tok
    sameClass a b = isModChar a == isModChar b
    isModChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''

addImport :: Text -> Text -> Text
addImport modul src
    | T.null modul = src
    | modul `elem` importedModules src = src
    | otherwise = insertImport ("import " <> modul) src

addScopedImport :: Text -> Text -> Text -> Text
addScopedImport modul name src
    | T.null modul || T.null name = src
    | importLine `elem` map T.stripStart (T.lines src) = src
    | otherwise = insertImport importLine src
  where
    importLine = "import " <> modul <> " (" <> entity <> ")"
    entity = if isOperator name then "(" <> name <> ")" else name
    isOperator = T.all (`elem` operatorChars)

qualifiedNotInScope :: Text -> [(Text, Text)]
qualifiedNotInScope err =
    nubOrd
        [ (alias, name)
        | qualified <- concatMap quotedTokens (notInScopeLines err)
        , let (dotted, name) = T.breakOnEnd "." qualified
              alias = T.dropEnd 1 dotted
        , not (T.null alias)
        , not (T.null name)
        ]
  where
    notInScopeLines = filter ("Not in scope:" `T.isInfixOf`) . T.lines
    nubOrd = foldr (\x acc -> x : filter (/= x) acc) []

unboundAliasUses :: Text -> [(Text, Text)]
unboundAliasUses err =
    [ p | p@(alias, _) <- qualifiedNotInScope err, alias `elem` unimportedAliases err
    ]

importedAliasMisses :: Text -> [(Text, Text)]
importedAliasMisses err =
    [ p
    | p@(alias, _) <- qualifiedNotInScope err
    , alias `notElem` unimportedAliases err
    ]

unimportedAliases :: Text -> [Text]
unimportedAliases err =
    concat
        [ quotedTokens l
        | l <- T.lines err
        , "No module named" `T.isInfixOf` l
        , "is imported" `T.isInfixOf` l
        ]

quotedTokens :: Text -> [Text]
quotedTokens = go
  where
    go t = case T.breakOn "\8216" t of
        (_, rest)
            | T.null rest -> []
            | otherwise ->
                let (tok, after) = T.breakOn "\8217" (T.drop 1 rest)
                 in if T.null after then [] else tok : go (T.drop 1 after)

addQualifiedImport :: Text -> Text -> Text -> Text
addQualifiedImport modul alias src
    | T.null modul || T.null alias = src
    | (modul, alias) `elem` qualifiedImports src = src
    | otherwise = insertImport importLine src
  where
    importLine = "import qualified " <> modul <> " as " <> alias

qualifiedImports :: Text -> [(Text, Text)]
qualifiedImports src =
    [ (modul, alias)
    | l <- map T.strip (T.lines src)
    , "import qualified " `T.isPrefixOf` l
    , let (before, rest) = T.breakOn " as " l
    , not (T.null rest)
    , modul <- take 1 (reverse (T.words before))
    , alias <- take 1 (T.words (T.drop 4 rest))
    ]

qualifiedAliases :: Text -> [Text]
qualifiedAliases = map snd . qualifiedImports

operatorChars :: String
operatorChars = "!#$%&*+./<=>?@\\^|-~:"

insertImport :: Text -> Text -> Text
insertImport importLine src =
    T.intercalate "\n" (before ++ [importLine] ++ after) <> trailer
  where
    ls = T.lines src
    trailer = if "\n" `T.isSuffixOf` src then "\n" else ""
    (before, after) = splitAt insertAt ls
    insertAt = maybe afterPreamble (+ 1) (lastIndex isImportLine ls)
    afterPreamble = length (takeWhile isPreamble ls)
    isPreamble l = let s = T.strip l in T.null s || "--" `T.isPrefixOf` s
    isImportLine l = "import " `T.isPrefixOf` T.stripStart l

importedModules :: Text -> [Text]
importedModules = concatMap moduleTokens . T.lines
  where
    moduleTokens l = case T.words (T.stripStart l) of
        ("import" : "qualified" : m : _) -> [m]
        ("import" : m : _) -> [m]
        _ -> []

lastIndex :: (a -> Bool) -> [a] -> Maybe Int
lastIndex p xs =
    fmap (\i -> length xs - 1 - i) (findIndex p (reverse xs))
