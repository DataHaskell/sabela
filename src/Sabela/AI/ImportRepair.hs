{-# LANGUAGE OverloadedStrings #-}

{- | Auto-repair for a wrong module name — the measured #1 weak-model failure
(it guesses @import Data.Frame@ when the module is @DataFrame@). GHC states the
correction ("Perhaps you meant DataFrame"), so the rename is a fact, not a guess —
the deterministic sibling of "Sabela.AI.DepRepair"/"Sabela.AI.ExtRepair", driven by
the same chokepoint repair loop. The package the module needs is added by the dep
fixer (extended to read GHC's "needs flag -package-id" note).
-}
module Sabela.AI.ImportRepair (
    moduleRenameFix,
    renameModule,
    addImport,
    addScopedImport,
) where

import Data.Char (isAlphaNum)
import Data.List (findIndex)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (misnamedModule)
import Sabela.Model (CellError (..))

{- | Correct a wrong module name in the cell source from GHC's "Could not find
module X. Perhaps you meant Y" — rename X→Y. Nothing when the run did not fail on a
correctable module name, or the rename would not change the source.
-}
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

{- | Replace a module name as a whole dotted token, so @Data.Frame@ is renamed but
@Data.Frame.TH.Records@ (a different, longer token) is left alone.
-}
renameModule :: Text -> Text -> Text -> Text
renameModule wrong right src
    | T.null wrong = src
    | otherwise = T.concat (map sub (T.groupBy sameClass src))
  where
    sub tok = if tok == wrong then right else tok
    sameClass a b = isModChar a == isModChar b
    isModChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''

{- | Insert @import \<modul\>@ into a cell, after the last existing @import@ line
(or after the leading @-- cabal:@/comment block when the cell imports nothing).
A no-op when the module is already imported (plain or qualified).
-}
addImport :: Text -> Text -> Text
addImport modul src
    | T.null modul = src
    | modul `elem` importedModules src = src
    | otherwise = insertImport ("import " <> modul) src

{- | Insert a SCOPED @import \<modul\> (\<name\>)@ naming just the symbol the
repair needs, after the last import / @-- cabal:@ block. An operator name is
wrapped in parens — @import M ((=~))@. A no-op when that exact scoped import is
already present. Scoped imports kill the name/class-collision layer, but
INSTANCES are not import-scopable, so the dep verify-and-revert is the
load-bearing soundness fix; this only narrows what a wrong pick can shadow.
-}
addScopedImport :: Text -> Text -> Text -> Text
addScopedImport modul name src
    | T.null modul || T.null name = src
    | importLine `elem` map T.stripStart (T.lines src) = src
    | otherwise = insertImport importLine src
  where
    importLine = "import " <> modul <> " (" <> entity <> ")"
    entity = if isOperator name then "(" <> name <> ")" else name
    isOperator = T.all (`elem` operatorChars)

-- | The characters a Haskell operator identifier is built from.
operatorChars :: String
operatorChars = "!#$%&*+./<=>?@\\^|-~:"

-- | Insert a fully-formed import line after the last import / preamble block.
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

-- | The whole-module name of every import line (plain or qualified).
importedModules :: Text -> [Text]
importedModules = concatMap moduleTokens . T.lines
  where
    moduleTokens l = case T.words (T.stripStart l) of
        ("import" : "qualified" : m : _) -> [m]
        ("import" : m : _) -> [m]
        _ -> []

-- | The greatest index whose element satisfies the predicate, if any.
lastIndex :: (a -> Bool) -> [a] -> Maybe Int
lastIndex p xs =
    fmap (\i -> length xs - 1 - i) (findIndex p (reverse xs))
