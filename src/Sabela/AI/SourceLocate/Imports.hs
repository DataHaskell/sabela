{-# LANGUAGE OverloadedStrings #-}

{- | The alias map a module's import block declares (qualifier -> module),
found by the same ladder as declarations: the parser first, then a lexical
scan for files the parser refuses (CPP and friends).
-}
module Sabela.AI.SourceLocate.Imports (
    aliasesJson,
    moduleAliases,
) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as K
import Data.Char (isUpper)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified GHC.Hs as Hs
import GHC.Parser.Lexer (ParseResult (..))
import GHC.Types.SrcLoc (unLoc)
import qualified Language.Haskell.GhclibParserEx.GHC.Parser as P

import Sabela.AI.SourceLocate (fileDynFlags)

-- | Alias -> module pairs, in import order; only imports that state an alias.
moduleAliases :: Text -> [(Text, Text)]
moduleAliases src = case P.parseModule (T.unpack src) (fileDynFlags src) of
    POk _ lmod -> mapMaybe aliasOf (Hs.hsmodImports (unLoc lmod))
    PFailed _ -> lexicalAliases src
  where
    aliasOf li = do
        let decl = unLoc li
        la <- Hs.ideclAs decl
        let alias = moduleText (unLoc la)
            modul = moduleText (unLoc (Hs.ideclName decl))
        pure (alias, modul)
    moduleText = T.pack . Hs.moduleNameString

{- | The aliases whose qualifiers the shown text actually uses, as the
payload object; Nothing when none apply, so the field can stay absent.
-}
aliasesJson :: Text -> Text -> Maybe Value
aliasesJson src shown
    | null used = Nothing
    | otherwise = Just (object [K.fromText a .= m | (a, m) <- used])
  where
    used = take 4 [p | p@(a, _) <- moduleAliases src, usesQualifier a]
    usesQualifier a = (a <> ".") `T.isInfixOf` shown

{- | Aliased imports read lexically: optional (or postpositive) @qualified@,
then @as@; enough for the CPP-ridden files the parser refuses.
-}
lexicalAliases :: Text -> [(Text, Text)]
lexicalAliases src =
    [ (alias, modul)
    | l <- map T.strip (T.lines src)
    , Just rest <- [T.stripPrefix "import " l]
    , Just (modul, alias) <- [aliasWords (T.words rest)]
    ]
  where
    aliasWords ws = case dropWhile (== "qualified") ws of
        (m : rest) -> case dropWhile (== "qualified") rest of
            ("as" : a : _)
                | plausible m && plausible (cleanAlias a) ->
                    Just (m, cleanAlias a)
            _ -> Nothing
        [] -> Nothing
    cleanAlias = T.takeWhile (/= '(')
    plausible t = maybe False (isUpper . fst) (T.uncons t)
