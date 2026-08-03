{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | What a block of injected source does to the scope it is injected into:
which names it resolves from that scope, and which modules it adds to it
unqualified. Both are ways a user's own import can be broken by the harness.
-}
module Test.PreludeScope (
    capturableNames,
    definedNames,
    unqualifiedImports,
    capturedBy,
    identifiersIn,
    qualifiedReferencesIn,
    unparseableMarker,
) where

import qualified Data.Char as Char
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified GHC.Hs as Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (Unqual))
import GHC.Types.SrcLoc (unLoc)

import Sabela.Parse (parseCellModule)
import Sabela.Parse.Ast (collectBinders, topLevelDefsFromDecl)

{- | Stands in the answer when the source could not be parsed, so a checker
that could not look never reports a clean result.
-}
unparseableMarker :: Text
unparseableMarker = "<source did not parse>"

{- | The value, constructor and type-constructor names this source looks up
without a qualifier and does not bind itself. Binders are collected per
declaration, and an instance's methods bind nothing: a method's own name in
its body resolves to the class, which is ambient.
-}
capturableNames :: Text -> [Text]
capturableNames src = case parsedInjection src of
    Nothing -> [unparseableMarker]
    Just m ->
        S.toAscList
            ( S.unions
                [ lookedUp d `S.difference` (moduleBinders m `S.union` localBinders d)
                | d <- map unLoc (Hs.hsmodDecls m)
                ]
            )

-- | The names this source itself brings into the scope it is injected into.
definedNames :: Text -> [Text]
definedNames src = case parsedInjection src of
    Nothing -> [unparseableMarker]
    Just m -> S.toAscList (moduleBinders m)

{- | Injected code is not always a module: a preamble is a list of statements.
A source that does not parse whole is re-read a line at a time, each statement
that is not already a declaration bound to a fresh name, which leaves the
names it resolves exactly as they were.
-}
parsedInjection :: Text -> Maybe (Hs.HsModule Hs.GhcPs)
parsedInjection src = case parseCellModule src of
    Just m -> Just m
    Nothing -> parseCellModule (T.unlines (zipWith asDecl [0 :: Int ..] (T.lines src)))
  where
    asDecl i l
        | T.null (T.strip l) = l
        | isJust (parseCellModule l) = l
        | otherwise = "_sabelaStatement" <> T.pack (show i) <> " = " <> l

{- | The modules this source adds to the surrounding scope unqualified. Each
one is permanent and unhideable: a later @hiding@ clause of the user's own
cannot take back a name this import keeps supplying.
-}
unqualifiedImports :: Text -> [Text]
unqualifiedImports src = case parsedInjection src of
    Nothing -> [unparseableMarker]
    Just m -> sort [n | i <- map unLoc (Hs.hsmodImports m), Just n <- [bare i]]
  where
    bare i
        | Hs.ideclQualified i /= Hs.NotQualified = Nothing
        | importsNothing i = Nothing
        | otherwise = Just (T.pack (Hs.moduleNameString (unLoc (Hs.ideclName i))))
    importsNothing i = case Hs.ideclImportList i of
        Just (Hs.Exactly, names) -> null (unLoc names)
        _ -> False

{- | The names of an ambient module that this source can take away from the
user who imported it, given what the session had already bound at the prompt
when the source went in: one of those bindings, which shadows the import
whichever came first; one the source resolves itself; or any of them, once the
source holds an unqualified import the user's @hiding@ clause cannot outrank.
The last arm is an over-approximation: the imported module's export list is
not known here, so every ambient name is reported as at risk.
-}
capturedBy :: [Text] -> [Text] -> Text -> [Text]
capturedBy promptBindings ambientExports src = uniq (shadowed <> reached)
  where
    shadowed =
        [n | n <- ambientExports, n `elem` (promptBindings <> definedNames src)]
    reached
        | not (null (unqualifiedImports src)) = ambientExports
        | otherwise = [n | n <- ambientExports, n `elem` capturableNames src]
    uniq = S.toAscList . S.fromList

lookedUp :: Hs.HsDecl Hs.GhcPs -> Set Text
lookedUp d = S.fromList (exprNames <> patConNames <> tyConNames)
  where
    exprNames =
        [ n
        | Hs.HsVar _ ln <- universeBi d :: [Hs.HsExpr Hs.GhcPs]
        , Just n <- [unqualified (unLoc ln)]
        ]
    patConNames =
        [ n
        | Hs.ConPat{Hs.pat_con = lc} <- universeBi d :: [Hs.Pat Hs.GhcPs]
        , Just n <- [unqualified (unLoc lc)]
        ]
    tyConNames =
        [ n
        | Hs.HsTyVar _ _ ln <- universeBi d :: [Hs.HsType Hs.GhcPs]
        , Just n <- [unqualified (unLoc ln)]
        , startsUpper n
        ]

{- | The names the module as a whole brings into being: top-level binders and
the record selectors its declarations declare.
-}
moduleBinders :: Hs.HsModule Hs.GhcPs -> Set Text
moduleBinders m =
    S.unions
        ( fieldBinders m
            : map (topLevelDefsFromDecl . unLoc) (Hs.hsmodDecls m)
        )

{- | The binders one declaration makes for its own body: its patterns, and
whatever its @where@ and @let@ blocks bind. Method bindings of an instance are
not among them, which is why they are reached through the declaration's local
binds rather than through the declaration itself.
-}
localBinders :: Hs.HsDecl Hs.GhcPs -> Set Text
localBinders d = S.union patNames whereNames
  where
    patNames =
        S.fromList
            [ n
            | Hs.VarPat _ ln <- universeBi d :: [Hs.Pat Hs.GhcPs]
            , Just n <- [unqualified (unLoc ln)]
            ]
    whereNames =
        S.unions
            [ collectBinders b
            | b <- universeBi d :: [Hs.HsLocalBinds Hs.GhcPs]
            ]

-- | The record selectors a declaration in this source brings into being.
fieldBinders :: Hs.HsModule Hs.GhcPs -> Set Text
fieldBinders m =
    S.fromList
        [ n
        | fld <- universeBi m :: [Hs.HsConDeclRecField Hs.GhcPs]
        , rdr <- universeBi (Hs.cdrf_names fld) :: [RdrName]
        , Just n <- [unqualified rdr]
        ]

unqualified :: RdrName -> Maybe Text
unqualified = \case
    Unqual occ -> Just (T.pack (occNameString occ))
    _ -> Nothing

startsUpper :: Text -> Bool
startsUpper t = case T.uncons t of
    Just (c, _) -> Char.isUpper c
    Nothing -> False

{- | Every identifier-shaped token in a source's code positions, the pool a
generated module draws its export list from so the collision case is actually
reached. String literals are excluded: their contents are data, not names.
-}
identifiersIn :: Text -> [Text]
identifiersIn src =
    S.toAscList
        (S.fromList [t | t <- T.split (not . identChar) (outsideLiterals src), isIdent t])
  where
    isIdent t = case T.uncons t of
        Just (c, _) -> Char.isAlpha c
        Nothing -> False

{- | The lower-case names this source reaches through a qualifier — the ambient
names it depends on, whatever alias it happens to reach them by. A module
exporting these is the module that would have broken it unqualified.
-}
qualifiedReferencesIn :: Text -> [Text]
qualifiedReferencesIn src =
    S.toAscList
        (S.fromList (concatMap afterQualifier (T.lines (outsideLiterals src))))
  where
    afterQualifier line =
        [ name
        | tok <- T.split (not . qualChar) line
        , let (qualifier, name) = T.breakOnEnd "." tok
        , startsUpper qualifier
        , not (T.null name)
        , not (startsUpper name)
        ]
    qualChar c = identChar c || c == '.'

identChar :: Char -> Bool
identChar c = Char.isAlphaNum c || c == '_' || c == '\''

outsideLiterals :: Text -> Text
outsideLiterals = T.pack . go False . T.unpack
  where
    go _ [] = []
    go inLit ('\\' : c : rest) = (if inLit then ' ' else c) : go inLit rest
    go inLit ('"' : rest) = ' ' : go (not inLit) rest
    go True (_ : rest) = ' ' : go True rest
    go False (c : rest) = c : go False rest
