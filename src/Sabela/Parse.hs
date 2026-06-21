{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | AST-based extraction of top-level definitions and free identifier uses
from a notebook cell's Haskell source.

This replaces the textual heuristic that lived in @Sabela.Topo@. We parse
each cell with @ghc-lib-parser@ (independent of the host GHC version) and
walk the resulting 'GHC.Hs.HsModule' to compute:

* @defs@ — top-level names introduced by the cell. Pulled from the LHS of
  every top-level declaration: function/value bindings, data/newtype/type
  /class names plus their data-constructors and class methods.

* @uses@ — every identifier referenced by the cell that is not bound
  somewhere within the cell itself. Computed as
  @(all 'HsVar' references) \\ (every binding-position name)@. This is a
  scope-conservative approximation: if the same name is bound locally
  (in a @where@, @let@, lambda, do-bind, list-comp generator, etc.) and
  /also/ used at top level, we treat it as bound and produce no
  dependency edge. Trade is intentional — it eliminates the cross-cell
  parameter-collision bug that the previous heuristic produced and that
  motivated this rewrite.

Cell sources are GHCi-style fragments, not modules. The pre-processor
strips @:set@ / @:type@ directives, drops @-- cabal:@ metadata lines, and
rewrites statement-form @let x = e@ and monadic @x \<- e@ into bare
declarations so the GHC parser will accept the result. The whole thing
is then wrapped in a synthetic @module M where { ... }@ and parsed with
'parseModule'. If that fails (incomplete code mid-edit), each chunk is
retried with 'parseDeclaration' and 'parseExpression' independently and
the parseable subset still contributes to @defs@/@uses@.
-}
module Sabela.Parse (
    CellSymbols (..),
    cellNames,
    cellSymbols,
) where

import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Driver.Session (
    DynFlags,
    defaultDynFlags,
    xopt_set,
 )
import qualified GHC.LanguageExtensions.Type as LE
import GHC.Parser.Lexer (ParseResult (..))
import GHC.Types.SrcLoc (GenLocated (..), unLoc)

import qualified Language.Haskell.GhclibParserEx.GHC.Parser as P
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import Sabela.Parse.Ast (
    CellSymbols (..),
    collectBinders,
    collectUses,
    declFreeVars,
    extractFromModule,
    topLevelDefsFromDecl,
 )

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

{- | Extract @(defs, uses)@ from a cell's Haskell source. A thin projection
of 'cellSymbols' for the many callers that only need those two sets.
-}
cellNames :: Text -> (Set Text, Set Text)
cellNames src = let s = cellSymbols src in (csDefs s, csUses s)

{- | Full symbol extraction for a cell: defs, uses, instance-provided method
names, and class-declared method names. See 'CellSymbols' for how the
extra channels feed typeclass reactivity.
-}
cellSymbols :: Text -> CellSymbols
cellSymbols src =
    let preprocessed = preprocess src
        moduleSrc =
            "module SabelaCell where\n"
                ++ T.unpack (T.unlines preprocessed)
     in case P.parseModule moduleSrc dynFlags of
            POk _ (L _ hsMod) -> extractFromModule hsMod
            PFailed _ ->
                let (defs, uses) = fallbackPerChunk preprocessed
                 in CellSymbols
                        { csDefs = defs
                        , csUses = uses
                        , csProvides = S.empty
                        , csClassMethods = S.empty
                        }

-- ---------------------------------------------------------------------------
-- Pre-processing: turn REPL fragments into a parseable module body
-- ---------------------------------------------------------------------------

{- | Drop GHCi directives and cabal-metadata lines. Rewrite statement-form
@let@ and monadic @\<-@ into top-level bindings so they parse as decls.
Returns the rewritten lines, ready to be glued together as the body of a
synthetic module.
-}
preprocess :: Text -> [Text]
preprocess src = concatMap rewriteLine (T.lines src)
  where
    rewriteLine raw
        | shouldDrop trimmed = []
        | Just rest <- T.stripPrefix "let " trimmed
        , noTopLevelIn rest =
            [reindent raw rest]
        | Just (binder, rhs) <- splitTopLevelArrow trimmed =
            [reindent raw (binder <> " = " <> rhs)]
        | otherwise = [raw]
      where
        trimmed = T.stripStart raw

    shouldDrop t =
        T.null t
            || ":" `T.isPrefixOf` t
            || "-- cabal:" `T.isPrefixOf` t
            || "--cabal:" `T.isPrefixOf` t

-- | Preserve original leading whitespace when rewriting a line's content.
reindent :: Text -> Text -> Text
reindent original newContent =
    let leading = T.takeWhile (\c -> c == ' ' || c == '\t') original
     in leading <> newContent

{- | A statement-form @let@ has no top-level @in@. We treat any line that
contains @ in @ at depth 0 as the regular expression-form @let ... in
...@ and leave it alone (let the parser handle it inside an expression
context if it ever ends up there).
-}
noTopLevelIn :: Text -> Bool
noTopLevelIn = go (0 :: Int) (0 :: Int) . T.unpack
  where
    -- Track paren depth and bracket depth. Stop at first top-level " in ".
    go _ _ [] = True
    go p b (' ' : 'i' : 'n' : ' ' : _) | p == 0 && b == 0 = False
    go p b ('(' : rest) = go (p + 1) b rest
    go p b (')' : rest) = go (max 0 (p - 1)) b rest
    go p b ('[' : rest) = go p (b + 1) rest
    go p b (']' : rest) = go p (max 0 (b - 1)) rest
    go p b (_ : rest) = go p b rest

{- | If the line is @ident <- rhs@ at top level, return @Just (ident, rhs)@.
We ignore @\<-@ that appears inside parens/brackets (list-comp generator,
do-block continuation, etc.).
-}
splitTopLevelArrow :: Text -> Maybe (Text, Text)
splitTopLevelArrow t =
    case findTopLevelArrow 0 0 (T.unpack t) of
        Nothing -> Nothing
        Just idx ->
            let (lhs, rhs0) = T.splitAt idx t
                rhs = T.drop 2 rhs0 -- drop "<-"
                lhsTrim = T.strip lhs
             in if isSimpleIdent lhsTrim
                    then Just (lhsTrim, T.stripStart rhs)
                    else Nothing
  where
    findTopLevelArrow :: Int -> Int -> String -> Maybe Int
    findTopLevelArrow _ _ [] = Nothing
    findTopLevelArrow p b ('<' : '-' : _) | p == 0 && b == 0 = Just 0
    findTopLevelArrow p b ('(' : rest) =
        succPos <$> findTopLevelArrow (p + 1) b rest
    findTopLevelArrow p b (')' : rest) =
        succPos <$> findTopLevelArrow (max 0 (p - 1)) b rest
    findTopLevelArrow p b ('[' : rest) =
        succPos <$> findTopLevelArrow p (b + 1) rest
    findTopLevelArrow p b (']' : rest) =
        succPos <$> findTopLevelArrow p (max 0 (b - 1)) rest
    findTopLevelArrow p b (_ : rest) = succPos <$> findTopLevelArrow p b rest

    succPos :: Int -> Int
    succPos = (+ 1)

isSimpleIdent :: Text -> Bool
isSimpleIdent t = case T.uncons t of
    Just (c, rest) ->
        (Char.isLower c || c == '_')
            && T.all isIdentChar rest
    Nothing -> False
  where
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

-- ---------------------------------------------------------------------------
-- Fallback: parse each chunk independently when full-module parse fails
-- ---------------------------------------------------------------------------

{- | If the synthesized module fails to parse, try each non-empty chunk
through 'parseDeclaration' and 'parseExpression' and union the results.
On chunk-level parse failure, retry with just the first line — handles
cases like @"let x = 1\\n  let y = 2"@ where a malformed indented
continuation would otherwise drop the whole chunk's contribution.
-}
fallbackPerChunk :: [Text] -> (Set Text, Set Text)
fallbackPerChunk lns =
    let chunks = splitChunks lns
        contributions = map analyseChunkRobust chunks
        (defs, uses) = foldr combine (S.empty, S.empty) contributions
        finalUses = uses `S.difference` defs
     in (defs, finalUses)
  where
    combine (d, u) (ds, us) = (S.union d ds, S.union u us)

analyseChunkRobust :: Text -> (Set Text, Set Text)
analyseChunkRobust chunk =
    case tryParseChunk chunk of
        Just contrib -> contrib
        Nothing -> case T.lines chunk of
            (firstLine : _ : _) ->
                fromMaybe (S.empty, S.empty) (tryParseChunk firstLine)
            _ -> (S.empty, S.empty)

tryParseChunk :: Text -> Maybe (Set Text, Set Text)
tryParseChunk chunk =
    case P.parseDeclaration (T.unpack chunk) dynFlags of
        POk _ ldecl ->
            let d = unLoc ldecl
             in Just (topLevelDefsFromDecl d, declFreeVars d)
        PFailed _ -> case P.parseExpression (T.unpack chunk) dynFlags of
            POk _ lexpr ->
                let allRefs = collectUses lexpr
                    localBinders = collectBinders lexpr
                 in Just (S.empty, allRefs `S.difference` localBinders)
            PFailed _ -> Nothing

splitChunks :: [Text] -> [Text]
splitChunks = go []
  where
    go acc [] = [unsplit (reverse acc) | not (null acc)]
    go acc (l : ls)
        | T.null (T.strip l) = case acc of
            [] -> go [] ls
            _ -> unsplit (reverse acc) : go [] ls
        | startsAtCol0 l = case acc of
            [] -> go [l] ls
            _ -> unsplit (reverse acc) : go [l] ls
        | otherwise = go (l : acc) ls
    unsplit = T.intercalate "\n"
    startsAtCol0 t = case T.uncons t of
        Just (c, _) -> c /= ' ' && c /= '\t'
        Nothing -> False

-- ---------------------------------------------------------------------------
-- DynFlags
-- ---------------------------------------------------------------------------

{- | Parser settings: enable extensions that real Sabela notebooks rely on
so the parser never refuses input the running GHCi would happily accept.
-}
dynFlags :: DynFlags
dynFlags =
    let base = defaultDynFlags fakeSettings
     in foldl xopt_set base extensions

extensions :: [LE.Extension]
extensions =
    [ LE.TypeApplications
    , LE.OverloadedStrings
    , LE.TemplateHaskell
    , LE.TemplateHaskellQuotes
    , LE.DataKinds
    , LE.PolyKinds
    , LE.RankNTypes
    , LE.GADTs
    , LE.GADTSyntax
    , LE.FlexibleContexts
    , LE.FlexibleInstances
    , LE.MultiParamTypeClasses
    , LE.FunctionalDependencies
    , LE.ScopedTypeVariables
    , LE.ConstraintKinds
    , LE.KindSignatures
    , LE.StandaloneDeriving
    , LE.DeriveGeneric
    , LE.DeriveFunctor
    , LE.DeriveFoldable
    , LE.DeriveTraversable
    , LE.GeneralizedNewtypeDeriving
    , LE.LambdaCase
    , LE.MultiWayIf
    , LE.RecordWildCards
    , LE.NamedFieldPuns
    , LE.TupleSections
    , LE.ViewPatterns
    , LE.BangPatterns
    , LE.ExplicitForAll
    , LE.PatternSynonyms
    , LE.ImportQualifiedPost
    , LE.NumericUnderscores
    , LE.BlockArguments
    , LE.OverloadedRecordDot
    , LE.OverloadedRecordUpdate
    , LE.QualifiedDo
    , LE.LinearTypes
    ]
