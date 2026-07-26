{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Parse (
    CellSymbols (..),
    cellNames,
    cellSymbols,
    parseCellModule,
    staleBindings,
    unparseableChunks,
    validateCellShape,
) where

import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Driver.Session (
    DynFlags,
    defaultDynFlags,
    xopt_set,
 )
import qualified GHC.Hs as Hs
import qualified GHC.LanguageExtensions.Type as LE
import GHC.Parser.Lexer (ParseResult (..))
import GHC.Types.SrcLoc (GenLocated (..), unLoc)

import qualified Language.Haskell.GhclibParserEx.GHC.Parser as P
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import Sabela.Diagnose (topLevelLetMessage)
import Sabela.Model (CellType (..))
import Sabela.Parse.Ast (
    CellSymbols (..),
    collectBinders,
    collectUses,
    declFreeVars,
    extractFromModule,
    topLevelDefsFromDecl,
 )
import Sabela.Parse.Preprocess (noTopLevelIn, preprocess)

cellNames :: Text -> (Set Text, Set Text)
cellNames src = let s = cellSymbols src in (csDefs s, csUses s)

staleBindings :: Text -> Text -> [Text]
staleBindings oldSrc newSrc =
    S.toAscList (fst (cellNames oldSrc) `S.difference` fst (cellNames newSrc))

cellSymbols :: Text -> CellSymbols
cellSymbols src =
    case parseCellModule src of
        Just hsMod -> extractFromModule hsMod
        Nothing ->
            let (defs, uses) = fallbackPerChunk (preprocess src)
             in CellSymbols
                    { csDefs = defs
                    , csUses = uses
                    , csProvides = S.empty
                    , csClassMethods = S.empty
                    }

parseCellModule :: Text -> Maybe (Hs.HsModule Hs.GhcPs)
parseCellModule src =
    let moduleSrc =
            "module SabelaCell where\n"
                ++ T.unpack (T.unlines (preprocess src))
     in case P.parseModule moduleSrc dynFlags of
            POk _ (L _ hsMod) -> Just hsMod
            PFailed _ -> Nothing

unparseableChunks :: Text -> [Text]
unparseableChunks src =
    [ chunk
    | chunk <- splitChunks (filter (not . commentLine) (preprocess src))
    , isNothing (tryParseChunk chunk)
    , not (moduleLevel chunk)
    ]
  where
    commentLine l =
        let t = T.stripStart l
         in "--" `T.isPrefixOf` t || "{-#" `T.isPrefixOf` t
    moduleLevel chunk = case P.parseModule (T.unpack chunk) dynFlags of
        POk _ _ -> True
        PFailed _ -> False

validateCellShape :: CellType -> Text -> Maybe Text
validateCellShape CodeCell src
    | hasTopLevelLet src = Just topLevelLetMessage
    | otherwise = Nothing
validateCellShape ProseCell src
    | not (S.null (csDefs (cellSymbols src))) =
        Just proseCodeMessage
    | otherwise = Nothing

proseCodeMessage :: Text
proseCodeMessage =
    "This is a ProseCell (Markdown), but it contains Haskell definitions. Put\
    \ executable code in a CodeCell instead, or keep the prose free of\
    \ top-level bindings."

hasTopLevelLet :: Text -> Bool
hasTopLevelLet = any isStmtLet . T.lines
  where
    isStmtLet raw =
        maybe False noTopLevelIn (T.stripPrefix "let " raw)

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
