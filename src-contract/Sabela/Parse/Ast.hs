{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The AST walk that powers 'Sabela.Parse.cellNames': given a parsed
@ghc-lib-parser@ 'Hs.HsModule', produce the cell's top-level
definitions and its free identifier uses (the textual driver in
'Sabela.Parse' wraps this with preprocessing + a per-chunk fallback).

Per-decl scoping is intentional — see 'declFreeVars'. Internal helpers
('bindBinders', 'tyClBinders', etc.) are unexported; only the entry
points that 'Sabela.Parse' actually wires together are public.
-}
module Sabela.Parse.Ast (
    -- * Module-level entry
    CellSymbols (..),
    extractFromModule,

    -- * Per-decl entries (used by the chunk-level fallback)
    declFreeVars,
    topLevelDefsFromDecl,

    -- * Generic traversals (used by the expression-level fallback)
    collectUses,
    collectBinders,
) where

import qualified Data.Char as Char
import Data.Data (Data)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Data.Generics.Uniplate.Data (universeBi)

import qualified GHC.Hs as Hs
import GHC.Types.SrcLoc (unLoc)
import Sabela.Parse.Ast.Names (rdrText)
import qualified Sabela.Parse.Ast.PatNodeBinders as PatNodeBinders

-- ---------------------------------------------------------------------------
-- Module-level extraction
-- ---------------------------------------------------------------------------

{- | What a single cell contributes to the reactivity DAG. @csDefs@/@csUses@
are the single-owner def/use sets; the two extra channels carry the
typeclass relationships the single-owner @defMap@ can't express:

* @csProvides@ — method names an *instance* binds. An instance owns no
  top-level name (the class does), so consumers of a method can't reach
  the instance through @defMap@. 'Sabela.Topo.buildDepGraph' turns these
  into edges from each method user to every implementing instance.

* @csClassMethods@ — method names a *class* declaration introduces. Used
  to scope @csProvides@ edges to methods that are actually declared in the
  notebook, so an @instance Show MyType@ doesn't wire every @show@ caller
  to itself.
-}
data CellSymbols = CellSymbols
    { csDefs :: Set Text
    , csUses :: Set Text
    , csProvides :: Set Text
    , csClassMethods :: Set Text
    }
    deriving (Eq, Show)

{- | Top-level extraction. Defs come from each top-level decl's LHS. Uses
are computed **per decl** (not globally over the module), so a parameter
@x@ that's local to one decl doesn't shadow a free @x@ in a sibling
decl's body. References to names this cell defines itself are subtracted
from uses since they're intra-cell, not external dependencies. An
instance's head type names are folded into uses so it depends on the cell
declaring its class.
-}
extractFromModule :: Hs.HsModule Hs.GhcPs -> CellSymbols
extractFromModule m =
    let topDecls = map unLoc (Hs.hsmodDecls m)
        insts = instanceDecls m
        defs = S.unions (map topLevelDefsFromDecl topDecls)
        rawUses = S.unions (map declFreeVars topDecls)
        instUses = S.unions (map instanceTypeUses insts)
        uses = (rawUses `S.union` instUses) `S.difference` defs
     in CellSymbols
            { csDefs = defs
            , csUses = uses
            , csProvides = S.unions (map instanceMethodNames insts)
            , csClassMethods = S.unions (map classMethodNames topDecls)
            }

-- | The instance declarations in a module body.
instanceDecls :: Hs.HsModule Hs.GhcPs -> [Hs.InstDecl Hs.GhcPs]
instanceDecls m = [inst | Hs.InstD _ inst <- map unLoc (Hs.hsmodDecls m)]

{- | Type-constructor / class names referenced anywhere in an instance decl
(its head and method types). Restricted to upper-case names so type
variables can't create spurious cross-cell edges. These become extra uses
so an instance depends on the cell defining its class (and any head types).
-}
instanceTypeUses :: Hs.InstDecl Hs.GhcPs -> Set Text
instanceTypeUses inst =
    S.fromList
        [ name
        | Hs.HsTyVar _ _ ln <- universeBi inst :: [Hs.HsType Hs.GhcPs]
        , let name = rdrText (unLoc ln)
        , isUpperName name
        ]

{- | Method names an instance binds. Collected generically (every binder in
the instance subtree); over-collecting a method's local @where@ helper only
ever adds a conservative extra edge, never drops a real one.
-}
instanceMethodNames :: Hs.InstDecl Hs.GhcPs -> Set Text
instanceMethodNames inst =
    S.unions
        [ bindBinders b
        | b <- universeBi inst :: [Hs.HsBindLR Hs.GhcPs Hs.GhcPs]
        ]

-- | Method names a class declaration introduces (its method signatures).
classMethodNames :: Hs.HsDecl Hs.GhcPs -> Set Text
classMethodNames = \case
    Hs.TyClD _ Hs.ClassDecl{Hs.tcdSigs = sigs} ->
        S.unions (map (sigBinders . unLoc) sigs)
    _ -> S.empty

isUpperName :: Text -> Bool
isUpperName t = case T.uncons t of
    Just (c, _) -> Char.isUpper c
    Nothing -> False

{- | Free variables of a single top-level declaration: every 'Hs.HsVar'
reference inside the decl, minus every name bound anywhere within that
same decl (function params, where/let binders, lambda binders, do-binds,
list-comp generators, case patterns).

Per-decl scoping is intentional: it's a coarse approximation of
proper lexical scope but it's *strictly* better than the prior textual
heuristic for the common case where one decl's parameter happens to
collide with a free use in a sibling decl's body.
-}
declFreeVars :: Hs.HsDecl Hs.GhcPs -> Set Text
declFreeVars decl =
    let allRefs = collectUses decl
        localBinders = collectBinders decl
     in allRefs `S.difference` localBinders

-- ---------------------------------------------------------------------------
-- Top-level def extraction
-- ---------------------------------------------------------------------------

topLevelDefsFromDecl :: Hs.HsDecl Hs.GhcPs -> Set Text
topLevelDefsFromDecl = \case
    Hs.ValD _ bind -> bindBinders bind
    Hs.TyClD _ tcd -> tyClBinders tcd
    Hs.SigD{} -> S.empty
    Hs.InstD{} -> S.empty
    Hs.DerivD{} -> S.empty
    Hs.DefD{} -> S.empty
    Hs.ForD{} -> S.empty
    Hs.WarningD{} -> S.empty
    Hs.AnnD{} -> S.empty
    Hs.RuleD{} -> S.empty
    Hs.SpliceD{} -> S.empty
    Hs.DocD{} -> S.empty
    Hs.RoleAnnotD{} -> S.empty
    _ -> S.empty

{- | Names introduced by an 'Hs.HsBindLR' (the LHS of a value/function binding).
Pattern synonyms count too — they declare a top-level name, so cross-cell
@import via PatternSynonyms@ deps need an edge to the defining cell.
-}
bindBinders :: Hs.HsBindLR Hs.GhcPs Hs.GhcPs -> Set Text
bindBinders = \case
    Hs.FunBind{Hs.fun_id = lname} -> S.singleton (rdrText (unLoc lname))
    Hs.PatBind{Hs.pat_lhs = lpat} -> patBinders (unLoc lpat)
    Hs.PatSynBind _ psb -> S.singleton (rdrText (unLoc (Hs.psb_id psb)))
    _ -> S.empty

{- | Names introduced by a 'Hs.TyClDecl': the type/class name itself, every
data constructor it declares, and every method signature in a class body.
-}
tyClBinders :: Hs.TyClDecl Hs.GhcPs -> Set Text
tyClBinders = \case
    Hs.DataDecl{Hs.tcdLName = ln, Hs.tcdDataDefn = ddef} ->
        S.insert (rdrText (unLoc ln)) (dataDefnConstructors ddef)
    Hs.SynDecl{Hs.tcdLName = ln} ->
        S.singleton (rdrText (unLoc ln))
    Hs.ClassDecl{Hs.tcdLName = ln, Hs.tcdSigs = sigs} ->
        S.insert (rdrText (unLoc ln)) (S.unions (map (sigBinders . unLoc) sigs))
    Hs.FamDecl _ fd -> S.singleton (rdrText (unLoc (Hs.fdLName fd)))

dataDefnConstructors :: Hs.HsDataDefn Hs.GhcPs -> Set Text
dataDefnConstructors ddef =
    S.unions [conDeclNames (unLoc lc) | lc <- toList (Hs.dd_cons ddef)]

conDeclNames :: Hs.ConDecl Hs.GhcPs -> Set Text
conDeclNames = \case
    Hs.ConDeclH98{Hs.con_name = ln} -> S.singleton (rdrText (unLoc ln))
    Hs.ConDeclGADT{Hs.con_names = lns} ->
        S.fromList [rdrText (unLoc ln) | ln <- NE.toList lns]

sigBinders :: Hs.Sig Hs.GhcPs -> Set Text
sigBinders = \case
    Hs.TypeSig _ lns _ ->
        S.fromList [rdrText (unLoc ln) | ln <- lns]
    Hs.ClassOpSig _ _ lns _ ->
        S.fromList [rdrText (unLoc ln) | ln <- lns]
    _ -> S.empty

-- ---------------------------------------------------------------------------
-- Pattern binders (recursive)
-- ---------------------------------------------------------------------------

{- | Binders introduced by a single pattern node, ignoring sub-patterns
(uniplate handles the recursion downstream). Splitting the recursion lets
us stay tolerant of GHC AST shape changes across @ghc-lib-parser@ minor
bumps — sub-patterns are reached generically rather than by hand-coded
constructor matching.
-}
patNodeBinders :: Hs.Pat Hs.GhcPs -> Set Text
patNodeBinders = PatNodeBinders.patNodeBinders

-- | Recursive pattern-binder extraction (every level of nesting).
patBinders :: Hs.Pat Hs.GhcPs -> Set Text
patBinders top =
    S.unions
        [patNodeBinders p | p <- universeBi top :: [Hs.Pat Hs.GhcPs]]

-- ---------------------------------------------------------------------------
-- Generic traversals over the AST (uniplate / Data-driven)
-- ---------------------------------------------------------------------------

{- | Every identifier reference (every 'Hs.HsVar' occurrence) anywhere in
the sub-tree.
-}
collectUses :: forall a. (Data a) => a -> Set Text
collectUses x =
    S.fromList
        [rdrText (unLoc ln) | Hs.HsVar _ ln <- universeBi x :: [Hs.HsExpr Hs.GhcPs]]

{- | Every name appearing in a binding position in the sub-tree: pattern
binders, function-binding names, type/class/method names, data constructors,
do-block @\<-@ binders, list-comprehension generators (which are pattern
binders inside 'BindStmt'), let binders. We collect them by visiting every
'Hs.HsBindLR' / 'Hs.Pat' / 'Hs.TyClDecl' the AST contains.
-}
collectBinders :: forall a. (Data a) => a -> Set Text
collectBinders x = S.unions [bindersFromBind, bindersFromPat, bindersFromTyCl]
  where
    bindersFromBind =
        S.unions
            [ bindBinders b
            | b <- universeBi x :: [Hs.HsBindLR Hs.GhcPs Hs.GhcPs]
            ]
    bindersFromPat =
        S.unions
            [ patNodeBinders p
            | p <- universeBi x :: [Hs.Pat Hs.GhcPs]
            ]
    bindersFromTyCl =
        S.unions
            [ tyClBinders t
            | t <- universeBi x :: [Hs.TyClDecl Hs.GhcPs]
            ]
