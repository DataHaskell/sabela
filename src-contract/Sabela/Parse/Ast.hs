{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Parse.Ast (
    CellSymbols (..),
    extractFromModule,
    declFreeVars,
    topLevelDefsFromDecl,
    topLevelSigsFromDecl,
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

data CellSymbols = CellSymbols
    { csDefs :: Set Text
    , csUses :: Set Text
    , csProvides :: Set Text
    , csClassMethods :: Set Text
    }
    deriving (Eq, Show)

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

instanceDecls :: Hs.HsModule Hs.GhcPs -> [Hs.InstDecl Hs.GhcPs]
instanceDecls m = [inst | Hs.InstD _ inst <- map unLoc (Hs.hsmodDecls m)]

instanceTypeUses :: Hs.InstDecl Hs.GhcPs -> Set Text
instanceTypeUses inst =
    S.fromList
        [ name
        | Hs.HsTyVar _ _ ln <- universeBi inst :: [Hs.HsType Hs.GhcPs]
        , let name = rdrText (unLoc ln)
        , isUpperName name
        ]

instanceMethodNames :: Hs.InstDecl Hs.GhcPs -> Set Text
instanceMethodNames inst =
    S.unions
        [ bindBinders b
        | b <- universeBi inst :: [Hs.HsBindLR Hs.GhcPs Hs.GhcPs]
        ]

classMethodNames :: Hs.HsDecl Hs.GhcPs -> Set Text
classMethodNames = \case
    Hs.TyClD _ Hs.ClassDecl{Hs.tcdSigs = sigs} ->
        S.unions (map (sigBinders . unLoc) sigs)
    _ -> S.empty

isUpperName :: Text -> Bool
isUpperName t = case T.uncons t of
    Just (c, _) -> Char.isUpper c
    Nothing -> False

declFreeVars :: Hs.HsDecl Hs.GhcPs -> Set Text
declFreeVars decl =
    let allRefs = collectUses decl
        localBinders = collectBinders decl
     in allRefs `S.difference` localBinders

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

topLevelSigsFromDecl :: Hs.HsDecl Hs.GhcPs -> Set Text
topLevelSigsFromDecl = \case
    Hs.SigD _ sig -> sigBinders sig
    _ -> S.empty

bindBinders :: Hs.HsBindLR Hs.GhcPs Hs.GhcPs -> Set Text
bindBinders = \case
    Hs.FunBind{Hs.fun_id = lname} -> S.singleton (rdrText (unLoc lname))
    Hs.PatBind{Hs.pat_lhs = lpat} -> patBinders (unLoc lpat)
    Hs.PatSynBind _ psb -> S.singleton (rdrText (unLoc (Hs.psb_id psb)))
    _ -> S.empty

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

patNodeBinders :: Hs.Pat Hs.GhcPs -> Set Text
patNodeBinders = PatNodeBinders.patNodeBinders

patBinders :: Hs.Pat Hs.GhcPs -> Set Text
patBinders top =
    S.unions
        [patNodeBinders p | p <- universeBi top :: [Hs.Pat Hs.GhcPs]]

collectUses :: forall a. (Data a) => a -> Set Text
collectUses x =
    S.fromList
        [rdrText (unLoc ln) | Hs.HsVar _ ln <- universeBi x :: [Hs.HsExpr Hs.GhcPs]]

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
