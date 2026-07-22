{- | The declaration-preservation law for cell repairs: the names a cell
DECLARES (defs plus standalone top-level signatures) are its contract, and a
repair candidate that drops one substituted the deliverable, not a reference
— however green it compiles (the run-20260721-005731 topMonth class).
-}
module Sabela.Parse.Declared (
    declaredNames,
    preservesDeclarations,
    signatureWithoutEquation,
) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

import qualified GHC.Hs as Hs
import GHC.Types.SrcLoc (unLoc)

import Sabela.Parse (CellSymbols (..), cellNames, parseCellModule)
import Sabela.Parse.Ast (
    extractFromModule,
    topLevelDefsFromDecl,
    topLevelSigsFromDecl,
 )

{- | Names the cell DECLARES: defs plus standalone top-level signature names.
A signature-only cell binds nothing (reactivity counts no def) yet still
claims its name as the cell's contract; parse failure falls back to defs.
-}
declaredNames :: Text -> Set Text
declaredNames src =
    case parseCellModule src of
        Just hsMod ->
            csDefs (extractFromModule hsMod)
                `S.union` S.unions
                    (map (topLevelSigsFromDecl . unLoc) (Hs.hsmodDecls hsMod))
        Nothing -> fst (cellNames src)

{- | The repair acceptance law over declarations: a candidate rewrite must
keep every name the original source declared.
-}
preservesDeclarations :: Text -> Text -> Bool
preservesDeclarations before after =
    declaredNames before `S.isSubsetOf` declaredNames after

{- | Top-level names the cell declares a signature for but gives no equation in
the same cell. Such a name binds nothing — GHCi answers @Variable not in scope:
name :: T@, a red cell that dams later inserts and feeds a bogus hole-fit target
(the topMonth trap). Diagnostic-class, task-independent: any signature missing
its body qualifies. Empty when the cell does not parse (no false positive).
-}
signatureWithoutEquation :: Text -> [Text]
signatureWithoutEquation src = case parseCellModule src of
    Nothing -> []
    Just hsMod ->
        let decls = map unLoc (Hs.hsmodDecls hsMod)
            sigs = S.unions (map topLevelSigsFromDecl decls)
            defs = S.unions (map topLevelDefsFromDecl decls)
         in S.toAscList (sigs `S.difference` defs)
