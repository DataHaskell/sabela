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

declaredNames :: Text -> Set Text
declaredNames src =
    case parseCellModule src of
        Just hsMod ->
            csDefs (extractFromModule hsMod)
                `S.union` S.unions
                    (map (topLevelSigsFromDecl . unLoc) (Hs.hsmodDecls hsMod))
        Nothing -> fst (cellNames src)

preservesDeclarations :: Text -> Text -> Bool
preservesDeclarations before after =
    declaredNames before `S.isSubsetOf` declaredNames after

signatureWithoutEquation :: Text -> [Text]
signatureWithoutEquation src = case parseCellModule src of
    Nothing -> []
    Just hsMod ->
        let decls = map unLoc (Hs.hsmodDecls hsMod)
            sigs = S.unions (map topLevelSigsFromDecl decls)
            defs = S.unions (map topLevelDefsFromDecl decls)
         in S.toAscList (sigs `S.difference` defs)
