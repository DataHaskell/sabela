module Siza.Agent.RepairGuard (
    goalFromErrorInCell,
    selfDeclaredSigs,
) where

import qualified Data.Set as Set
import Data.Text (Text)

import qualified GHC.Hs as Hs
import GHC.Types.SrcLoc (unLoc)

import Sabela.AI.HoleRepair (goalFromError)
import Sabela.Parse.Ast (topLevelDefsFromDecl, topLevelSigsFromDecl)
import Siza.Lang.Haskell (parseModuleE)

selfDeclaredSigs :: Text -> [Text]
selfDeclaredSigs src = case parseModuleE src of
    Left _ -> []
    Right m ->
        let decls = map unLoc (Hs.hsmodDecls m)
            sigs = Set.unions (map topLevelSigsFromDecl decls)
            defs = Set.unions (map topLevelDefsFromDecl decls)
         in Set.toList (sigs `Set.difference` defs)

goalFromErrorInCell :: [Text] -> Text -> Maybe (Text, Text)
goalFromErrorInCell selfDeclared err = do
    (name, ty) <- goalFromError err
    if name `elem` selfDeclared then Nothing else Just (name, ty)
