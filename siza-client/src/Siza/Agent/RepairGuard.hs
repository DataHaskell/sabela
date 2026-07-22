{- | The false-goal guard for red-cell repair (R9-T4): a name the SUBMITTED
cell declares a signature for but leaves without an equation is being defined
here, so a not-in-scope error about it is not a producer-hunt target — hunting
one sent the topMonth cascade after a @String@ producer for the very binding
under definition. Diagnostic-class: the check is on shape, not the name.
-}
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

{- | Names the submitted cell declares a signature for but leaves without an
equation. Empty when the cell does not parse — no false suppression on an
incomplete mid-edit source.
-}
selfDeclaredSigs :: Text -> [Text]
selfDeclaredSigs src = case parseModuleE src of
    Left _ -> []
    Right m ->
        let decls = map unLoc (Hs.hsmodDecls m)
            sigs = Set.unions (map topLevelSigsFromDecl decls)
            defs = Set.unions (map topLevelDefsFromDecl decls)
         in Set.toList (sigs `Set.difference` defs)

{- | 'Sabela.AI.HoleRepair.goalFromError' guarded by @selfDeclared@: no target
when the not-in-scope name is one the cell defines here without an equation.
-}
goalFromErrorInCell :: [Text] -> Text -> Maybe (Text, Text)
goalFromErrorInCell selfDeclared err = do
    (name, ty) <- goalFromError err
    if name `elem` selfDeclared then Nothing else Just (name, ty)
