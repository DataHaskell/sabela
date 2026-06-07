{-# LANGUAGE LambdaCase #-}

module Sabela.Parse.Ast.PatNodeBinders (patNodeBinders) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified GHC.Hs as Hs
import GHC.Types.SrcLoc (unLoc)
import qualified Language.Haskell.Syntax as Hs
import Sabela.Parse.Ast.Names

patNodeBinders :: Hs.Pat Hs.GhcPs -> Set Text
patNodeBinders = \case
    Hs.VarPat _ ln -> S.singleton (rdrText (unLoc ln))
    Hs.AsPat _ ln _ _ -> S.singleton (rdrText (unLoc ln))
    Hs.NPlusKPat _ ln _ _ _ _ -> S.singleton (rdrText (unLoc ln))
    _ -> S.empty
