{-# LANGUAGE OverloadedStrings #-}

{- | The compile-only candidate selector of the speculative repair tier:
keep every candidate whose rewritten binding @:type@-checks clean, without
executing any. Split from "Sabela.AI.Capabilities.Edit.HoleSearch" for the
module-size cap.
-}
module Sabela.AI.Capabilities.Edit.TypeSelect (
    selectCleanByTypeCheck,
    typeCheckTarget,
) where

import Control.Monad (filterM)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.Session.Query (
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckValueWith,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

{- | Every candidate whose expression @:type@-checks clean, in order, WITHOUT
running any. Returning ALL survivors lets the caller's health vet walk past a
candidate that type-checks in isolation but fails the binding's goal.
-}
selectCleanByTypeCheck :: App -> [Text] -> IO [Text]
selectCleanByTypeCheck _ [] = pure []
selectCleanByTypeCheck app cands = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing -> pure []
        Just backend -> filterM (checkClean backend) cands
  where
    checkClean backend c = do
        primitive <- featureEnabled "SABELA_TYPECHECK_PRIMITIVE"
        if primitive && classifyTypecheckInput c /= OutsideValueSubset
            then
                tcSucceeded
                    <$> typecheckValueWith
                        (ST.sbQueryType backend)
                        (ST.sbQueryBindings backend)
                        c
            else do
                out <- ST.sbQueryType backend (typeCheckTarget c)
                pure (isClean (healthOfTypeQuery out))

{- | The expression to @:type@ for a candidate source: the RHS of the LAST
@x = expr@ binding line, else the whole stripped source. The last binding is the
one a cell defines for its dependents, and the one a repair rewrote.

A multi-line cell must not fall through to @:type@-ing the whole source — that
never checks clean, so every correct candidate would be rejected and the tier
would look inert. Imports and @x <- e@ statements are skipped; a non-checkable
candidate still fails the check, so at worst a repair is missed, never kept.
-}
typeCheckTarget :: Text -> Text
typeCheckTarget src = case reverse (mapMaybe bindingRhs (T.lines stripped)) of
    (rhs : _) -> rhs
    [] -> stripped
  where
    stripped = T.strip src
    bindingRhs ln
        | T.isPrefixOf "import " l = Nothing
        | (_, rhs) <- T.breakOn " = " l
        , not (T.null rhs) =
            Just (T.strip (T.drop 3 rhs))
        | otherwise = Nothing
      where
        l = T.strip ln
