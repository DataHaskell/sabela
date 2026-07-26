{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.TypeSelect (
    selectCleanByTypeCheck,
    typeCheckTarget,
) where

import Control.Monad (filterM)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.Session.Query (
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckValueWith,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

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
