{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.HoleNudge (
    attachPairs,
    holeNudgePairs,
    inventedNames,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Char (isLower)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair.Mitigate (substituteNameInCode)
import Sabela.AI.FitRule (holeFitsJson)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.HoleRepair (holeTypeFromDiagnostic)
import Sabela.Parse (cellNames)

inventedNames :: Text -> Text -> [Text]
inventedNames diagnostic src =
    nub
        [ n
        | chunk <- T.splitOn "\n\n" diagnostic
        , Just n <- [scopeSubject chunk]
        , not (n `Set.member` defined)
        , valueLevel n
        ]
  where
    defined = fst (cellNames src)
    valueLevel n = case T.uncons (lastSegment n) of
        Just (c, _) -> isLower c || c == '_'
        Nothing -> False
    lastSegment n = case reverse (T.splitOn "." n) of
        (s : _) -> s
        [] -> n

holeNudgePairs :: (Text -> IO Text) -> Text -> Text -> IO [Pair]
holeNudgePairs probe diagnostic src = case inventedNames diagnostic src of
    [] -> pure []
    (name : _) -> do
        let holed = substituteNameInCode name "_" src
        if holed == src
            then pure []
            else do
                blob <- probe holed
                pure $ case holeTypeFromDiagnostic blob of
                    Nothing -> []
                    Just ty ->
                        [ "typeDirected"
                            .= object
                                ( [ "invented" .= name
                                  , "holeType" .= ty
                                  , "note" .= note name ty
                                  ]
                                    <> fitPairs blob
                                )
                        ]
  where
    note name ty =
        name
            <> " is not defined in the session or any imported module. With a \
               \typed hole in its place, the call site needs: "
            <> ty
    fitPairs blob = case holeFitsJson fitCap blob of
        [] -> []
        fits -> ["holeFits" .= fits]

fitCap :: Int
fitCap = 8

attachPairs :: [Pair] -> Value -> Value
attachPairs [] v = v
attachPairs pairs (Object o) = Object (KM.union o extra)
  where
    extra = case object pairs of
        Object e -> e
        _ -> KM.empty
attachPairs _ v = v
