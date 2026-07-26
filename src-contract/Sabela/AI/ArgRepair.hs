{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ArgRepair (
    argFillCandidates,
    insertArgAt,
    missingArgType,
    tooFewArgsTarget,
) where

import Data.List (findIndex)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleRepair (holeFitNames, substituteNameAt)

tooFewArgsTarget :: Text -> Maybe Text
tooFewArgsTarget err =
    listToMaybe
        [ name
        | l <- T.lines err
        , Just rest <- [T.stripPrefix "Probable cause: `" (T.strip l)]
        , let (name, rest') = T.breakOn "'" rest
        , "' is applied to too few arguments" `T.isPrefixOf` rest'
        , not (T.null name)
        ]

missingArgType :: Text -> Text -> Maybe Text
missingArgType err fn = do
    let ls = T.lines err
        marker = "In the first argument of `" <> fn <> "'"
    i <- findIndex (marker `T.isInfixOf`) ls
    listToMaybe
        [ ty
        | l <- reverse (take i ls)
        , (_, rest) <- [T.breakOn expectedMarker l]
        , not (T.null rest)
        , let ty = T.strip (T.drop (T.length expectedMarker) rest)
        , not (T.null ty)
        ]
  where
    expectedMarker = "Couldn't match expected type:"

argFillCandidates :: Text -> [Text]
argFillCandidates = holeFitNames

insertArgAt :: (Int, Int) -> Text -> Text -> Text -> Maybe Text
insertArgAt sp fn fill = substituteNameAt sp fn (fn <> " " <> fill)
