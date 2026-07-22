{-# LANGUAGE OverloadedStrings #-}

{- | The ARGUMENT-INSERTION repair move: GHC's \"applied to too few arguments\"
diagnostic names the misapplied function and (via the first-argument mismatch)
the missing argument's type. No rename can fix this class — the move inserts
the missing argument, filled by a hole fit of its type. Extraction preserves
qualified spellings verbatim; the query layer sanitizes.
-}
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

-- | The function GHC's probable-cause line says is applied to too few arguments.
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

{- | The expected type of the function's FIRST argument: the nearest preceding
@Couldn't match expected type:@ above the @In the first argument of `fn'@ line.
-}
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

{- | Fills for an argument slot, from the hole fits of its type. Deliberately
NOT filtered by @vacuousFit@: @Nothing@ or @mempty@ silently EMPTIES a cell as
a full-RHS replacement, but is the legitimate feed for an argument (the
@takeWhileP Nothing@ label).
-}
argFillCandidates :: Text -> [Text]
argFillCandidates = holeFitNames

{- | Insert the fill directly after the function at the reported site:
@fn args…@ becomes @fn fill args…@. Span-validated like 'substituteNameAt' —
no site, or a different token there, yields no candidate.
-}
insertArgAt :: (Int, Int) -> Text -> Text -> Text -> Maybe Text
insertArgAt sp fn fill = substituteNameAt sp fn (fn <> " " <> fill)
