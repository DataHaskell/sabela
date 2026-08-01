{-# LANGUAGE OverloadedStrings #-}

{- | The one refusal a green candidate can still earn. It goes out through the
same 'GateSource' as every other rejection, so the text under @source@ is the
caller's own on this path too.
-}
module Sabela.AI.Capabilities.Edit.CompileGate.Defaulting (
    gateDefaultingRejection,
    defaultedToUnit,
) where

import Data.Aeson (Value, (.=))
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate.Source (
    GateSource (..),
    compiledSourcePairs,
 )
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Api (errorJsonWith)
import Sabela.Session.MaterializeStage (
    Attribution (..),
    DisposableResult (..),
    MaterializeStage (..),
    attributionText,
    materializeStageText,
 )

{- | A green candidate that only type-checked because GHC defaulted an
ambiguous variable to an empty type is not a pass: nothing in the cell pinned
the variable down, so GHC picked for it. Any note the write already owed is
carried out with the refusal rather than dropped with the commit.
-}
gateDefaultingRejection ::
    Maybe Int -> [Text] -> GateSource -> DisposableResult -> Maybe Value
gateDefaultingRejection mReplaces notes gsrc result =
    case defaultedToUnit (disposableStderr result) of
        [] -> Nothing
        vars ->
            Just . refusalAck "compile-gate" mReplaces $
                errorJsonWith
                    ( "This compiles, but only because GHC had to default "
                        <> T.intercalate ", " vars
                        <> " to a type carrying no information: nothing in the cell \
                           \pins it down. Annotate the expression, or apply it to the \
                           \value you mean."
                    )
                    ( [ "verdict" .= verdictTag VerdictDiagnostic
                      , "stage" .= materializeStageText StageCandidateTypecheck
                      , "attributedTo" .= attributionText AttributedCandidate
                      , "defaultedTypeVariables" .= vars
                      , "source" .= gateSubmitted gsrc
                      ]
                        <> compiledSourcePairs gsrc
                        <> ["note" .= T.unwords notes | not (null notes)]
                    )

{- | Type variables GHC resolved by defaulting to a type carrying no
information. Defaulting to Integer is ordinary Haskell; defaulting to @()@ or
@Any@ means nothing in the expression pinned the variable down.
-}
defaultedToUnit :: Text -> [Text]
defaultedToUnit diagnostic =
    nub
        [ var
        | chunk <- drop 1 (T.splitOn "Defaulting the type variable" diagnostic)
        , "to type" `T.isInfixOf` chunk
        , (var : ty : _) <- [quotedNames chunk]
        , unqualify ty `elem` emptyTypes
        ]
  where
    emptyTypes = ["()", "Any"]
    unqualify = T.takeWhileEnd (/= '.')

quotedNames :: Text -> [Text]
quotedNames t = case T.breakOn "\8216" t of
    (_, rest)
        | T.null rest -> []
        | otherwise ->
            let (name, after) = T.breakOn "\8217" (T.drop 1 rest)
             in if T.null after then [] else name : quotedNames (T.drop 1 after)
