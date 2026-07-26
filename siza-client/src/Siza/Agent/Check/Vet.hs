{-# LANGUAGE OverloadedStrings #-}

{- | Compile-vetting for covering checks (C2 task 1): a proposal is trialled
off-notebook BEFORE the user is asked to accept it. live_test19 displayed
@length ys == 629@ for acceptance and only discovered at run time that it does
not compile, which reads to the user as a test that ran.
-}
module Siza.Agent.Check.Vet (
    vetCheckWith,
    vetProposal,
    vetProposalAgainst,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome, toolOutcomeValue)
import Siza.Agent.Check (CheckResult (..), classifyCheck, markerSrc)
import Siza.Agent.Check.Gate (
    CheckRefusal (..),
    mentionsAny,
    perturbCheck,
    perturbationsFor,
    referenceGate,
    refusalNote,
 )
import Siza.Agent.Tools (renderOutcome)

-- | How a tool call is made; injected so vetting is testable without a server.
type Call = ToolName -> Value -> IO (Either Text ToolOutcome)

{- | Trial a proposed check through @try@. It runs the same marker the live run
would, so the vet and the run cannot disagree about whether the check compiles.
-}
vetCheckWith :: Call -> Text -> IO CheckResult
vetCheckWith call check
    | T.null (T.strip check) = pure CheckNotApplicable
    | otherwise = do
        out <-
            renderOutcome
                <$> call
                    Try
                    (object ["code" .= markerSrc check, "language" .= ("Haskell" :: Text)])
        pure (classifyCheck out)

{- | Drop a proposal that cannot compile, returning the empty check so the
caller falls through to its no-check path. The discard is disclosed: a silently
dropped check would leave the user believing one was offered.
-}
vetProposal :: Call -> Text -> IO Text
vetProposal call proposed = do
    owned <- ownedBindingTypes call
    vetProposalAgainst call owned proposed

{- | The task's own bindings and their types: @list_cells@ for the names each
cell defines, then @check_type@ for the type that decides which
perturbations apply. Bounded, since this runs before every proposal.
-}
ownedBindingTypes :: Call -> IO [(Text, Text)]
ownedBindingTypes call = do
    cells <- call ListCells (object ["full" .= False])
    let names = take maxOwnedBindings (definesOf (payloadOf cells))
    concat <$> mapM typeOf names
  where
    typeOf n = do
        r <- call CheckType (object ["expr" .= n])
        let ty = signatureOf n (fieldText "result" (payloadOf r))
        pure [(n, ty) | not (T.null ty)]

maxOwnedBindings :: Int
maxOwnedBindings = 8

-- | Every @defines@ name across the notebook's cells.
definesOf :: Value -> [Text]
definesOf v =
    [ n
    | Array cs <- [fieldValue "cells" v]
    , c <- toList cs
    , Array ds <- [fieldValue "defines" c]
    , String n <- toList ds
    ]

-- | The type from a @check_type@ answer (@name :: ty@), else the whole line.
signatureOf :: Text -> Text -> Text
signatureOf name res = case T.breakOn " :: " firstLine of
    (lhs, rest)
        | not (T.null rest)
        , T.strip lhs == name ->
            T.strip (T.drop 4 rest)
    _ -> ""
  where
    firstLine = fromMaybe "" (listToMaybe (T.lines res))

payloadOf :: Either Text ToolOutcome -> Value
payloadOf (Right o) = toolOutcomeValue o
payloadOf _ = object []

fieldValue :: Text -> Value -> Value
fieldValue k (Object o) = fromMaybe Null (KM.lookup (Key.fromText k) o)
fieldValue _ _ = Null

fieldText :: Text -> Value -> Text
fieldText k v = case fieldValue k v of
    String s -> s
    _ -> ""

{- | 'vetProposal' plus the symbolic gates (C2 tasks 2 and 3): the proposal
must compile, REFERENCE a binding the task's own cells define, and be
falsifiable by some perturbation of that binding's value. A check that
cannot fail is not a test, however cleanly it compiles.
-}
vetProposalAgainst :: Call -> [(Text, Text)] -> Text -> IO Text
vetProposalAgainst call ownedTypes proposed = do
    verdict <- vetCheckWith call proposed
    if verdict == CheckUncheckable
        then discard "does not compile"
        else case referenceGate (map fst ownedTypes) proposed of
            Just r | not (null ownedTypes) -> discard (refusalNote r)
            _ -> do
                discriminating <- survivesMutation call ownedTypes proposed
                if discriminating
                    then pure proposed
                    else discard (refusalNote Indiscriminate)
  where
    discard why = do
        TIO.putStrLn ("  \9888 discarded a check: " <> why <> ": " <> proposed)
        pure ""

{- | Does some perturbation of a referenced binding falsify the check? Each
trial runs through @try@, so nothing is committed. A binding whose type
yields no perturbation cannot decide, so it abstains rather than refusing.
-}
survivesMutation :: Call -> [(Text, Text)] -> Text -> IO Bool
survivesMutation call ownedTypes check
    | null perturbed = pure True
    | otherwise = do
        results <- mapM (fmap (== CheckFailed) . vetCheckWith call) perturbed
        pure (or results)
  where
    perturbed =
        [ perturbCheck name p check
        | (name, ty) <- ownedTypes
        , mentionsAny [name] check
        , p <- perturbationsFor ty name
        ]
