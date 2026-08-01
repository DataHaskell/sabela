{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Check.Vet (
    CheckScope (..),
    checkScopeFor,
    emptyScope,
    notebookBindingNames,
    ownedDefines,
    typedScope,
    vetCheckWith,
    vetProposal,
    vetProposalAgainst,
    vetVerdictAgainst,
    survivesMutation,
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
import Siza.Agent.Check (CheckResult (..), classifyTryBool)
import Siza.Agent.Check.Gate (
    CheckRefusal (..),
    MutationOutcome (..),
    mentionsAny,
    perturbCheck,
    perturbationsFor,
    referenceGate,
    refusalNote,
 )

type Call = ToolName -> Value -> IO (Either Text ToolOutcome)

{- | The names a covering check may be about, and the subset of them the
harness could type. Callers hand this in, so the verifier is keyed to the
scope of the request rather than to whatever the notebook happens to hold.
-}
data CheckScope = CheckScope
    { csNames :: [Text]
    , csTypes :: [(Text, Text)]
    }
    deriving (Eq, Show)

emptyScope :: CheckScope
emptyScope = CheckScope [] []

typedScope :: [(Text, Text)] -> CheckScope
typedScope tys = CheckScope (map fst tys) tys

{- | Probe the check with `try`: the bare expression, whose Bool value is the
verdict. A wrapped putStrLn would be IO, which `try` declines to run.
-}
vetCheckWith :: Call -> Text -> IO CheckResult
vetCheckWith call check
    | T.null (T.strip check) = pure CheckNotApplicable
    | otherwise =
        classifyTryBool
            <$> call
                Try
                (object ["code" .= check, "language" .= ("Haskell" :: Text)])

vetProposal :: Call -> [Text] -> Text -> IO Text
vetProposal call names proposed = do
    scope <- checkScopeFor call names
    vetProposalAgainst call scope proposed

-- | Type the names in scope. Only names the kernel can type can be perturbed.
checkScopeFor :: Call -> [Text] -> IO CheckScope
checkScopeFor call names = do
    tys <- concat <$> mapM typeOf capped
    pure (CheckScope capped tys)
  where
    capped = take maxOwnedBindings names
    typeOf n = do
        r <- call CheckType (object ["expr" .= n])
        let ty = signatureOf n (fieldText "result" (payloadOf r))
        pure [(n, ty) | not (T.null ty)]

{- | Every name the notebook's cells declare. The scope a caller who names no
scope of its own gets — an explicit widening, not a silent one.
-}
notebookBindingNames :: Call -> IO [Text]
notebookBindingNames call =
    definesOf . payloadOf <$> call ListCells (object ["full" .= False])

{- | The names the cells this turn owns declare, as the notebook itself
reports them in its listing. The reference set a covering check must be about
is read from the notebook's own answer, never re-derived from source text.
-}
ownedDefines :: [Int] -> Either Text ToolOutcome -> [Text]
ownedDefines cids = definesWhere (maybe False (`elem` cids)) . payloadOf

maxOwnedBindings :: Int
maxOwnedBindings = 8

definesOf :: Value -> [Text]
definesOf = definesWhere (const True)

definesWhere :: (Maybe Int -> Bool) -> Value -> [Text]
definesWhere keep v =
    [ n
    | Array cs <- [fieldValue "cells" v]
    , c <- toList cs
    , keep (cellIdOf c)
    , Array ds <- [fieldValue "defines" c]
    , String n <- toList ds
    ]

cellIdOf :: Value -> Maybe Int
cellIdOf c = case fieldValue "id" c of
    Number i -> Just (round i)
    _ -> Nothing

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

{- | The vetting verdict: @Left@ why the check was discarded, @Right@ the
check itself when it survives. Writes nothing — stdout belongs to the caller.
-}
vetVerdictAgainst :: Call -> CheckScope -> Text -> IO (Either Text Text)
vetVerdictAgainst call scope proposed = do
    verdict <- vetCheckWith call proposed
    if verdict == CheckUncheckable
        then pure (Left "does not compile")
        else case referenceGate (csNames scope) proposed of
            Just r -> pure (Left (refusalNote r))
            Nothing -> mutationVerdict <$> survivesMutation call scope proposed
  where
    mutationVerdict Discriminating = Right proposed
    mutationVerdict NotDiscriminating = Left (refusalNote Indiscriminate)
    mutationVerdict NoPerturbationAvailable = Left (refusalNote Unperturbable)

-- | 'vetVerdictAgainst' for the CLI: narrates a discard and yields no check.
vetProposalAgainst :: Call -> CheckScope -> Text -> IO Text
vetProposalAgainst call scope proposed =
    vetVerdictAgainst call scope proposed >>= \case
        Right kept -> pure kept
        Left why -> do
            TIO.putStrLn ("  \9888 discarded a check: " <> why <> ": " <> proposed)
            pure ""

{- | A check no perturbation can falsify is not a passed mutation check; it is
a check the machinery could not run.
-}
survivesMutation :: Call -> CheckScope -> Text -> IO MutationOutcome
survivesMutation call scope check
    | null perturbed = pure NoPerturbationAvailable
    | otherwise = do
        results <- mapM (fmap (== CheckFailed) . vetCheckWith call) perturbed
        pure (if or results then Discriminating else NotDiscriminating)
  where
    perturbed =
        [ perturbCheck name p check
        | (name, ty) <- csTypes scope
        , mentionsAny [name] check
        , p <- perturbationsFor ty name
        ]
