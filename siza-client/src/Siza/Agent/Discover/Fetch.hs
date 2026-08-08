{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Fetch (
    blankPayload,
    capabilityArgs,
    fetchNotebookEnv,
    fetchOk,
    fetchSession,
    fetchSessionScoped,
    probeHidden,
    queryVariants,
) where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isUpper)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Classify (sessionAnswer)
import Siza.Agent.Discover.Interpret (
    envFromCells,
    parseCells,
    signatureRequest,
 )
import Siza.Agent.Discover.Types (
    Interpreted (..),
    NotebookEnv,
    Scope (..),
    SourceAnswer (..),
 )

blankPayload :: Value -> Bool
blankPayload (Object o) = case KM.lookup "status" o of
    Just (String st) -> st == "error"
    _ -> all blankPayload (KM.elems o)
blankPayload (Array a) = null a
blankPayload _ = True

queryVariants :: Text -> [Text]
queryVariants q =
    t : [bare | Just bare <- [unqualify t], bare /= t] <> valueTokens
  where
    t = fromMaybe (T.strip q) (signatureRequest q)
    unqualify s
        | T.any (== '.') s
        , not (" " `T.isInfixOf` s)
        , Just (c0, _) <- T.uncons s
        , isUpper c0
        , lastComp <- last (T.splitOn "." s)
        , Just (c, _) <- T.uncons lastComp
        , not (isUpper c) =
            Just lastComp
        | otherwise = Nothing
    valueTokens = case T.words t of
        [a, b] | isValueWord a, isTypeWord b -> [a]
        [a, b] | isTypeWord a, isValueWord b -> [b]
        _ -> []
    isValueWord w =
        T.length w >= 3
            && not (T.any (== '.') w)
            && maybe False (not . isUpper . fst) (T.uncons w)
    isTypeWord w = maybe False (isUpper . fst) (T.uncons (T.dropWhile (== '.') w))

{- | A scoped request is a precise one, so the scope rides along: searched
globally, a package the caller named loses on rank to whatever else answers to
the same word, and the scope filter then has nothing of theirs to keep.
-}
capabilityArgs :: Bool -> Scope -> Interpreted -> Value
capabilityArgs capSearch scope interp =
    object
        ( ["query" .= iRaw interp, "semantic" .= capSearch]
            ++ ["package" .= p | Just p <- [scPackage scope]]
            ++ ["module" .= m | Just m <- [scModule scope]]
        )

fetchNotebookEnv ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) -> IO NotebookEnv
fetchNotebookEnv call = do
    r <- call ListCells (object ["full" .= True])
    pure . envFromCells $ case r of
        Right (ToolOk v) -> parseCells v
        _ -> []

fetchSession ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Interpreted ->
    IO (Maybe Value)
fetchSession call = fetchSessionScoped call Nothing

fetchSessionScoped ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Maybe Text ->
    Interpreted ->
    IO (Maybe Value)
fetchSessionScoped call mModule interp = go variants Nothing Nothing
  where
    variants =
        iName interp
            : [v | v <- queryVariants (iRaw interp), v /= iName interp]
    scopePairs = ["module" .= m | Just m <- [mModule]]
    go [] best fallback = pure (best `orElse` fallback)
    go (v : rest) best fallback = do
        r <- call FindFunction (object (("query" .= v) : scopePairs))
        case r of
            Right (ToolOk payload)
                | namesMatches payload -> pure (Just payload)
                | not (blankPayload payload) ->
                    go rest (best `orElse` Just payload) fallback
                | otherwise -> go rest best (fallback `orElse` Just payload)
            _ -> go rest best fallback
    orElse a b = a <|> b

namesMatches :: Value -> Bool
namesMatches (Object o) = case KM.lookup "matches" o of
    Just (Array ms) -> not (null ms)
    _ -> False
namesMatches _ = False

fetchOk ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    ToolName ->
    Value ->
    IO (Maybe Value)
fetchOk call tn args = do
    r <- call tn args
    pure $ case r of
        Right (ToolOk v) -> Just v
        _ -> Nothing

{- | Card a candidate package we hold no card for, by probing one of its
modules. The caller's terms ride along, so the card is ranked against the need
that raised it rather than against nothing.
-}
probeHidden ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Interpreted ->
    [SourceAnswer] ->
    IO [SourceAnswer]
probeHidden call interp answers
    | any (isJust . saCard) answers = pure []
    | sessionDown = pure []
    | otherwise = fmap concat . mapM probeOne $ take maxProbes probeTargets
  where
    sessionDown =
        any (\a -> saSource a == "session" && not (saOk a)) answers
    probeOne m = do
        r <-
            fetchOk
                call
                FindFunction
                (object ["module" .= m, "query" .= probeTerms m])
        pure [sessionAnswer interp r | isJust r]
    probeTerms m = if T.null (iName interp) then m else iName interp
    probeTargets =
        nubOrd
            [ m
            | (_, mods) <-
                sortOn
                    (not . queryNamed . fst)
                    (nubOrdOn fst (concatMap saPkgModules answers))
            , m <- take 1 mods
            ]
    queryNamed p =
        T.toLower p `elem` map T.toLower (iName interp : iTerms interp)

maxProbes :: Int
maxProbes = 3

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nubOrdOn id

nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn f = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | f x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert (f x) seen) xs
