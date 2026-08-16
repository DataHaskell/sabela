{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Façade verification against a real session: candidates are discovered
loosely (hoogle, dotted ancestors), but a claim exists only when a probe in a
matching package environment reports the exact defining identity.
-}
module Sabela.AI.TypeOriginProbe (
    ProbeRunner,
    annotateDisposableWith,
    annotatePureEvalWith,
    claimsWith,
    exportedByPairs,
    facadeClaims,
    prefixCandidates,
) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM)
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Pair)
import Data.List (inits, nub)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Edit.ScratchVet (
    scopeEstablished,
    scratchScopeReport,
 )
import Sabela.AI.HoogleClient (HoogleHit (..), queryAllDbs)
import Sabela.AI.Store (AIStore)
import Sabela.AI.TypeOrigin (
    OriginId (..),
    annotateExportedLines,
    facadeClaimKey,
    originsFromText,
    probeAccepts,
    rankVerified,
    renderClaim,
 )
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    MaterializeFailure (..),
 )
import Sabela.SessionTypes (SessionBackend (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App, getAIStore)

-- | Ask one candidate module for @:info@ on the name; Nothing = no scope.
type ProbeRunner = Text -> Text -> IO (Maybe Text)

{- | The verified (origin, façade) claims a raw GHC text supports, given a
probe runner and a candidate source. Pure orchestration; no claim survives
without a probe acceptance, and no probe runs without an origin.
-}
claimsWith ::
    ProbeRunner -> (OriginId -> IO [Text]) -> Text -> IO [(OriginId, Text)]
claimsWith runner candidatesOf raw =
    fmap catMaybes . forM origins $ \o -> do
        cands <- take candidateCap . nub <$> candidatesOf o
        verified <- filterM (verify o) cands
        pure ((,) o <$> listToMaybe (rankVerified (oiModule o) verified))
  where
    origins = take originCap (originsFromText raw)
    verify o cand = maybe False (probeAccepts o) <$> runner cand (oiName o)

{- | Claims verified against the shared scratch scope with the given deps.
Bounded: an absent store, a cold scratchpad, or the deadline all mean an
honest miss, never a guess.
-}
facadeClaims :: App -> [Text] -> Text -> IO [(OriginId, Text)]
facadeClaims app deps raw
    | null (originsFromText raw) = pure []
    | otherwise = do
        mStore <- getAIStore app
        case mStore of
            Nothing -> pure []
            Just store -> do
                r <-
                    try
                        ( timeout
                            probeDeadlineUs
                            ( claimsWith
                                (scratchRunner app store deps)
                                candidatesFor
                                raw
                            )
                        )
                pure $ case r of
                    Right (Just claims) -> claims
                    Right Nothing -> []
                    Left (_ :: SomeException) -> []

-- | The payload object, absent when there is nothing verified to say.
exportedByPairs :: [(OriginId, Text)] -> [Pair]
exportedByPairs [] = []
exportedByPairs claims =
    [ "exportedBy"
        .= object
            [ K.fromText (facadeClaimKey (map fst claims) o)
                .= renderClaim o facade
            | (o, facade) <- claims
            ]
    ]

-- | A disposable result with its stderr and failure message annotated.
annotateDisposableWith ::
    [(OriginId, Text)] -> DisposableResult -> DisposableResult
annotateDisposableWith [] r = r
annotateDisposableWith claims r =
    r
        { disposableStderr = ann (disposableStderr r)
        , disposableFailure = annF <$> disposableFailure r
        }
  where
    ann = annotateExportedLines claims
    annF f = f{failureMessage = ann (failureMessage f)}

-- | A pure-eval result with its error text annotated.
annotatePureEvalWith ::
    [(OriginId, Text)] -> ST.PureEvalResult -> ST.PureEvalResult
annotatePureEvalWith [] r = r
annotatePureEvalWith claims r =
    r{ST.pureEvalError = annotateExportedLines claims (ST.pureEvalError r)}

-- | Proper dotted ancestors of a module, nearest first; discovery only.
prefixCandidates :: Text -> [Text]
prefixCandidates m =
    [ T.intercalate "." pre
    | pre <- reverse (drop 2 (inits segs))
    , pre /= segs
    ]
  where
    segs = T.splitOn "." m

candidatesFor :: OriginId -> IO [Text]
candidatesFor o = do
    hoogleMods <- hoogleCandidates o
    pure (prefixCandidates (oiModule o) <> hoogleMods)

{- | Documented modules hoogle lists for (name, package) — candidates for the
probe, never evidence. Without a unit there is no package to scope by.
-}
hoogleCandidates :: OriginId -> IO [Text]
hoogleCandidates o = case originPackage o of
    Nothing -> pure []
    Just pkg -> do
        r <-
            try
                ( queryAllDbs
                    [ "search"
                    , "--count=20"
                    , "--json"
                    , T.unpack (oiName o <> " +" <> pkg <> " is:exact")
                    ]
                ) ::
                IO (Either SomeException [HoogleHit])
        pure $ case r of
            Right hits ->
                nub
                    [ hhModule h
                    | h <- hits
                    , hhName h == oiName o
                    , hhPackage h == pkg
                    , not (T.null (hhModule h))
                    ]
            Left _ -> []

originPackage :: OriginId -> Maybe Text
originPackage o = packageOfUnit <$> oiUnit o
  where
    packageOfUnit u = case break versionish (T.splitOn "-" u) of
        (name@(_ : _), _) -> T.intercalate "-" name
        _ -> u
    versionish s =
        not (T.null s) && T.all (\c -> c `elem` ("0123456789." :: String)) s

{- | One probe against the shared scratchpad: a fresh per-module alias keeps
scopes from bleeding between candidates. Never cached across calls — the
scratch environment's resolved plan can change under a dependency-text key.
-}
scratchRunner :: App -> AIStore -> [Text] -> ProbeRunner
scratchRunner app store deps cand name = do
    r <- try establishAndAsk :: IO (Either SomeException (Maybe Text))
    pure (either (const Nothing) id r)
  where
    alias = "_SabelaOriginProbe_" <> T.replace "." "_" cand
    establishAndAsk = do
        (backend, report) <-
            scratchScopeReport
                app
                store
                deps
                ("import qualified " <> cand <> " as " <> alias)
        if scopeEstablished report
            then Just <$> sbQueryInfo backend (alias <> "." <> name)
            else pure Nothing

originCap :: Int
originCap = 4

candidateCap :: Int
candidateCap = 3

probeDeadlineUs :: Int
probeDeadlineUs = 10 * 1000 * 1000
