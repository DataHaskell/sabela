{-# LANGUAGE OverloadedStrings #-}

{- | Stage 0 of the discover ranking (docs/discover/search-api.md section 7):
exact-name lookups DISTINCT from every fuzzy scan — the name looked up in the
notebook's imported (loaded) modules via the session, and the hoogle
exact-name query — so stratum membership is a lookup, not a ranking accident.
-}
module Siza.Agent.Discover.Exact (stageZero) where

import Data.Aeson (Value (..), object, (.=))
import Data.List (nub)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Classify (capabilityAnswer, sessionAnswer)
import Siza.Agent.Discover.Types (
    DHit (..),
    Interpreted (..),
    NotebookEnv (..),
    SourceAnswer (..),
 )

-- | Imported-module browses per name query, bounding the lookup fan-out.
maxBrowses :: Int
maxBrowses = 3

-- | Exact hoogle lookups one prose query may fan out to, bounding the cost.
maxProseExact :: Int
maxProseExact = 3

{- | The stage-0 answers, by query shape. Name\/module\/package shapes run
the exact hoogle lookup on the resolved token; PROSE runs one bounded exact
lookup per usable term — the lexical hoogle tier is never shape-gated
(section 2: a fuzzy-channel miss must not hide an exactly-named deliverable).
Session hits are kept to the exact name only; hoogle hits to exact
name\/module\/package matches, so a backend that ignores the exact flag
cannot flood the merge with a fuzzy wall.
-}
stageZero ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    NotebookEnv ->
    Interpreted ->
    IO [SourceAnswer]
stageZero call env interp = case iShape interp of
    "name" -> do
        sess <- mapM lookupIn (take maxBrowses targets)
        hoo <- exactHoogle n
        pure (catMaybes sess ++ hoo)
    "module" -> do
        -- The session's own browse of the module is stage-0 evidence too
        -- (section 3.3): its card must resolve the name in EVERY mode.
        sess <- lookupModule n
        hoo <- exactHoogle n
        pure (maybeToList sess ++ hoo)
    "package" -> exactHoogle n
    "prose" ->
        concat <$> mapM exactHoogle (take maxProseExact usableTerms)
    _ -> pure []
  where
    n = iName interp
    usableTerms = [t | t <- iTerms interp, T.length t >= 3]
    targets =
        nub (maybeToList (iScope interp) ++ map fst (neImportCells env))
    lookupIn m = do
        r <- callOk (call FindFunction (object ["query" .= m]))
        pure (exactOnly . sessionAnswer interp . Just <$> r)
    lookupModule m = do
        r <- callOk (call FindFunction (object ["query" .= m]))
        pure (moduleOnly . sessionAnswer interp . Just <$> r)
    -- A module browse keeps its card and the module's own attributed hits.
    moduleOnly a =
        a{saHits = [h | h <- saHits a, dhModule h == n || dhName h == n]}
    exactHoogle t = do
        r <-
            callOk . call SearchCapability $
                object ["query" .= t, "exact" .= True, "semantic" .= False]
        pure [exactHits t (capabilityAnswer interp (Just v)) | Just v <- [r]]
    exactOnly a =
        a
            { saHits = [h | h <- saHits a, dhName h == n]
            , saCard = Nothing
            , saPkgModules = []
            }
    exactHits t a = a{saHits = filter (exactFor t) (saHits a)}
    exactFor t h =
        dhName h == t
            || dhModule h == t
            || T.toLower (dhName h) == lt
            || T.toLower (dhModule h) == lt
            || T.toLower (dhPackage h) == lt
      where
        lt = T.toLower t

callOk :: IO (Either Text ToolOutcome) -> IO (Maybe Value)
callOk act = do
    r <- act
    pure $ case r of
        Right (ToolOk v) -> Just v
        _ -> Nothing
