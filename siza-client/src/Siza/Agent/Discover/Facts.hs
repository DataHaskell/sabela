{-# LANGUAGE OverloadedStrings #-}

{- | The held-fact list and its one fold (search-api.md section 8): bounded
size, first-seen order, and the install-state replacement rule. Shared by the
harvest path ('Siza.Agent.Discover.Advice') and the ledger's own writers, so
every fact — harvested or probed — enters the list the same way.
-}
module Siza.Agent.Discover.Facts (
    foldFacts,
    installFactKey,
    maxHeldFacts,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (InstallState, installText)

maxHeldFacts :: Int
maxHeldFacts = 8

{- | Fold new facts into a bounded held list. A fresh install-state fact
REPLACES the package's earlier one — one package never holds two at once.
-}
foldFacts :: [Text] -> [Text] -> [Text]
foldFacts new facts = take maxHeldFacts (foldl addFact facts new)
  where
    addFact acc f
        | f `elem` acc || T.null f = acc
        | Just p <- installFactKey f =
            [g | g <- acc, installFactKey g /= Just p] ++ [f]
        | otherwise = acc ++ [f]

{- | The package of an install-state fact (@"pkg (state): …"@ as
'Siza.Agent.Discover.Advice.harvestFacts' shapes them); 'Nothing' for any
other held fact. The world-change wipe keys its fact reset on it.
-}
installFactKey :: Text -> Maybe Text
installFactKey f = case T.words f of
    (p : st : _)
        | "(" `T.isPrefixOf` st
        , T.dropAround (`elem` ("():" :: String)) st `elem` states ->
            Just p
    _ -> Nothing
  where
    states = map installText [minBound .. maxBound :: InstallState]
