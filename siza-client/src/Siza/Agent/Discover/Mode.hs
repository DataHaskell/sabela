{-# LANGUAGE OverloadedStrings #-}

{- | Which envelope a request's mode renders. A mode changes the rendering and
never the index, so a name that resolves is never denied because the mode had
no shape for it.
-}
module Siza.Agent.Discover.Mode (
    answerFor,
    asConstruct,
    isConstruct,
    modeRedirect,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Siza.Agent.Discover.Advice (setField)
import Siza.Agent.Discover.Hackage (hackageMatching, withFactsFor)
import Siza.Agent.Discover.Inventory (inventoryEnvelope, topicTokens)
import Siza.Agent.Discover.Merge (
    discoverEnvelopeRecent,
    discoverEnvelopeScoped,
 )
import Siza.Agent.Discover.Request (
    DiscoverMode (..),
    DiscoverRequest (..),
 )
import Siza.Agent.Discover.Types (
    HackageInfo,
    Interpreted (..),
    NotebookEnv,
    SourceAnswer,
 )

isConstruct :: DiscoverRequest -> Interpreted -> Bool
isConstruct req interp =
    drMode req == ModeConstruct || iShape interp == "construct"

asConstruct :: DiscoverRequest -> Interpreted -> Interpreted
asConstruct req interp
    | drMode req == ModeConstruct = interp{iShape = "construct"}
    | otherwise = interp

modeRedirect ::
    DiscoverRequest ->
    NotebookEnv ->
    Interpreted ->
    [SourceAnswer] ->
    HackageInfo ->
    Value ->
    Value
modeRedirect req env interp0 answers hk v
    | stateText v /= "not_found" = v
    | stateText searchV /= "found" = v
    | otherwise = setField "next" redirectNote searchV
  where
    searchV =
        discoverEnvelopeScoped env interp0 (drScope req) (drLimit req) answers hk
    redirectNote =
        "'"
            <> iName interp0
            <> "' resolves; mode="
            <> modeName
            <> " had no mode-shaped answer for it, so this is its search \
               \rendering (modes change the rendering, never the index)."
    modeName = case drMode req of
        ModeInventory -> "inventory"
        ModeConstruct -> "construct"
        ModeSearch
            | iShape interp0 == "construct" -> "construct"
            | otherwise -> "search"

stateText :: Value -> Text
stateText (Object o) = case KM.lookup "state" o of
    Just (String s) -> s
    _ -> ""
stateText _ = ""

answerFor ::
    [Text] ->
    DiscoverRequest ->
    NotebookEnv ->
    Interpreted ->
    [SourceAnswer] ->
    HackageInfo ->
    IO Value
answerFor recent req env interp answers hk = case drMode req of
    ModeSearch ->
        pure
            ( discoverEnvelopeRecent
                recent
                env
                interp
                (drScope req)
                (drLimit req)
                answers
                hk
            )
    ModeInventory -> do
        lexical <- hackageMatching lexicalCap (topicTokens interp)
        hkL <- withFactsFor lexical hk
        pure
            ( inventoryEnvelope
                env
                interp
                (drScope req)
                (drLimit req)
                answers
                hkL
                lexical
            )
    ModeConstruct ->
        pure
            ( discoverEnvelopeScoped
                env
                interp
                (drScope req)
                (drLimit req)
                answers
                hk
            )

-- | How many lexical package-name matches an inventory listing draws on.
lexicalCap :: Int
lexicalCap = 25
