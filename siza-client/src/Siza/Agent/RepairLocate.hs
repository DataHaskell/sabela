{-# LANGUAGE OverloadedStrings #-}

{- | The discover-backed locators the repair cascade's tiers draw on (R7.6:
one catalogue, two consumers — healing can never propose what search would
deny). A qualified name queries its BARE spelling only (R3.1).
-}
module Siza.Agent.RepairLocate (
    discoverModules,
    discoverRenames,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleRepair (editDistance)
import Sabela.AI.RepairDispatch (
    missingModuleFromDiag,
    notInScopeFromDiag,
 )
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.RepairTiers (splitQualified)

type Dispatch = ToolCall -> IO (Either Text ToolOutcome)

{- | Resolve the diagnostic's wrong name through the SAME discover catalogue
search answers from. A qualified name (@T.unpack@) queries its bare name.
-}
discoverModules :: Dispatch -> Text -> IO [(Text, Text, Maybe Text)]
discoverModules disp errText = case notInScopeFromDiag errText of
    Nothing -> pure []
    Just w -> do
        let bare = snd (splitQualified w)
        out <- disp (ToolCall "discover" (object ["query" .= bare]))
        pure $ case out of
            Right (ToolOk v) ->
                [ (bare, m, pkgOf h)
                | h <- hitsOf v
                , hitText "name" h == bare
                , let m = hitText "module" h
                , not (T.null m)
                , m /= "(not installed)"
                ]
            _ -> []
  where
    pkgOf h =
        let inst = hitText "install" h
         in if inst `elem` ["hidden", "absent-known"]
                then Just (hitText "package" h)
                else Nothing

{- | Nearest catalogue modules for a could-not-find-module diagnostic, via
the same discover path — the TierModuleRename locator (Data.Frame class).
-}
discoverRenames :: Dispatch -> Text -> IO [(Text, Text)]
discoverRenames disp errText = case missingModuleFromDiag errText of
    Nothing -> pure []
    Just wrong -> do
        out <- disp (ToolCall "discover" (object ["query" .= wrong]))
        pure $ case out of
            Right (ToolOk v) ->
                [ (wrong, m)
                | m <-
                    take 3 . sortOn (editDistance wrong) . nub $
                        [ hitText "module" h
                        | h <- hitsOf v
                        , not (T.null (hitText "module" h))
                        , hitText "module" h /= "(not installed)"
                        ]
                ]
            _ -> []

hitsOf :: Value -> [Value]
hitsOf (Object o) | Just (Array a) <- KM.lookup "hits" o = toList a
hitsOf _ = []

hitText :: Text -> Value -> Text
hitText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
hitText _ _ = ""
