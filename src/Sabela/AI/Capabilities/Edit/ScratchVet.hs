{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.Edit.ScratchVet (
    cellImportLines,
    cellScopeLines,
    sanitizeGoal,
    scratchScopeBackend,
    scratchVet,
    splitArrows,
    vetAlias,
    vetProbe,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Scratchpad (ensureScratchpad)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.GoalText (sanitizeGoal, splitArrows)
import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.AI.Store (AIStore)
import Sabela.SessionTypes (CellLang (..), SessionBackend (..))
import Sabela.State (App)

scratchVet ::
    App -> AIStore -> Text -> [Text] -> Text -> Text -> Maybe Text -> IO Bool
scratchVet _ _ _ _ _ _ Nothing = pure True
scratchVet app store src deps modName name (Just goal) = do
    enabled <- featureEnabled "SABELA_SCRATCH_VET"
    if not enabled
        then pure True
        else do
            r <- try (timeout vetTimeoutMicros go)
            let verdict = case r of
                    Right (Just ok) -> ok
                    _ -> False
            debugDumpVet modName name goal r verdict
            pure verdict
  where
    go = do
        backend <- scratchScopeBackend app store deps src
        _ <-
            sbRunBlock
                backend
                ("import qualified " <> modName <> " as " <> vetAlias modName)
        resp <- sbQueryType backend (vetProbe modName name goal)
        pure (isClean (healthOfTypeQuery resp))

vetTimeoutMicros :: Int
vetTimeoutMicros = 60 * 1000 * 1000

debugDumpVet ::
    Text -> Text -> Text -> Either SomeException (Maybe Bool) -> Bool -> IO ()
debugDumpVet modName name goal r verdict = do
    mp <- lookupEnv "SABELA_DEBUG_VET"
    case mp of
        Just p
            | not (null p)
            , p /= "0" ->
                appendFile p . T.unpack $
                    "vet "
                        <> modName
                        <> "."
                        <> name
                        <> " :: "
                        <> goal
                        <> " -> "
                        <> T.pack (show verdict)
                        <> " ("
                        <> outcome
                        <> ")\n"
        _ -> pure ()
  where
    outcome = case r of
        Left e -> "exception: " <> T.take 120 (T.pack (show e))
        Right Nothing -> "timeout"
        Right (Just _) -> "probe"

scratchScopeBackend :: App -> AIStore -> [Text] -> Text -> IO SessionBackend
scratchScopeBackend app store deps src = do
    backend <- ensureScratchpad app store Haskell deps
    mapM_ (sbRunBlock backend) (cellScopeLines src)
    pure backend

vetAlias :: Text -> Text
vetAlias m = "V_" <> T.replace "." "_" m

cellScopeLines :: Text -> [Text]
cellScopeLines src =
    [ T.strip l
    | l <- T.lines src
    , let s = T.stripStart l
    , "import " `T.isPrefixOf` s || "type " `T.isPrefixOf` s
    ]

vetProbe :: Text -> Text -> Text -> Text
vetProbe modName name goal =
    "("
        <> vetAlias modName
        <> "."
        <> name
        <> T.concat [" (undefined :: " <> a <> ")" | a <- args]
        <> ") `asTypeOf` (undefined :: "
        <> res
        <> ")"
  where
    segs = splitArrows (sanitizeGoal goal)
    (args, res) = case reverse segs of
        (r : rest) -> (reverse rest, r)
        [] -> ([], "")

cellImportLines :: Text -> [Text]
cellImportLines src =
    [T.strip l | l <- T.lines src, "import " `T.isPrefixOf` T.stripStart l]
