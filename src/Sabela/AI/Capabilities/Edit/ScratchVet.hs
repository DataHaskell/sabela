{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Scratch-session candidate vetting: ask GHC, in the ISOLATED scratchpad,
whether a resolver candidate's name actually inhabits the goal type — before
anything is executed into the live session (a live-vetted reject leaks its
import into live scope). Kill-switch: @SABELA_SCRATCH_VET=0@.
-}
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

{- | Vet one candidate @(module, name)@ against the goal type in the scratch
session: replay the cell's imports, import the candidate module under a
collision-free alias, and @:type@-check the aliased name at the sanitized goal.
'Nothing' goal endorses (no oracle question to ask); any scratch failure —
missing module, uninstallable dep, type mismatch — declines. The live session
is never touched.
-}
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

{- | A vet may force a scratch rebuild with the candidate's package; a
pathological install must not eat the repair budget. Timeout is a decline.
-}
vetTimeoutMicros :: Int
vetTimeoutMicros = 60 * 1000 * 1000

-- | Vet verdicts appended to the file @SABELA_DEBUG_VET@ names; off unless set.
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

{- | The scratch session with the CELL's scope replayed (imports + type
synonyms) — the query surface for any probe over a goal type GHC may have
spelled with the cell's own names. Isolated: the live prompt is never touched.
-}
scratchScopeBackend :: App -> AIStore -> [Text] -> Text -> IO SessionBackend
scratchScopeBackend app store deps src = do
    backend <- ensureScratchpad app store Haskell deps
    mapM_ (sbRunBlock backend) (cellScopeLines src)
    pure backend

-- | A stable, collision-free qualified alias for a candidate module.
vetAlias :: Text -> Text
vetAlias m = "V_" <> T.replace "." "_" m

{- | The cell's imports AND single-line type synonyms, replayed so the goal
type — which GHC may spell with the cell's own synonym — parses in scratch.
-}
cellScopeLines :: Text -> [Text]
cellScopeLines src =
    [ T.strip l
    | l <- T.lines src
    , let s = T.stripStart l
    , "import " `T.isPrefixOf` s || "type " `T.isPrefixOf` s
    ]

{- | The one @:type@ query that asks GHC whether the candidate fits the goal:
apply it to a typed @undefined@ per goal argument and unify the result via
@asTypeOf@. A @::@ annotation would SKOLEMIZE the goal's inferred type
variables and demand the candidate be at least that polymorphic, declining
legitimate candidates; application unifies instead, and an arity mismatch
still declines.
-}
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

-- | The cell's own import lines, replayed so its types are in scratch scope.
cellImportLines :: Text -> [Text]
cellImportLines src =
    [T.strip l | l <- T.lines src, "import " `T.isPrefixOf` T.stripStart l]
