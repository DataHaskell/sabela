{-# LANGUAGE OverloadedStrings #-}

{- | The @find_function@ tool: a Hoogle-style search over the live session's
installed modules. A keyword query ranks every exposed function with
"Sabela.AI.Capability"; an exact module-name query returns that module's raw
@:browse@ listing (all exports — values, types, classes — not just value bindings).
-}
module Sabela.AI.Capabilities.ModuleSearch (execFindFunction) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Capability (
    Capability (..),
    Hit (..),
    Match (..),
    defaultSynonyms,
    parseCapabilities,
    searchCapabilities,
 )
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Diagnose (diagnose, guidancePairs)
import Sabela.SessionTypes (SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession)

{- | @query@ is a keyword (@animate@) or a module name (which returns that
module's raw exports). Ranks the installed modules' functions; returns nothing
on a true miss — no misleading near-match. To search by TYPE use @find_by_type@.
-}
execFindFunction :: App -> Value -> IO ToolOutcome
execFindFunction app input =
    if T.null q
        then
            pure
                ( errOutcome
                    (errorJson "query required (a keyword or a module name)")
                )
        else do
            mBackend <- getHaskellSession (appSessions app)
            case mBackend of
                Nothing ->
                    pure
                        ( errOutcome
                            (errorJson "No live Haskell session — run a cell first to start GHCi.")
                        )
                Just backend -> do
                    mods <- sbQueryComplete backend "import "
                    if q `elem` mods
                        then do
                            raw <- sbQueryBrowse backend q
                            pure (browseOutcome q raw)
                        else do
                            caps <- buildIndex backend (take maxIndexModules (filter interesting mods))
                            pure (matchesOutcome q (searchCapabilities defaultSynonyms caps q))
  where
    q =
        let qq = fieldText "query" input
         in if T.null qq then fieldText "module" input else qq

-- | Cap on modules browsed per keyword search, to bound the round-trip cost.
maxIndexModules :: Int
maxIndexModules = 120

{- | Browse each module once and parse its value bindings into capabilities: the
per-call index the keyword search ranks over.
-}
buildIndex :: SessionBackend -> [Text] -> IO [Capability]
buildIndex backend mods =
    concat <$> mapM (\m -> parseCapabilities m <$> sbQueryBrowse backend m) mods

{- | Keep the discovery-relevant modules: drop the base/boot namespaces the model
already knows, by first path component (keeps @DataFrame.*@, @Granite.*@,
@Sabela.Notebook.*@ and declared deps; drops @Data.*@, @Control.*@, @GHC.*@).
-}
interesting :: Text -> Bool
interesting m = T.takeWhile (/= '.') m `notElem` baseNamespaces

baseNamespaces :: [Text]
baseNamespaces =
    [ "GHC"
    , "Data"
    , "Control"
    , "System"
    , "Text"
    , "Type"
    , "Foreign"
    , "Numeric"
    , "Prelude"
    , "Debug"
    , "Unsafe"
    , "Language"
    ]

-- | Shape ranked hits into @{query, matches: [{module, name, type, via}]}@.
matchesOutcome :: Text -> [Hit] -> ToolOutcome
matchesOutcome q hits =
    okOutcome $ object ["query" .= q, "matches" .= map hitJSON hits]
  where
    hitJSON h =
        object
            [ "module" .= capModule (hitCap h)
            , "name" .= capName (hitCap h)
            , "type" .= capType (hitCap h)
            , "via" .= matchName (hitVia h)
            ]

matchName :: Match -> Text
matchName ByName = "name"
matchName ByType = "type"
matchName BySynonym = "synonym"
matchName ByModule = "module"

{- | A module-name query returns the module's RAW @:browse@ listing — every
export (values, types, classes), not just the value bindings 'parseCapabilities'
keeps — with the same @-- cabal:@ guidance a failed cell gets, so a hidden-package
wall becomes an actionable dependency hint.
-}
browseOutcome :: Text -> Text -> ToolOutcome
browseOutcome m raw =
    okOutcome $
        object (["module" .= m, "exports" .= raw] <> guidancePairs (diagnose raw))
