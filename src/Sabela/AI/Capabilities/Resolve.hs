{-# LANGUAGE OverloadedStrings #-}

{- | Phase-0 name→module resolution for the neuro-symbolic Hackage search: given a
not-in-scope name the model used, find the installed module(s) that export it (by
browsing) and the import + package a cell needs to use it. The pure core (exact-name
lookup + the import line) is shared by the resolve tool and its tests; the browse is
the live half. Reuses the @find_function@ machinery ('parseCapabilities') and the
curated 'packageForModule'.
-}
module Sabela.AI.Capabilities.Resolve (
    lookupByName,
    resolutionImport,
    resolveName,
) where

import Data.Text (Text)

import Sabela.AI.Capability (Capability (..), parseCapabilities)
import Sabela.Diagnose.Packages (packageForModule)
import Sabela.SessionTypes (SessionBackend (..))

{- | Exact top-level-name matches over a capability index — the pure core the
resolve tool and its tests share (whole-name, not substring).
-}
lookupByName :: Text -> [Capability] -> [Capability]
lookupByName name = filter ((== name) . capName)

{- | The import line and the package a resolution needs declared: the module's
@import@, plus the curated 'packageForModule' for its @-- cabal: build-depends:@.
'Nothing' package when the module is not in the curated table (the dep fixer's
runtime backstop then handles it).
-}
resolutionImport :: Capability -> (Text, Maybe Text)
resolutionImport cap =
    ("import " <> capModule cap, packageForModule (capModule cap))

{- | Browse the given installed modules and return those that export @name@. The
live half; the caller scopes the module list (the gate scopes to its package set,
so the per-call browse cost is bounded).
-}
resolveName :: SessionBackend -> [Text] -> Text -> IO [Capability]
resolveName backend mods name = do
    caps <- concat <$> mapM browse mods
    pure (lookupByName name caps)
  where
    browse m = parseCapabilities m <$> sbQueryBrowse backend m
