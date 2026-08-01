{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Resolve (
    lookupByName,
    resolutionImport,
    resolveName,
) where

import Data.Text (Text)

import Sabela.AI.Capability (Capability (..), parseCapabilities)
import Sabela.Diagnose.Packages (findModulePackage)
import Sabela.SessionTypes (SessionBackend (..))

lookupByName :: Text -> [Capability] -> [Capability]
lookupByName name = filter ((== name) . capName)

{- | The import line for a capability, and the package that exposes its module.
The package comes from the installed package db rather than a curated table, so
it answers for every installed package instead of eight.
-}
resolutionImport :: Capability -> IO (Text, Maybe Text)
resolutionImport cap = do
    mPkg <- findModulePackage (capModule cap)
    pure ("import " <> capModule cap, mPkg)

resolveName :: SessionBackend -> [Text] -> Text -> IO [Capability]
resolveName backend mods name = do
    caps <- concat <$> mapM browse mods
    pure (lookupByName name caps)
  where
    browse m = parseCapabilities m <$> sbQueryBrowse backend m
