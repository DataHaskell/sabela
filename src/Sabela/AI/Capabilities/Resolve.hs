{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Resolve (
    lookupByName,
    resolutionImport,
    resolveName,
) where

import Data.Text (Text)

import Sabela.AI.Capability (Capability (..), parseCapabilities)
import Sabela.Diagnose.Packages (packageForModule)
import Sabela.SessionTypes (SessionBackend (..))

lookupByName :: Text -> [Capability] -> [Capability]
lookupByName name = filter ((== name) . capName)

resolutionImport :: Capability -> (Text, Maybe Text)
resolutionImport cap =
    ("import " <> capModule cap, packageForModule (capModule cap))

resolveName :: SessionBackend -> [Text] -> Text -> IO [Capability]
resolveName backend mods name = do
    caps <- concat <$> mapM browse mods
    pure (lookupByName name caps)
  where
    browse m = parseCapabilities m <$> sbQueryBrowse backend m
