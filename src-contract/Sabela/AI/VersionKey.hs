{-# LANGUAGE OverloadedStrings #-}

{- | The dotted-version sort key shared by the sdist cache and the Hackage
facts builder, so both order a package's versions the same way.
-}
module Sabela.AI.VersionKey (versionKey) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Numeric sort key for a dotted version; a bad segment reads as 0.
versionKey :: Text -> [Int]
versionKey = map segInt . T.splitOn "."
  where
    segInt s = case reads (T.unpack s) of
        [(n, "")] -> n
        _ -> 0
