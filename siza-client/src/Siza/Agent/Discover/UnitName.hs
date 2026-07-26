module Siza.Agent.Discover.UnitName (
    unitPackageName,
    scrubUnitTokens,
    scrubCardUnits,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Interpret (stripVersion)

unitPackageName :: Text -> Text
unitPackageName w
    | plain /= base && versionTail base = plain
    | otherwise = w
  where
    base = T.takeWhile (/= ':') w
    plain = stripVersion base
    versionTail b = case reverse (T.splitOn "-" b) of
        (v : _ : _) ->
            not (T.null v)
                && T.any isDigit v
                && T.all (\c -> isDigit c || c == '.') v
        _ -> False

scrubUnitTokens :: Text -> Text
scrubUnitTokens = T.unwords . map unitPackageName . T.words

scrubCardUnits :: Value -> Value
scrubCardUnits (Object o) =
    Object
        (adjustText unitPackageName "package" (adjustText scrubUnitTokens "cabal" o))
  where
    adjustText f k m = case KM.lookup k m of
        Just (String s) -> KM.insert k (String (f s)) m
        _ -> m
scrubCardUnits v = v
