{- | Version-qualified GHC unit labels (@pkg-1.2.3:sublib@) are compiler
internals no harness channel may carry (R3.10/P6): the card scrub collapses
them to the installable package name. Keyed on the version-qualified evidence
class alone — plain names and unversioned sublib refs pass untouched.
-}
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

{- | A version-qualified unit label collapsed to its installable package name
(@dataframe-core-2.0.0.0:internal@ -> @dataframe-core@); anything without a
version segment is returned unchanged.
-}
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

-- | Word-wise scrub for free-text fields (cabal lines, notes).
scrubUnitTokens :: Text -> Text
scrubUnitTokens = T.unwords . map unitPackageName . T.words

-- | Scrub a card's @package@ and @cabal@ fields at the emitting seam.
scrubCardUnits :: Value -> Value
scrubCardUnits (Object o) =
    Object
        (adjustText unitPackageName "package" (adjustText scrubUnitTokens "cabal" o))
  where
    adjustText f k m = case KM.lookup k m of
        Just (String s) -> KM.insert k (String (f s)) m
        _ -> m
scrubCardUnits v = v
