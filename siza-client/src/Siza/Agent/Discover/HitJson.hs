{- | Reading a source's JSON: the hit every reader starts from, and the fields
it is built out of. A field a source did not state reads as empty, never as
a stand-in value.
-}
module Siza.Agent.Discover.HitJson (
    baseHit,
    maybeTextAt,
    textAt,
    textAt',
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    MatchKind (..),
 )

baseHit :: Text -> Text -> Text -> DHit
baseHit n m p =
    DHit n "" m p "" InstAbsentUnknown MkSemantic "" Nothing Nothing Nothing

textAt :: K.Key -> KM.KeyMap Value -> Text
textAt k o = case KM.lookup k o of
    Just (String s) -> s
    _ -> ""

textAt' :: K.Key -> Value -> Text
textAt' k (Object o) = textAt k o
textAt' _ _ = ""

maybeTextAt :: K.Key -> Value -> Maybe Text
maybeTextAt k (Object o) = case KM.lookup k o of
    Just (String s) -> Just s
    _ -> Nothing
maybeTextAt _ _ = Nothing
