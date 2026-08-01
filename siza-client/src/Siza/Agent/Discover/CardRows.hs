{- | A module listing becomes hits. One @:browse@ declaration is one row, and a
row becomes a hit only when its head is a name the caller could write:
presenting a fragment of a wrapped signature as a callable name is the harness
inventing an API. Rows that do not parse are counted and disclosed.
-}
module Siza.Agent.Discover.CardRows (
    cardHits,
    cardUnparsedNote,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.LeakShape (lexicalEntity)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    mkHit,
 )

cardHits :: Interpreted -> Value -> [DHit]
cardHits interp v = map hitOf (fst (splitRows v))
  where
    modName = case v of
        Object o -> textAt "module" o
        _ -> ""
    hitOf (n, ty) =
        (mkHit n modName "")
            { dhType = ty
            , dhInstall = InstInstalled
            , dhOrigin = "session"
            , dhKind = if n == iName interp then MkExact else MkModule
            }

cardUnparsedNote :: Value -> Text
cardUnparsedNote v
    | dropped <= 0 = ""
    | otherwise =
        "card coverage partial: "
            <> T.pack (show dropped)
            <> " listing rows did not parse as declarations"
  where
    dropped = snd (splitRows v)

splitRows :: Value -> ([(Text, Text)], Int)
splitRows (Object o) = (readable, length rows - length readable)
  where
    rows =
        [ splitSig line
        | Just (Array es) <- [KM.lookup "exports" o]
        , String line <- toList es
        ]
    readable = [r | r@(n, _) <- rows, lexicalEntity n]
splitRows _ = ([], 0)

splitSig :: Text -> (Text, Text)
splitSig line = case T.breakOn "::" (declHead line) of
    (n, rest)
        | T.null rest -> (T.strip n, "")
        | otherwise -> (T.strip n, T.strip (T.drop 2 rest))

{- | A declaration row names its head, not its keyword: @data Value@ is the
entity @Value@.
-}
declHead :: Text -> Text
declHead l = case T.words l of
    (kw : nm : _) | kw `elem` declKeywords -> nm
    _ -> l
  where
    declKeywords = ["type", "data", "newtype", "class", "pattern"] :: [Text]

textAt :: K.Key -> KM.KeyMap Value -> Text
textAt k o = case KM.lookup k o of
    Just (String s) -> s
    _ -> ""
