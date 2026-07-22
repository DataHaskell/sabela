{- | Envelope sub-renderers shared by the discover merge (search-api.md
section 5): interpreted echo, per-source consulted rows, hackage row.
Split from "Siza.Agent.Discover.Merge" for the module-size cap.
-}
module Siza.Agent.Discover.Render (
    consultedJson,
    dedupSources,
    hackageJson,
    interpretedJson,
) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (
    HackageInfo (..),
    Interpreted (..),
    SourceAnswer (..),
 )

{- | One consulted row per backend: stage-0 and fuzzy passes share a source,
so a source is ok when ANY of its passes answered (first non-empty note wins).
-}
dedupSources :: [SourceAnswer] -> [SourceAnswer]
dedupSources = foldl mergeIn []
  where
    mergeIn acc a = case break ((== saSource a) . saSource) acc of
        (pre, x : post) -> pre ++ [combine x a] ++ post
        _ -> acc ++ [a]
    combine x a =
        x
            { saOk = saOk x || saOk a
            , saNote = if T.null (saNote x) then saNote a else saNote x
            }

interpretedJson :: Interpreted -> Value
interpretedJson interp =
    object $
        ["shape" .= iShape interp]
            <> [ "resolved" .= resolvedText
               | iName interp /= iRaw interp || isJust (iScope interp)
               ]
            <> ["note" .= iNote interp | not (T.null (iNote interp))]
  where
    resolvedText =
        iName interp <> maybe "" (" in " <>) (iScope interp)

consultedJson :: SourceAnswer -> Value
consultedJson a =
    object $
        [ "source" .= saSource a
        , "status" .= (if saOk a then "ok" else "unavailable" :: Text)
        ]
            <> ["note" .= saNote a | not (T.null (saNote a))]

hackageJson :: HackageInfo -> Value
hackageJson hk =
    object $
        [ "source" .= ("hackage" :: Text)
        , "status" .= (if hiAvailable hk then "ok" else "unavailable" :: Text)
        ]
            <> [ "note" .= ("names cache missing — run make search-cache" :: Text)
               | not (hiAvailable hk)
               ]
