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

dedupSources :: [SourceAnswer] -> [SourceAnswer]
dedupSources = foldl mergeIn []
  where
    mergeIn acc a = case break ((== saSource a) . saSource) acc of
        (pre, x : post) -> pre ++ [combine x a] ++ post
        _ -> acc ++ [a]
    combine x a =
        x
            { saOk = saOk x || saOk a
            , saAllOk = saAllOk x && saAllOk a
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

{- | How complete this source's answer is. A source consulted more than once
is only `ok` when every consultation succeeded, so a note recording a failure
can never sit beside an `ok`.
-}
sourceStatus :: SourceAnswer -> Text
sourceStatus a
    | not (saOk a) = "unavailable"
    | saAllOk a = "ok"
    | otherwise = "partial"

consultedJson :: SourceAnswer -> Value
consultedJson a =
    object $
        [ "source" .= saSource a
        , "status" .= sourceStatus a
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
