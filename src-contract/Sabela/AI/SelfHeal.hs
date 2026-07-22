{-# LANGUAGE OverloadedStrings #-}

{- | Loud + conservative self-heal helpers: name what a repair changed, decline
a lexically distant rename, and contrast a wrong name with the real candidates
— a silent or over-eager repair confuses the model more than the error did.
-}
module Sabela.AI.SelfHeal (
    attachSelfHeal,
    contrastLine,
    plausibleRename,
    selfHealNote,
    sourceDelta,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleRepair (
    editDistance,
    goalFromError,
    orderBySimilarity,
    suggestedNames,
 )

{- | Accept a rename only when the new name is lexically close to the wrong
one: within 2 edits, or half the wrong name's length for longer names. A
distant constant (@customers@ → @mempty@) substitutes MEANING, not spelling —
it can compile green with a wrong value, which is worse than staying red.
-}
plausibleRename :: Text -> Text -> Bool
plausibleRename wrong chose =
    editDistance wrong chose <= max 2 (T.length wrong `div` 2)

{- | The wrong-vs-real contrast for an error whose diagnostic carries a
did-you-mean: names the phantom, lists the real candidates nearest-first, and
directs the model away from re-submitting the phantom.
-}
contrastLine :: Text -> Maybe Text
contrastLine raw
    | cascadeNoise err = Nothing
    | otherwise = do
        (wrong, _) <- goalFromError err
        case orderBySimilarity wrong (nub (suggestedNames err)) of
            [] -> Nothing
            cands ->
                Just $
                    "`"
                        <> wrong
                        <> "` does not exist here. Closest real names: "
                        <> T.intercalate ", " (map tick (take 3 cands))
                        <> ". Use one of these — verify its exact signature with check_type first — and do not write `"
                        <> wrong
                        <> "` again."
  where
    err = unescapeDiag raw
    tick n = "`" <> n <> "`"
    -- A module-load/hidden-package cascade: its not-in-scope names are
    -- collateral, not phantoms — telling the model to avoid them is harmful.
    cascadeNoise t =
        any (`T.isInfixOf` t) ["Could not load module", "hidden package"]

-- | Diagnostics can arrive JSON-escaped on client rails; undo the common escapes.
unescapeDiag :: Text -> Text
unescapeDiag =
    T.replace "\\\"" "\"" . T.replace "\\n" "\n" . T.replace "\\t" "\t"

-- | Line-level diff between two sources: (removed lines, added lines).
sourceDelta :: Text -> Text -> ([Text], [Text])
sourceDelta old new =
    ( [l | l <- oldLs, l `notElem` newLs]
    , [l | l <- newLs, l `notElem` oldLs]
    )
  where
    oldLs = T.lines old
    newLs = T.lines new

{- | The model-facing note for a kept repair: 'Nothing' unless the line diff
is nonempty (R7.1: a note exists iff the stated edit changed the source), else
the diff plus the full post-heal source, so "build on the CURRENT source"
always carries the source it refers to.
-}
selfHealNote :: Text -> Text -> Maybe Value
selfHealNote before after
    | null removed && null added = Nothing
    | otherwise =
        Just $
            object
                [ "note"
                    .= ( "self_heal rewrote this cell before it ran. Build on the CURRENT source in the `source` field; do not re-submit your original code." ::
                            Text
                       )
                , "removed" .= toJSON removed
                , "added" .= toJSON added
                , "source" .= after
                ]
  where
    (removed, added) = sourceDelta before after

-- | Attach a self-heal note to a cell-result object; identity when 'Nothing'.
attachSelfHeal :: Maybe Value -> Value -> Value
attachSelfHeal (Just note) (Object o) =
    Object (KM.insert "self_heal" note o)
attachSelfHeal _ v = v
