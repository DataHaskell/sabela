{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.SelfHeal (
    attachSelfHeal,
    attachSelfHealSuggestions,
    contrastLine,
    dependencySuggestionNote,
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

plausibleRename :: Text -> Text -> Bool
plausibleRename wrong chose =
    editDistance wrong chose <= max 2 (T.length wrong `div` 2)

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
    cascadeNoise t =
        any (`T.isInfixOf` t) ["Could not load module", "hidden package"]

unescapeDiag :: Text -> Text
unescapeDiag =
    T.replace "\\\"" "\"" . T.replace "\\n" "\n" . T.replace "\\t" "\t"

sourceDelta :: Text -> Text -> ([Text], [Text])
sourceDelta old new =
    ( [l | l <- oldLs, l `notElem` newLs]
    , [l | l <- newLs, l `notElem` oldLs]
    )
  where
    oldLs = T.lines old
    newLs = T.lines new

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

attachSelfHeal :: Maybe Value -> Value -> Value
attachSelfHeal (Just note) (Object o) =
    Object (KM.insert "self_heal" note o)
attachSelfHeal _ v = v

dependencySuggestionNote :: [Text] -> Text -> Value
dependencySuggestionNote pkgs src =
    object
        [ "packages" .= toJSON pkgs
        , "source" .= src
        , "note"
            .= ( "This would compile once `-- cabal: build-depends: "
                    <> deps
                    <> "` is added, but self_heal never adds a dependency "
                    <> "automatically. Add that line yourself (replace_cell_source) "
                    <> "if you want it."
               )
        ]
  where
    deps = T.intercalate ", " pkgs

attachSelfHealSuggestions :: [Value] -> Value -> Value
attachSelfHealSuggestions [] v = v
attachSelfHealSuggestions sugs (Object o) =
    Object (KM.insert "self_heal_suggestions" (toJSON sugs) o)
attachSelfHealSuggestions _ v = v
