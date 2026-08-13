{-# LANGUAGE OverloadedStrings #-}

{- |
Technique: scratch-cell check runner [Gating/Repair].
Guarantee: a verdict is read only from the channel that computed it: the executed cell's output.
Entry: 'runMarkerWith'. Bracket-shaped cell lifetime: insert, read GRADE_PASS/GRADE_FAIL, always delete.
-}
module Siza.Agent.Check.Marker (
    MarkerRun (..),
    markerSrc,
    markerOutput,
    runMarkerWith,
    executedOutput,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (ToolOk), toolOutcomeValue)
import Siza.Agent.Render (renderOutcome, withInsertDefaults)

-- | What a marker run produced: either nothing at all, or what the cell printed.
data MarkerRun
    = -- | the rendered rejection, for the caller to report
      MarkerRefused Text
    | -- | what the executed cell printed
      MarkerRan Text
    deriving (Eq, Show)

markerSrc :: Text -> Text
markerSrc check =
    "putStrLn (if (" <> check <> ") then \"GRADE_PASS\" else \"GRADE_FAIL\")"

-- | The text a run offers a classifier. A refusal offers none.
markerOutput :: MarkerRun -> Text
markerOutput (MarkerRefused _) = ""
markerOutput (MarkerRan out) = out

{- | Run a marker in a scratch cell and delete it again. The cell id comes
from the insert that created it: taking the notebook's highest id instead
deletes the caller's newest cell whenever the scratch cell is refused.
-}
runMarkerWith ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) -> Text -> IO MarkerRun
runMarkerWith call src = do
    ins <- call InsertCell (withInsertDefaults (object ["source" .= src]))
    case committedCellId ins of
        Nothing -> pure (MarkerRefused (renderOutcome ins))
        Just cid -> do
            out <- call ExecuteCell (object ["cell_id" .= cid])
            _ <- call DeleteCell (object ["cell_id" .= cid])
            pure (MarkerRan (executedOutput out))

-- | The cell an insert actually created, or nothing when it was refused.
committedCellId :: Either Text ToolOutcome -> Maybe Int
committedCellId (Right (ToolOk (Object o))) = case KM.lookup "cellId" o of
    Just (Number n) -> Just (round n)
    _ -> Nothing
committedCellId _ = Nothing

{- | What the executed cell printed, read from the fields that carry cell
output and from nowhere else — never from the payload as a whole, which may
echo the submitted source.
-}
executedOutput :: Either Text ToolOutcome -> Text
executedOutput (Right o) = fromValue (toolOutcomeValue o)
executedOutput _ = ""

fromValue :: Value -> Text
fromValue (String s) = s
fromValue (Object o) = case KM.lookup "outputs" o of
    Just (Array a) -> T.intercalate "\n" (concatMap outputText (toList a))
    _ -> firstField ["result", "stdout"] o
fromValue _ = ""

outputText :: Value -> [Text]
outputText (Object o) = [s | Just (String s) <- [KM.lookup "oiOutput" o]]
outputText _ = []

firstField :: [Text] -> KM.KeyMap Value -> Text
firstField ks o =
    case [s | k <- ks, Just (String s) <- [KM.lookup (K.fromText k) o]] of
        (s : _) -> s
        [] -> ""
