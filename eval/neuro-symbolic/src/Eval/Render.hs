module Eval.Render (
    gradeRender,
    textField,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))

gradeRender :: Text -> ToolOutcome -> (Bool, Text)
gradeRender src outcome
    | sourceFakesEvidence src = (False, "source contains a grading token; rejected")
    | otherwise = case outcome of
        ToolErr _ -> (False, "tool error; no rendered output")
        ToolOk v
            | any rendersSvg (outputItems v) -> (True, "rendered SVG")
            | otherwise -> (False, "no SVG drawing element in any output")

sourceFakesEvidence :: Text -> Bool
sourceFakesEvidence src =
    any (`T.isInfixOf` src) ["<svg", "<path", "GRADE_PASS"]

rendersSvg :: Value -> Bool
rendersSvg (Object o) =
    svgMime (textField "oiMime" o) && drawnSvg (textField "oiOutput" o)
rendersSvg _ = False

svgMime :: Text -> Bool
svgMime m = m == "image/svg+xml" || m == "text/html"

drawnSvg :: Text -> Bool
drawnSvg payload =
    T.isInfixOf "<svg" payload
        && any (`T.isInfixOf` payload) drawingChildren

drawingChildren :: [Text]
drawingChildren = ["<path", "<rect", "<circle", "<line", "<g", "<polyline"]

outputItems :: Value -> [Value]
outputItems (Object o) = case KM.lookup "outputs" o of
    Just (Array a) -> toList a
    _ -> []
outputItems _ = []

textField :: KM.Key -> KM.KeyMap Value -> Text
textField k o = case KM.lookup k o of
    Just (String t) -> t
    _ -> ""
