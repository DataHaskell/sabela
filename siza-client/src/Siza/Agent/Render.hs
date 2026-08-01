{- | Rendering a tool outcome for a text-shaped caller, and the write defaults
every insert carries. Kept below the catalogue so the check surface can reach
them without importing the tool router.
-}
module Siza.Agent.Render (
    renderOutcome,
    withInsertDefaults,
    errLabel,
) where

import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Siza.Agent.OutcomeDistill (distillOutcome)

withInsertDefaults :: Value -> Value
withInsertDefaults (Object o) =
    Object $
        def "cell_type" (String "CodeCell") $
            def "language" (String "Haskell") o
  where
    def k val m = if KM.member k m then m else KM.insert k val m
withInsertDefaults v = v

renderOutcome :: Either Text ToolOutcome -> Text
renderOutcome (Left e) = "transport error: " <> e
renderOutcome (Right (ToolOk v)) = trunc (enc (distillOutcome v))
renderOutcome (Right (ToolErr v)) =
    errLabel v <> ": " <> trunc (enc (distillOutcome v))

errLabel :: Value -> Text
errLabel (Object o)
    | Just (String t) <- KM.lookup "verdict" o
    , t == verdictTag VerdictDiagnostic =
        "CODE ISSUE"
    | Just (String t) <- KM.lookup "verdict" o
    , t == verdictTag VerdictCouldNotRun =
        "NOT RUN"
errLabel _ = "TOOL ERROR"

enc :: Value -> Text
enc = TE.decodeUtf8 . LBS.toStrict . encode

trunc :: Text -> Text
trunc t = if T.length t > 6000 then T.take 6000 t <> " …[truncated]" else t
