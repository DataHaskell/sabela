{-# LANGUAGE OverloadedStrings #-}

{- | The typed cell outcome carving (R2-2 of @docs/siza-redesign.md@ §1.2).

The sole cell-result wire shape: the legacy @ran/ok/outputs/error/errors@
blob is gone. 'toCellResult' is the single pure mapper from the
@executeCell@ result (@Either Text ExecutionResult@) to the sum-typed
'CellResult'; 'okCellResult' replaces the hand-computed
@null errs && isNothing error@ predicate with @crOutcome == Succeeded@.
-}
module Sabela.AI.CellResult (
    Diagnostic,
    AbortReason (..),
    CellOutcome (..),
    CellResult (..),
    CellId,
    OwnedCells,
    okCellResult,
    notebookHealthy,
    toCellResult,
    toToolOutcome,
    mergeToolOk,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair, Parser)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (
    ExecutionResult (..),
    ToolOutcome (..),
 )
import Sabela.Model (CellError, OutputItem)

-- | A cell diagnostic (error or warning), carrying a cell-relative location.
type Diagnostic = CellError

{- | Why a cell never produced a result. Maps the three @executeCell@ @Left@
strings.
-}
data AbortReason = Interrupted | Superseded | TimedOut
    deriving (Eq, Show)

{- | The single outcome sum. Constructors are POSITIONAL, not records: a
record selector on 'Raised'/'Rejected'/'Aborted' would be partial. @ok@ is
@crOutcome == Succeeded@.
-}
data CellOutcome
    = Succeeded
    | Raised !Text
    | Rejected ![Diagnostic]
    | Aborted !AbortReason
    deriving (Eq, Show)

{- | Outcome plus the orthogonal outputs and warnings. @crWarnings@ carries the
non-fatal diagnostics GHC reports alongside a successful (or failing) run.
-}
data CellResult = CellResult
    { crOutcome :: CellOutcome
    , crOutputs :: [OutputItem]
    , crWarnings :: [Diagnostic]
    }
    deriving (Eq, Show)

-- | The typed restatement of the legacy @null errs && isNothing error@.
okCellResult :: CellResult -> Bool
okCellResult cr = crOutcome cr == Succeeded

-- | A notebook cell identifier, matching @cellId@ in "Sabela.Model".
type CellId = Int

-- | The ids of cells an eval episode wrote, so the harness knows what it owns.
type OwnedCells = Set CellId

{- | Did every cell of this run succeed? The empty list is healthy: nothing
red. Lets the harness decide when an episode is finished.
-}
notebookHealthy :: [CellResult] -> Bool
notebookHealthy = all okCellResult

{- | Map an @executeCell@ result to a 'CellResult'. The three @Left@ strings
become 'Aborted' reasons; a @Right@ with @erError@ set is 'Raised', a
non-empty @erErrors@ is 'Rejected', otherwise 'Succeeded'.
-}
toCellResult :: Either Text ExecutionResult -> [OutputItem] -> CellResult
toCellResult res outputs =
    CellResult (outcomeOf res) outputs (warningsOf res)
  where
    outcomeOf (Left msg) = Aborted (abortReason msg)
    outcomeOf (Right er)
        | Just e <- erError er = Raised e
        | not (null (erErrors er)) = Rejected (erErrors er)
        | otherwise = Succeeded
    warningsOf (Left _) = []
    warningsOf (Right er) = erWarnings er

-- | The three @executeCell@ @Left@ strings, matched on the stable prefix.
abortReason :: Text -> AbortReason
abortReason msg
    | msg == "Cancelled" = Interrupted
    | "Request superseded" `T.isPrefixOf` msg = Superseded
    | "Cell execution timed out" `T.isPrefixOf` msg = TimedOut
    | otherwise = Interrupted

------------------------------------------------------------------------
-- Tool-outcome envelope (§1.8)
------------------------------------------------------------------------

{- | The one total map from a cell result to the unchanged 'ToolOutcome'
envelope. EVERY 'CellResult' — including 'Rejected' (compile error),
'Raised', and 'Aborted' — is a *successful tool call reporting a cell
outcome*, so it is 'ToolOk' (@is_error: false@). Tool-mechanics failures
(bad args, Busy, unknown tool) are not 'CellResult's; they stay 'ToolErr'
in the dispatcher. Mapping a compile error to @is_error: true@ would revive
the backwards-boolean bug the envelope was built to kill.
-}
toToolOutcome :: CellResult -> ToolOutcome
toToolOutcome = ToolOk . toJSON

{- | 'toToolOutcome' with extra wire keys (e.g. @cellId@) merged into the
'ToolOk' object alongside the typed @outcome/ok/...@ fields. The typed keys
win on collision. A non-object 'CellResult' encoding is impossible, so the
fallback just keeps the typed value.
-}
mergeToolOk :: CellResult -> [Pair] -> ToolOutcome
mergeToolOk cr extra = case toToolOutcome cr of
    ToolOk (Object o) -> ToolOk (Object (KM.union o (KM.fromList extra)))
    other -> other

------------------------------------------------------------------------
-- Wire shape
------------------------------------------------------------------------

instance ToJSON AbortReason where
    toJSON Interrupted = "Interrupted"
    toJSON Superseded = "Superseded"
    toJSON TimedOut = "TimedOut"

instance FromJSON AbortReason where
    parseJSON = withText "AbortReason" $ \t -> case t of
        "Interrupted" -> pure Interrupted
        "Superseded" -> pure Superseded
        "TimedOut" -> pure TimedOut
        _ -> fail ("unknown abort reason: " <> show t)

instance ToJSON CellOutcome where
    toJSON Succeeded = object ["tag" .= ("Succeeded" :: Text)]
    toJSON (Raised msg) =
        object ["tag" .= ("Raised" :: Text), "message" .= msg]
    toJSON (Rejected ds) =
        object ["tag" .= ("Rejected" :: Text), "errors" .= ds]
    toJSON (Aborted r) =
        object ["tag" .= ("Aborted" :: Text), "reason" .= r]

instance FromJSON CellOutcome where
    parseJSON = withObject "CellOutcome" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "Succeeded" -> pure Succeeded
            "Raised" -> Raised <$> o .: "message"
            "Rejected" -> Rejected . fromMaybe [] <$> o .:? "errors"
            "Aborted" -> Aborted <$> o .: "reason"
            _ -> fail ("unknown outcome tag: " <> show tag)

instance ToJSON CellResult where
    toJSON cr =
        object
            [ "outcome" .= crOutcome cr
            , "outputs" .= crOutputs cr
            , "warnings" .= crWarnings cr
            , "ok" .= okCellResult cr
            ]

instance FromJSON CellResult where
    parseJSON = withObject "CellResult" $ \o -> do
        outcome <- o .: "outcome"
        outputs <- o .: "outputs"
        warnings <- o .: "warnings"
        pure (CellResult outcome outputs warnings)
