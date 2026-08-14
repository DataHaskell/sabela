{- |
Technique: the verify tool [Gating/Repair].
Guarantee: exposes the vet+marker pipeline as a tool via the Call (ToolName-based) convention.
Entry: 'runVerifyCall'. Next: Siza.Agent.Check.
-}
module Siza.Agent.VerifyTool (
    Call,
    runVerifyCall,
    verifyToolName,
    verifyDescription,
    verifyProperties,
    verifyRequired,
    verdictText,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (
    CheckResult (..),
    checkVerdict3With,
    degenerateCheck,
    degenerateNote,
 )
import Siza.Agent.Check.Vet (
    notebookBindingNames,
    scopeForCheck,
    vetVerdictAgainst,
 )

type Call = ToolName -> Value -> IO (Either Text ToolOutcome)

verifyToolName :: Text
verifyToolName = "verify"

verifyDescription :: Text
verifyDescription =
    "Check a claim about work THIS TASK committed, against the live kernel. \
    \Pass one boolean Haskell expression over bindings your own committed \
    \cells define (`total == 42 && length rows == 3`). Runs it in a scratch \
    \cell that is deleted afterwards, and answers pass | fail | uncheckable \
    \| not_applicable. A failing check comes back with the conjunct that \
    \failed and the value your code actually computed. A check over names no \
    \cell of yours defines is uncheckable: insert_cell first, then verify. \
    \Use it to confirm work is done rather than asserting it."

verifyProperties :: [(Text, Value)]
verifyProperties =
    [
        ( "check"
        , object
            [ "type" .= ("string" :: Text)
            , "description"
                .= ( "A boolean Haskell expression over bindings this task's \
                     \committed cells define." ::
                        Text
                   )
            ]
        )
    ]

verifyRequired :: [Text]
verifyRequired = ["check"]

{- | The check is vetted before it is trusted: one that does not compile, names
nothing the notebook defines, or passes under perturbation proves nothing.

The caller names the check, so the notebook's own bindings are the scope it is
read against — explicitly, and empty is a refusal rather than a silent pass.
-}
runVerifyCall :: Call -> Text -> IO ToolOutcome
runVerifyCall call check
    | T.null (T.strip check) =
        pure (notApplicable check "no check was given")
    | degenerateCheck check =
        pure (notApplicable check degenerateNote)
    | otherwise = do
        scope <- scopeForCheck call check =<< notebookBindingNames call
        vetted <- vetVerdictAgainst call scope check
        case vetted of
            Left why -> pure (discarded check why)
            Right _ -> report check =<< checkVerdict3With call check

{- | Only a check that ran and passed is a green result. A verdict the tool
could not reach is an error, not a quiet ok.
-}
report :: Text -> (CheckResult, Maybe Text) -> IO ToolOutcome
report check (result, mDetail) =
    pure $ case result of
        CheckPassed -> ToolOk payload
        _ -> ToolErr payload
  where
    payload =
        object
            ( ["verdict" .= verdictText result, "check" .= check]
                <> [detailKey .= d | Just d <- [mDetail]]
            )
    detailKey
        | result == CheckFailed = "counterexample"
        | result == CheckPassed = "ran"
        | otherwise = "note"

verdictText :: CheckResult -> Text
verdictText CheckPassed = "pass"
verdictText CheckFailed = "fail"
verdictText CheckUncheckable = "uncheckable"
verdictText CheckNotApplicable = "not_applicable"

notApplicable :: Text -> Text -> ToolOutcome
notApplicable check why =
    ToolErr
        ( object
            [ "verdict" .= verdictText CheckNotApplicable
            , "check" .= check
            , "note" .= why
            ]
        )

discarded :: Text -> Text -> ToolOutcome
discarded check why =
    ToolErr
        ( object
            [ "verdict" .= verdictText CheckUncheckable
            , "check" .= check
            , "discarded" .= why
            ]
        )
