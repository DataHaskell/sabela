{- | @verify@: run a boolean claim about the notebook against the live kernel
and report a verdict.

Client-side, like @discover@: it composes insert/execute/delete through
'Siza.Agent.Check', so the server needs no new capability. The check is vetted
first — one that cannot fail is not evidence.
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
    checkScopeFor,
    notebookBindingNames,
    vetVerdictAgainst,
 )

type Call = ToolName -> Value -> IO (Either Text ToolOutcome)

verifyToolName :: Text
verifyToolName = "verify"

verifyDescription :: Text
verifyDescription =
    "Check a claim about the notebook against the live kernel. Pass one \
    \boolean Haskell expression over bindings the notebook already defines \
    \(`total == 42 && length rows == 3`). Runs it in a scratch cell that is \
    \deleted afterwards, and answers pass | fail | uncheckable | \
    \not_applicable. A failing check comes back with the conjunct that failed \
    \and the value your code actually computed. Use it to confirm work is \
    \done rather than asserting it."

verifyProperties :: [(Text, Value)]
verifyProperties =
    [
        ( "check"
        , object
            [ "type" .= ("string" :: Text)
            , "description"
                .= ( "A boolean Haskell expression over the notebook's own \
                     \bindings." ::
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
        scope <- checkScopeFor call =<< notebookBindingNames call
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
