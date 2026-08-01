{-# LANGUAGE OverloadedStrings #-}

{- | What a scratchpad run reports. Kept apart from the session plumbing so
what the model is shown can be pinned against the run it came from.
-}
module Sabela.AI.Capabilities.Scratchpad.Payload (
    scratchpadPayload,
    scratchpadVerdict,
    withVerdict,
    silentDiagnostic,
    isolationDiagnostic,
    annotateChurn,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Diagnose (diagnoseWith, guidancePairs)
import Sabela.Errors (scrubHarnessFrames)

{- | The scratchpad result. The reported stderr is the compiler's own text with
the harness's frames dropped, and the guidance is classified against that same
text and against @code@ — the snippet submitted — so no advice contradicts
either. @compact@ is how the caller shortens a long stream for the wire.
-}
scratchpadPayload ::
    (Applicative m) => (Text -> m Value) -> Text -> Text -> Text -> m Value
scratchpadPayload compact code stdout stderr =
    build <$> compact stdout <*> compact reported
  where
    reported = scrubHarnessFrames stderr
    guidance = guidancePairs (diagnoseWith Nothing code reported)
    diagPair =
        ["diagnostic" .= d | Just d <- [silentDiagnostic stdout reported guidance]]
    build stdoutV stderrV =
        object
            ( [ "verdict" .= verdictTag (scratchpadVerdict stdout reported)
              , "stdout" .= stdoutV
              , "stderr" .= stderrV
              ]
                <> guidance
                <> diagPair
            )

scratchpadVerdict :: Text -> Text -> VerdictClass
scratchpadVerdict stdout stderr
    | not (T.null (T.strip stderr)) = VerdictDiagnostic
    | not (T.null (T.strip stdout)) = VerdictOk
    | otherwise = VerdictCouldNotRun

withVerdict :: VerdictClass -> Value -> Value
withVerdict c (Object o) =
    Object (KM.insert (Key.fromText "verdict") (String (verdictTag c)) o)
withVerdict c v = object ["verdict" .= verdictTag c, "result" .= v]

silentDiagnostic :: Text -> Text -> [a] -> Maybe Text
silentDiagnostic stdout stderr guidance
    | T.null (T.strip stdout) && T.null (T.strip stderr) && null guidance =
        Just isolationDiagnostic
    | otherwise = Nothing

isolationDiagnostic :: Text
isolationDiagnostic =
    "No output and no error. The scratchpad is ISOLATED from the notebook \
    \session: it cannot see notebook bindings, and packages need their own \
    \`-- cabal:` line inside the snippet. A pure binding prints nothing — \
    \`print` it here, or probe live notebook state with check_type / \
    \list_bindings, or just insert a cell instead."

{- | The repeated-failure note, over a count the turn's own counter holds. The
causes it lists are the common ones, not a reading of this snippet.
-}
annotateChurn :: Int -> Value -> Value
annotateChurn n (Object o) =
    Object $
        KM.insert
            (Key.fromText "_sabelaHint")
            ( String $
                "You have had "
                    <> T.pack (show n)
                    <> " consecutive failing scratchpad calls this turn. Common causes:"
                    <> " (a) top-level `let` (forbidden by scripths — write `x = 1` without `let`);"
                    <> " (b) ambiguous type defaults (pin with `:: Int` or `:: Double`);"
                    <> " (c) missing import. Before retrying, either ghci_query :type"
                    <> " the function to confirm its signature, or step back and explain to"
                    <> " the user what you're blocked on."
            )
            o
annotateChurn n other =
    object
        [ "scratchpadResult" .= other
        , "_sabelaHint"
            .= ( "Churning: "
                    <> T.pack (show n)
                    <> " consecutive scratchpad errors. Change approach or ask the user." ::
                    Text
               )
        ]
