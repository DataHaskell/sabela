{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateFrontier (
    Frontier (..),
    Step (..),
    startFrontier,
    stepFrontier,
    frontierPairs,
    repairedSourceKey,
    disposableDiagnostic,
    disposableExecution,
    probeBudget,
    probeDeadlineUs,
    repairRounds,
) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Health (Health, healthOfResult, improvesHealthFor)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Errors (parseErrors)
import Sabela.Parse (cellNames)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
 )

{- | How far repair has got: the best source proven so far, what was applied
to reach it, and the diagnostic that remains. It starts as the model's own
submission and only ever moves to a candidate that cleared something.
-}
data Frontier = Frontier
    { frontierSubmitted :: Text
    , frontierSrc :: Text
    , frontierFixes :: [Text]
    , frontierResult :: DisposableResult
    }

data Step = Commit Frontier | Advance Frontier | Skip

{- | Starts from what the caller submitted and what the harness normalised it
to. They are the same text on a path that rewrote nothing.
-}
startFrontier :: Text -> Text -> DisposableResult -> Frontier
startFrontier subm compiled = Frontier subm compiled []

{- | Green commits; a compile error may still advance the frontier, because a
fix that only uncovers the errors behind it has still made progress. A probe
that timed out or never ran proves nothing either way.
-}
stepFrontier :: Frontier -> (Text, [Text]) -> DisposableResult -> Step
stepFrontier frontier (candidate, fixes) result
    | verdict == DisposableOk = Commit advanced
    | verdict /= DisposableCompileError = Skip
    | improvesHealthFor defined (frontierHealth frontier) (frontierHealth advanced)
    , withinErrorClasses before after =
        Advance advanced
    | otherwise = Skip
  where
    verdict = disposableVerdict result
    before = disposableDiagnostic (frontierResult frontier)
    after = disposableDiagnostic result
    advanced =
        frontier
            { frontierSrc = candidate
            , frontierFixes = frontierFixes frontier <> fixes
            , frontierResult = result
            }
    defined = fst (cellNames (frontierSrc frontier))

frontierHealth :: Frontier -> Health
frontierHealth = healthOfResult . disposableExecution . frontierResult

{- | GHC error classes, coarse enough to be stable across wordings and fine
enough to tell progress from a trade.
-}
data ErrorClass = ClassModule | ClassParse | ClassScope | ClassType | ClassOther
    deriving (Eq, Ord, Show)

{- | A probe earns its round only when it did not buy the reduction with a new
kind of error. Clearing a missing module is the one exception: it stopped GHC
before it read the body, so the errors that then surface were always there.
-}
withinErrorClasses :: Text -> Text -> Bool
withinErrorClasses before after
    | ClassModule `elem` old, ClassModule `notElem` new = True
    | otherwise = all (`elem` old) new
  where
    old = errorClasses before
    new = errorClasses after

errorClasses :: Text -> [ErrorClass]
errorClasses diagnostic =
    nub [classOf chunk | chunk <- T.splitOn "\n\n" diagnostic, hasError chunk]
  where
    hasError chunk = not (T.null (T.strip chunk))
    classOf chunk
        | anyOf ["could not find module", "could not load module", "hidden package"] chunk =
            ClassModule
        | anyOf ["parse error", "lexical error"] chunk = ClassParse
        | anyOf ["not in scope", "ambiguous occurrence"] chunk = ClassScope
        | anyOf ["couldn't match", "no instance for", "ambiguous type"] chunk =
            ClassType
        | otherwise = ClassOther
    anyOf needles chunk = any (`T.isInfixOf` T.toLower chunk) needles

{- | The fixes a rejection carries, so the reader sees its remaining errors
against the source they were measured on. They are measured, not proven: each
one reduced the errors it was tried against, which is weaker than correct.
-}
frontierPairs :: Frontier -> [Pair]
frontierPairs frontier
    | null (frontierFixes frontier) = []
    | otherwise =
        [ "partialRepair"
            .= object
                [ "applied" .= frontierFixes frontier
                , "note" .= note
                ]
        ]
  where
    note :: Text
    note =
        "Nothing was committed. The fixes are in `"
            <> repairedSourceKey frontier
            <> "`, which is the text the compiler read, and the diagnostic is \
               \what remains after them. Each fix measurably reduced the errors, \
               \which is not the same as being right."

{- | The rejection field holding the source the fixes were applied to. A
rewrite is disclosed under `compiledSource`; with nothing to disclose there is
only the one source, and it is the caller's own.
-}
repairedSourceKey :: Frontier -> Text
repairedSourceKey frontier
    | frontierSrc frontier == frontierSubmitted frontier = "source"
    | otherwise = "compiledSource"

disposableDiagnostic :: DisposableResult -> Text
disposableDiagnostic result =
    maybe (disposableStderr result) failureMessage (disposableFailure result)

disposableExecution :: DisposableResult -> Either Text ExecutionResult
disposableExecution result
    | disposableVerdict result == DisposableOk =
        Right (ExecutionResult [] Nothing [] [])
    | otherwise = Right (ExecutionResult [] holistic errs [])
  where
    diagnostic = disposableDiagnostic result
    errs = parseErrors diagnostic
    holistic = if null errs then Just diagnostic else Nothing

-- | Disposable builds are the expensive part of a write; bound them.
probeBudget :: Int
probeBudget = 6

{- | A count bounds how many probes a search may spend, not how long they take.
This bounds the wall clock a probe search may start a new probe within, so a
notebook whose materialisation is slow costs at most one probe past it.
-}
probeDeadlineUs :: Int
probeDeadlineUs = 60 * 1000000

repairRounds :: Int
repairRounds = 3
