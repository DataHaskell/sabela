{-# LANGUAGE OverloadedStrings #-}

{- | Tool descriptions that more than one catalogue offers. One definition,
so the text the model routes on cannot drift apart from surface to surface,
or from the capability it describes.
-}
module Sabela.AI.ToolDoc (
    readFileDescription,
    readFilePathArg,
    tryDescription,
    tierEvaluatePure,
    tierTypecheckOnly,
    trialTierNames,
    trialRefusalGrammar,
    refusalEmpty,
    refusalMultipleExpressions,
    refusalExpressionNotFinal,
    refusalMetaCommand,
    refusalMetaQuery,
    refusalUnsafeSyntax,
    checkTypeImportsDoc,
) where

import Data.Text (Text)
import qualified Data.Text as T

{- | Names the three @view@ tags a read can return, so the description and
the result speak the same words. Every clause is a field the work-dir read
actually emits: @bytes@, @view@, @shape@, @strings@ and @sampled@.
-}
readFileDescription :: Text
readFileDescription =
    "Describe a file, whatever is in it. With no `repo`, reads from the \
    \notebook's work directory and returns the file's size in bytes plus one \
    \`view` chosen from the bytes themselves: `text` (the contents), \
    \`delimited` (the delimiter, the column names when a header row was read, \
    \a type per column, and the first rows), or `binary` (the printable \
    \strings found in it) when the bytes do not read as text. Read a file \
    \before writing a cell against it, so the names in the cell are names you have \
    \seen. A file larger than the sample is read at both ends, and the result \
    \says which bytes it read. With `repo` set to \"owner/name\", reads that \
    \file's text out of the GitHub repository instead."

{- | States where a path is resolved from and nothing else. An example naming
a real file would be a name the model had not read, offered on the surface
that is supposed to stop it guessing one.
-}
readFilePathArg :: Text
readFilePathArg =
    "Path to the file, relative to the notebook's work directory, or within \
    \the repository when `repo` is set."

{- | The two tiers a trial can be answered at, in the words the result's @tier@
field carries. The capability derives that field from these, so a tier the
description does not name cannot be emitted.
-}
tierEvaluatePure :: Text
tierEvaluatePure = "evaluate_pure"

tierTypecheckOnly :: Text
tierTypecheckOnly = "typecheck_only"

trialTierNames :: [Text]
trialTierNames = [tierEvaluatePure, tierTypecheckOnly]

{- | One clause per class of source a trial refuses, in the planner's own
words. The planner's refusal text is built from these, so a class the surface
does not state cannot be returned.
-}
trialRefusalGrammar :: [Text]
trialRefusalGrammar =
    [ refusalEmpty
    , refusalMultipleExpressions
    , refusalExpressionNotFinal
    , refusalMetaCommand
    , refusalMetaQuery
    , refusalUnsafeSyntax
    ]

refusalEmpty :: Text
refusalEmpty = "code required"

refusalMultipleExpressions :: Text
refusalMultipleExpressions = "cannot follow more than one final expression"

refusalExpressionNotFinal :: Text
refusalExpressionNotFinal = "a trial's expression must come last"

refusalMetaCommand :: Text
refusalMetaCommand =
    "meta-commands that can touch the filesystem, the loaded set or process \
    \state are not admitted"

refusalMetaQuery :: Text
refusalMetaQuery = "only reads the type environment, so check_type answers it"

refusalUnsafeSyntax :: Text
refusalUnsafeSyntax = "cannot own a compile-time or purity escape"

{- | The modules a type question is asked in. Named the same on every surface
that offers the argument, so a call written for one dispatches on the others.
-}
checkTypeImportsDoc :: Text
checkTypeImportsDoc =
    "Optional. Modules to have in scope for this question, e.g. \
    \[\"Data.Map\", \"Data.Text\"]. The answer is given in exactly these \
    \modules' scope, in a session of its own; the notebook's own scope is not \
    \changed. Omit to ask the live session."

{- | Names the tier vocabulary and the refusal grammar the capability actually
computes, so the promise and the payload cannot drift apart. Nothing a
candidate would perform is performed at either tier.
-}
tryDescription :: Text
tryDescription =
    "Find out whether code WILL work before writing it: whether a dependency \
    \resolves, whether an import exists, whether an expression type-checks. \
    \Declare a candidate-only dependency with a `-- cabal: build-depends:` \
    \line and this answers whether it can be installed at all, without \
    \restarting the kernel or touching the notebook. Sees the notebook's live \
    \bindings. Where the run settles it, the result names the tier it was \
    \answered at beside the `executed` fact that tier is read from: `"
        <> tierEvaluatePure
        <> "`, the candidate's expression was evaluated, or `"
        <> tierTypecheckOnly
        <> "`, the candidate was put to the compiler and not performed — \
           \nothing it would read, write or delete happened, whatever the \
           \code says. A candidate the live session cannot answer is answered \
           \in a disposable one, which re-runs the notebook's own settled \
           \cells to establish its context and lists them under \
           \`replayedCells`. The notebook and its cells are unchanged either \
           \way; if a trial disturbed the kernel the result says so under \
           \`recovery`. It refuses only for these reasons: "
        <> T.intercalate "; " trialRefusalGrammar
        <> "."
