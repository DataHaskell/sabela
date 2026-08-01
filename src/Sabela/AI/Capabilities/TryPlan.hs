{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.TryPlan (
    TrialPlan (..),
    TrialPlanError (..),
    TrialTier (..),
    MetaQuery (..),
    planTrial,
    candidateNeedsDisposable,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Parser (
    CabalMeta (..),
    Line (..),
    ScriptFile (..),
    parseScript,
 )
import ScriptHs.Render (
    Kind (..),
    Piece (..),
    lineText,
    toGhciScript,
    toPieces,
 )

{- | Which of the two questions a candidate can be put to. A pure candidate can
be answered by inspecting it in the live session; a contained one has an effect
or a purity escape and may only run in a disposable session.
-}
data TrialTier = TierPure | TierContained
    deriving (Bounded, Enum, Eq, Show)

{- | A GHCi meta-command that only interrogates the type environment, with the
imports the same candidate declared. It has no effect, so it is a question for
the tool that answers types rather than a refusal.
-}
data MetaQuery = MetaQuery
    { mqCommand :: Text
    , mqSubject :: Text
    , mqImports :: [Text]
    }
    deriving (Eq, Show)

data TrialPlan = TrialPlan
    { trialMeta :: CabalMeta
    , trialSetup :: Text
    , trialExpression :: Maybe Text
    , trialSource :: Text
    , trialTier :: TrialTier
    }
    deriving (Eq, Show)

data TrialPlanError
    = TrialEmpty
    | TrialMultipleExpressions
    | TrialExpressionNotFinal
    | TrialMetaCommand Text
    | TrialMetaQuery MetaQuery
    | TrialUnsafeSyntax Text
    deriving (Eq, Show)

planTrial :: Text -> Either TrialPlanError TrialPlan
planTrial raw
    | T.null (T.strip raw) = Left TrialEmpty
    | Just hazard <- firstHazard raw meta = Left (TrialUnsafeSyntax hazard)
    | otherwise = do
        (setupLines, actions, boundEffect) <-
            foldPieces importLines (toPieces (scriptLines parsed))
        expression <- case actions of
            [] -> Right Nothing
            [one] -> Right (Just one)
            _ -> Left TrialMultipleExpressions
        pure
            TrialPlan
                { trialMeta = meta
                , trialSetup = toGhciScript setupLines
                , trialExpression = expression
                , trialSource = raw
                , trialTier =
                    if boundEffect || hasPurityEscape raw
                        then TierContained
                        else TierPure
                }
  where
    parsed = parseScript raw
    meta = scriptMeta parsed
    importLines = [t | Import t <- scriptLines parsed]

{- | A contained candidate never takes the live fast path: the live session is
the notebook's own, and only the disposable one is discardable.
-}
candidateNeedsDisposable :: TrialPlan -> Bool
candidateNeedsDisposable plan =
    trialTier plan == TierContained
        || not (metaIsEmpty (trialMeta plan))
        || not (T.null (T.strip (trialSetup plan)))
        || maybe True (T.any (`elem` ['\n', '\r'])) (trialExpression plan)

foldPieces :: [Text] -> [Piece] -> Either TrialPlanError ([Line], [Text], Bool)
foldPieces imports = go [] [] False False
  where
    go setup actions _ effect [] = Right (reverse setup, reverse actions, effect)
    go setup actions seenAction effect (piece : rest) = case piece of
        PBlank -> go (Blank : setup) actions seenAction effect rest
        PPragma p
            | seenAction -> Left TrialExpressionNotFinal
            | otherwise -> go (Pragma p : setup) actions False effect rest
        PImport i
            | seenAction -> Left TrialExpressionNotFinal
            | otherwise -> go (Import i : setup) actions False effect rest
        PUnit KComment ls -> go (reverse ls <> setup) actions seenAction effect rest
        PUnit KDeclaration ls
            | seenAction -> Left TrialExpressionNotFinal
            | otherwise -> go (reverse ls <> setup) actions False effect rest
        PUnit KAction ls ->
            go setup (renderUnit ls : actions) True effect rest
        PUnit KIOBind ls
            | seenAction -> Left TrialExpressionNotFinal
            | otherwise -> go (reverse ls <> setup) actions False True rest
        PUnit KTHSplice ls ->
            Left (TrialUnsafeSyntax ("Template Haskell splice: " <> renderUnit ls))
        PGhciCommand cmd -> case metaQueryOf cmd of
            Just (command, subject) ->
                Left (TrialMetaQuery (MetaQuery command subject imports))
            Nothing -> Left (TrialMetaCommand cmd)
    renderUnit = T.stripEnd . T.unlines . map lineText

{- | A meta-command that only interrogates the type environment, split into the
command (with its flags) and the subject it asks about. A command this does not
recognise is treated as effectful: an unknown effect is an effect.
-}
metaQueryOf :: Text -> Maybe (Text, Text)
metaQueryOf raw = case T.words (T.strip raw) of
    (cmd : rest)
        | T.toLower cmd `elem` pureQueryCommands ->
            let (flags, subject) = span ("+" `T.isPrefixOf`) rest
             in Just (T.unwords (cmd : flags), T.unwords subject)
    _ -> Nothing

pureQueryCommands :: [Text]
pureQueryCommands =
    [":type", ":t", ":kind", ":kind!", ":k", ":k!", ":info", ":i", ":doc"]

firstHazard :: Text -> CabalMeta -> Maybe Text
firstHazard raw meta
    | not (null (metaGhcOptions meta)) =
        Just "candidate ghc-options are not admitted by try"
    | not (null (metaUnknownKeys meta)) =
        Just "unknown candidate Cabal directives are not admitted by try"
    | not (null (metaSourceRepos meta)) =
        Just "candidate source repositories require a qualified build sandbox"
    | not (null (metaExtraLibDirs meta) && null (metaExtraIncludeDirs meta)) =
        Just "candidate include/library directories require a qualified build sandbox"
    | path : _ <- metaPackages meta =
        Just
            ( "candidate local packages require a qualified build sandbox: "
                <> path
            )
    | Just extension <- firstForbiddenExtension (metaExts meta) =
        Just ("candidate extension is not admitted by try: " <> extension)
    | "language unsafe" `T.isInfixOf` normalized
        || "language trustworthy" `T.isInfixOf` normalized
        || "language nosafe" `T.isInfixOf` normalized =
        Just "candidate cannot override the scratch safety mode"
    | hasSpliceSyntax raw || hasQuasiquoteSyntax raw =
        Just "candidate contains a Template Haskell splice or quasiquote"
    | "{-#ann" `T.isInfixOf` compact =
        Just "candidate contains a compile-time annotation"
    | "foreign" `T.isInfixOf` lowered =
        Just "candidate contains an FFI declaration"
    | any (`T.isInfixOf` lowered) harnessEscapes =
        Just "candidate contains an unsafe compile-time escape"
    | otherwise = Nothing
  where
    lowered = T.toLower raw
    normalized = T.unwords (T.words lowered)
    compact = T.filter (`notElem` [' ', '\t', '\r', '\n']) lowered

{- | Escapes that would let a candidate reach past the harness itself: the
harness's own generated-name prefix, and compile flags. Containment cannot help
with either, so they stay refused.
-}
harnessEscapes :: [Text]
harnessEscapes = ["_sabela", "options_ghc"]

{- | Escapes from the type system's purity guarantee. They say nothing about
whether the code can be checked — only that the live session must not host it,
because its assurance there is the candidate's type.
-}
hasPurityEscape :: Text -> Bool
hasPurityEscape raw = any (`T.isInfixOf` T.toLower raw) escapes
  where
    escapes =
        [ "unsafeperformio"
        , "unsafedupableperformio"
        , "unsafeinterleaveio"
        , "unsafecoerce"
        , "system.io.unsafe"
        , "ghc.io.unsafe"
        , "unsafe.coerce"
        , "runio"
        , "qrunio"
        ]

hasSpliceSyntax :: Text -> Bool
hasSpliceSyntax source =
    any startsSplice (T.tails source)
  where
    startsSplice tailText = case T.unpack (T.take 2 tailText) of
        ['$', next] -> next == '$' || next == '(' || isIdentifierStart next
        _ -> False
    isIdentifierStart c =
        isAsciiLower c
            || isAsciiUpper c
            || c == '_'

hasQuasiquoteSyntax :: Text -> Bool
hasQuasiquoteSyntax source =
    any startsQuote (T.tails source)
  where
    startsQuote tailText = case T.uncons tailText of
        Just ('[', rest) ->
            let (name, suffix) = T.span isQuoterChar rest
             in (not (T.null name) || "|" `T.isPrefixOf` suffix)
                    && "|" `T.isPrefixOf` suffix
        _ -> False
    isQuoterChar c =
        isAsciiLower c
            || isAsciiUpper c
            || isDigit c
            || c `elem` ("_.'" :: String)

firstForbiddenExtension :: [Text] -> Maybe Text
firstForbiddenExtension = firstMatch . map (T.toLower . T.strip)
  where
    firstMatch [] = Nothing
    firstMatch (extension : rest)
        | extension `elem` forbiddenExtensions = Just extension
        | otherwise = firstMatch rest
    forbiddenExtensions =
        [ "unsafe"
        , "nosafe"
        , "trustworthy"
        , "templatehaskell"
        , "templatehaskellquotes"
        , "quasiquotes"
        , "foreignfunctioninterface"
        , "cpp"
        ]

metaIsEmpty :: CabalMeta -> Bool
metaIsEmpty meta =
    null (metaDeps meta)
        && null (metaExts meta)
        && null (metaGhcOptions meta)
        && null (metaExtraLibDirs meta)
        && null (metaExtraIncludeDirs meta)
        && null (metaPackages meta)
        && null (metaSourceRepos meta)
        && null (metaUnknownKeys meta)
