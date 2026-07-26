{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.TryPlan (
    TrialPlan (..),
    TrialPlanError (..),
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

data TrialPlan = TrialPlan
    { trialMeta :: CabalMeta
    , trialSetup :: Text
    , trialExpression :: Maybe Text
    , trialSource :: Text
    }
    deriving (Eq, Show)

data TrialPlanError
    = TrialEmpty
    | TrialMultipleExpressions
    | TrialExpressionNotFinal
    | TrialEffectfulStatement Text
    | TrialMetaCommand Text
    | TrialUnsafeSyntax Text
    deriving (Eq, Show)

planTrial :: Text -> Either TrialPlanError TrialPlan
planTrial raw
    | T.null (T.strip raw) = Left TrialEmpty
    | Just hazard <- firstHazard raw meta = Left (TrialUnsafeSyntax hazard)
    | otherwise = do
        (setupLines, actions) <- foldPieces (toPieces (scriptLines parsed))
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
                }
  where
    parsed = parseScript raw
    meta = scriptMeta parsed

candidateNeedsDisposable :: TrialPlan -> Bool
candidateNeedsDisposable plan =
    not (metaIsEmpty (trialMeta plan))
        || not (T.null (T.strip (trialSetup plan)))
        || maybe True (T.any (`elem` ['\n', '\r'])) (trialExpression plan)

foldPieces :: [Piece] -> Either TrialPlanError ([Line], [Text])
foldPieces = go [] [] False
  where
    go setup actions _ [] = Right (reverse setup, reverse actions)
    go setup actions seenAction (piece : rest) = case piece of
        PBlank -> go (Blank : setup) actions seenAction rest
        PPragma p
            | seenAction -> Left TrialExpressionNotFinal
            | otherwise -> go (Pragma p : setup) actions False rest
        PImport i
            | seenAction -> Left TrialExpressionNotFinal
            | otherwise -> go (Import i : setup) actions False rest
        PUnit KComment ls -> go (reverse ls <> setup) actions seenAction rest
        PUnit KDeclaration ls
            | seenAction -> Left TrialExpressionNotFinal
            | otherwise -> go (reverse ls <> setup) actions False rest
        PUnit KAction ls ->
            go setup (renderUnit ls : actions) True rest
        PUnit KIOBind ls ->
            Left (TrialEffectfulStatement (renderUnit ls))
        PUnit KTHSplice ls ->
            Left (TrialUnsafeSyntax ("Template Haskell splice: " <> renderUnit ls))
        PGhciCommand cmd -> Left (TrialMetaCommand cmd)
    renderUnit = T.stripEnd . T.unlines . map lineText

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
    | any (`T.isInfixOf` lowered) forbidden =
        Just "candidate contains an unsafe compile-time or purity escape"
    | otherwise = Nothing
  where
    lowered = T.toLower raw
    normalized = T.unwords (T.words lowered)
    compact = T.filter (`notElem` [' ', '\t', '\r', '\n']) lowered
    forbidden =
        [ "_sabela"
        , "unsafeperformio"
        , "unsafedupableperformio"
        , "unsafeinterleaveio"
        , "unsafecoerce"
        , "system.io.unsafe"
        , "ghc.io.unsafe"
        , "unsafe.coerce"
        , "runio"
        , "qrunio"
        , "options_ghc"
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
