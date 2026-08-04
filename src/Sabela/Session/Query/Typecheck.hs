{-# LANGUAGE OverloadedStrings #-}

{- | Type-checking a cell without running it, restricted to the value subset
GHCi can decide with @:type@. Anything outside that subset (a data or class
declaration, an import) is declined rather than guessed at.
-}
module Sabela.Session.Query.Typecheck (
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckValueWith,
    typecheckLetDeclarations,
) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import Sabela.Session (Session)
import Sabela.Session.Query.Command (QueryCommand (..), runQueryCommand)

data TypecheckResult = TypecheckResult
    { tcSucceeded :: Bool
    , tcDiagnostics :: Text
    }
    deriving (Eq, Show)

data TypecheckInput = ValueExpression | ValueBindings | OutsideValueSubset
    deriving (Eq, Show)

classifyTypecheckInput :: Text -> TypecheckInput
classifyTypecheckInput source
    | null ls = ValueExpression
    | any outside ls = OutsideValueSubset
    | all isBinding ls = ValueBindings
    | any isBinding ls = OutsideValueSubset
    | otherwise = ValueExpression
  where
    ls = map stripLet (meaningfulLines source)
    outside line = any (`T.isPrefixOf` lower line) excluded
    excluded =
        [ "data "
        , "newtype "
        , "type "
        , "class "
        , "instance "
        , "import "
        , "foreign "
        , "default "
        , "infix"
        , "{-#"
        , "-- cabal:"
        ]
    lower = T.map toLower . T.stripStart
    isBinding line = case definitionLhs line of
        Just lhs -> not (T.null (T.strip lhs)) && T.all validLhs lhs
        Nothing -> False
    validLhs c = isAlphaNum c || isSpace c || c `elem` ("_'(),[]" :: String)

{- | The left-hand side up to a definition's @=@, or nothing when the line
defines nothing. A comparison is not a definition: @print (1 == 1)@ has no
left-hand side, and reading one there wraps an expression in @let { … }@.
-}
definitionLhs :: Text -> Maybe Text
definitionLhs = go ""
  where
    go acc t = case T.uncons t of
        Nothing -> Nothing
        Just ('=', rest)
            | continuesOperator rest || endsOperator acc ->
                go (T.snoc acc '=') rest
            | otherwise -> Just acc
        Just (c, rest) -> go (T.snoc acc c) rest
    continuesOperator rest = case T.uncons rest of
        Just (c, _) -> c `elem` ("=><" :: String)
        Nothing -> False
    endsOperator acc = case T.unsnoc acc of
        Just (_, c) -> c `elem` ("=/<>!" :: String)
        Nothing -> False

meaningfulLines :: Text -> [Text]
meaningfulLines = filter (not . T.null) . map T.strip . T.lines

stripLet :: Text -> Text
stripLet line = maybe line T.stripStart (T.stripPrefix "let " (T.stripStart line))

typecheckLetDeclarations :: Session -> Text -> IO TypecheckResult
typecheckLetDeclarations sess =
    typecheckValueWith
        (runQueryCommand sess . QueryType)
        (runQueryCommand sess QueryBindings)

typecheckValueWith :: (Text -> IO Text) -> IO Text -> Text -> IO TypecheckResult
typecheckValueWith askType askBindings source = do
    started <- getMonotonicTimeNSec
    enabled <- primitiveEnabled
    case (enabled, classifyTypecheckInput source) of
        (False, _) -> finish started "disabled" True "type-check primitive disabled" True
        (_, OutsideValueSubset) ->
            finish started "not-in-value-subset" False "not in the Path-2 value subset" True
        _ -> do
            before <- askBindings
            output <- askType (wrapped source)
            after <- askBindings
            let failed = any (`T.isInfixOf` output) failureSignals
                ok = not failed && expectedSuffix source `T.isInfixOf` output
            finish started (if ok then "ok" else "diagnostic") ok output (before == after)
  where
    failureSignals = ["error:", "Found hole:", "parse error"]
    finish started verdict ok diagnostics unchanged = do
        finished <- getMonotonicTimeNSec
        hPutStrLn stderr $
            "sabela_typecheck mode=path2-value verdict="
                <> verdict
                <> " no_pollution="
                <> map toLower (show unchanged)
                <> " latency_us="
                <> show ((finished - started) `div` 1000)
        pure
            ( TypecheckResult
                (ok && unchanged)
                ( if unchanged
                    then diagnostics
                    else diagnostics <> "\nPath-2 polluted live bindings"
                )
            )
    wrapped s = case classifyTypecheckInput s of
        ValueBindings ->
            ("(let { " <>)
                . (<> " } in ())")
                . T.intercalate "; "
                . map stripLet
                $ meaningfulLines s
        _ -> s
    expectedSuffix s = case classifyTypecheckInput s of
        ValueBindings -> ":: ()"
        _ -> "::"

primitiveEnabled :: IO Bool
primitiveEnabled = do
    value <- lookupEnv "SABELA_TYPECHECK_PRIMITIVE"
    pure $ maybe True ((`notElem` ["0", "off", "false", "no"]) . map toLower) value
