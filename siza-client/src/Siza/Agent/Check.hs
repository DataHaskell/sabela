{-# LANGUAGE OverloadedStrings #-}

{- | The chat-native covering check: propose a Haskell boolean, run it off-notebook
through a throwaway marker cell, and classify the result. This is the light half of
what the eval grader ('Eval.Task') once bundled — no scoring corpus, no 'Verdict',
just insert/exec/delete a @GRADE_PASS@/@GRADE_FAIL@ marker and read the token back.
-}
module Siza.Agent.Check (
    CheckResult (..),
    classifyCheck,
    extractTestExpr,
    interpretConfirm,
    markerSrc,
    runMarkerWith,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Salvage (salvageCell)
import Sabela.AI.Types (ToolOutcome (ToolOk))
import Siza.Agent.Tools (renderOutcome, withInsertDefaults)

{- | The three outcomes of a covering check, telling an invalid target (the check
itself does not compile) apart from a genuinely failing one.
-}
data CheckResult = CheckPassed | CheckFailed | CheckUncheckable
    deriving (Eq, Show)

{- | Classify a covering-check marker's output: 'markerSrc' prints GRADE_PASS /
GRADE_FAIL when the check compiles, so neither token means the check itself did not
compile — no clear target, not a wrong answer.
-}
classifyCheck :: Text -> CheckResult
classifyCheck out
    | "GRADE_PASS" `T.isInfixOf` out = CheckPassed
    | "GRADE_FAIL" `T.isInfixOf` out = CheckFailed
    | otherwise = CheckUncheckable

markerSrc :: Text -> Text
markerSrc check =
    "putStrLn (if (" <> check <> ") then \"GRADE_PASS\" else \"GRADE_FAIL\")"

{- | Run a covering-check marker and report whether it greened; the tool caller is
injected so this is testable without a live server. The marker runs in the live
session (to see its bindings) but is deleted after, leaving no acceptance cell.
-}
runMarkerWith ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) -> Text -> IO (Bool, Text)
runMarkerWith call src = do
    _ <- call InsertCell (withInsertDefaults (object ["source" .= src]))
    after <- maxId . listValue <$> call ListCells (object [])
    out <- renderOutcome <$> call ExecuteCell (object ["cell_id" .= after])
    _ <- call DeleteCell (object ["cell_id" .= after])
    pure (T.isInfixOf "GRADE_PASS" out, out)

-- | Unwrap a @list_cells@ tool result to its cells 'Value' (or empty on error).
listValue :: Either Text ToolOutcome -> Value
listValue (Right (ToolOk v)) = v
listValue _ = object []

maxId :: Value -> Int
maxId (Array a) =
    maximum
        (0 : [round s | Object c <- toList a, Just (Number s) <- [KM.lookup "id" c]])
maxId (Object o) = maybe 0 maxId (KM.lookup "cells" o)
maxId _ = 0

extractTestExpr :: Text -> Text
extractTestExpr reply = case salvageCell reply of
    Just code -> deQuote (firstNonEmptyLine code)
    Nothing -> case inlineCode reply of
        Just span_ -> T.strip span_
        Nothing -> deQuote (firstNonEmptyLine reply)
  where
    deQuote = T.strip . T.dropAround (== '`') . T.strip

firstNonEmptyLine :: Text -> Text
firstNonEmptyLine = headOr "" . filter (not . T.null . T.strip) . T.lines
  where
    headOr d [] = d
    headOr _ (x : _) = x

inlineCode :: Text -> Maybe Text
inlineCode t = case T.breakOn "`" t of
    (_, rest)
        | not (T.null rest)
        , (code, close) <- T.breakOn "`" (T.drop 1 rest)
        , not (T.null close)
        , not (T.null (T.strip code)) ->
            Just code
    _ -> Nothing

interpretConfirm :: Text -> Text -> Text
interpretConfirm proposed input
    | low `elem` ["skip", "no", "n"] = ""
    | T.null low || low `elem` ["y", "yes"] = proposed
    | looksLikeTest stripped = stripped
    | otherwise = ""
  where
    stripped = T.strip input
    low = T.toLower stripped

looksLikeTest :: Text -> Bool
looksLikeTest t =
    any
        (`T.isInfixOf` t)
        ["==", "/=", "<=", ">=", "<", ">", "&&", "||", " elem ", "isInfixOf"]
