{-# LANGUAGE OverloadedStrings #-}

{- | The chat-native covering check: propose a Haskell boolean, run it off-notebook
through a throwaway marker cell, and classify the result. This is the light half of
what the eval grader ('Eval.Task') once bundled — no scoring corpus, no 'Verdict',
just insert/exec/delete a @GRADE_PASS@/@GRADE_FAIL@ marker and read the token back.
-}
module Siza.Agent.Check (
    CheckResult (..),
    caseScrutinee,
    probeExpr,
    ceMarkerSrc,
    checkVerdict3With,
    checkVerdictWith,
    classifyCheck,
    counterexampleFor,
    conjuncts,
    counterexampleLine,
    eqLhs,
    extractTestExpr,
    interpretConfirm,
    markerSrc,
    parseCeIndex,
    runMarkerWith,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (ToolOk))
import Siza.Agent.CheckExtract (extractTestExpr, interpretConfirm)
import Siza.Agent.Tools (renderOutcome, withInsertDefaults)
import Text.Read (readMaybe)

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

{- | Split a boolean check on its top-level @&&@ — each conjunct is one required
example, so a failure can be pinned to the first example that does not hold.
Bracket- and string-aware; a conjunction-free check is a single conjunct.
-}
conjuncts :: Text -> [Text]
conjuncts = map (T.strip . T.pack) . go 0 "" . T.unpack
  where
    go :: Int -> String -> String -> [String]
    go 0 acc ('&' : '&' : cs) = reverse acc : go 0 "" cs
    go d acc ('"' : cs) = let (s, cs') = spanString cs in go d (reverse s ++ '"' : acc) cs'
    go d acc (c : cs)
        | c `elem` ("([{" :: String) = go (d + 1) (c : acc) cs
        | c `elem` (")]}" :: String) = go (max 0 (d - 1)) (c : acc) cs
        | otherwise = go d (c : acc) cs
    go _ acc [] = [reverse acc]
    spanString ('\\' : c : cs) = let (s, cs') = spanString cs in ('\\' : c : s, cs')
    spanString ('"' : cs) = ("\"", cs)
    spanString (c : cs) = let (s, cs') = spanString cs in (c : s, cs')
    spanString [] = ("", [])

{- | The LHS of a conjunct shaped @lhs == rhs@ at bracket depth 0 — the side
worth probing with @print@ for the actual value. 'Nothing' for any other shape.
-}
eqLhs :: Text -> Maybe Text
eqLhs t = go (0 :: Int) "" (T.unpack t)
  where
    go 0 acc ('=' : '=' : _)
        | not (T.null lhs) = Just lhs
      where
        lhs = T.strip (T.pack (reverse acc))
    go d acc ('"' : cs) = let (s, cs') = breakString cs in go d (reverse s ++ '"' : acc) cs'
    go d acc (c : cs)
        | c `elem` ("([{" :: String) = go (d + 1) (c : acc) cs
        | c `elem` (")]}" :: String) = go (max 0 (d - 1)) (c : acc) cs
        | otherwise = go d (c : acc) cs
    go _ _ [] = Nothing
    breakString ('\\' : c : cs) = let (s, cs') = breakString cs in ('\\' : c : s, cs')
    breakString ('"' : cs) = ("\"", cs)
    breakString (c : cs) = let (s, cs') = breakString cs in (c : s, cs')
    breakString [] = ("", [])

{- | The scrutinee of a conjunct shaped @(case expr of ...)@ — a tolerance
check wraps the interesting call in a case, so the scrutinee is the side worth
probing for the actual value.
-}
caseScrutinee :: Text -> Maybe Text
caseScrutinee t = do
    body <- T.stripPrefix "case " (T.strip (stripParens (T.strip t)))
    let (scrut, rest) = T.breakOn " of" body
    if T.null rest || T.null (T.strip scrut)
        then Nothing
        else Just (T.strip scrut)
  where
    stripParens s = case (T.stripPrefix "(" s, T.stripSuffix ")" s) of
        (Just inner, Just _) -> T.dropEnd 1 inner
        _ -> s

-- | The probe-able expression of a conjunct: an @==@ LHS, or a case scrutinee.
probeExpr :: Text -> Maybe Text
probeExpr c = case eqLhs c of
    Just lhs -> Just lhs
    Nothing -> caseScrutinee c

{- | A marker that prints @CE_i@ for the first failing conjunct (or @CE_NONE@),
so ONE execution names the failing example.
-}
ceMarkerSrc :: [Text] -> Text
ceMarkerSrc cs =
    "putStrLn (head ([\"CE_\" ++ show i | (i, ok) <- zip [(0 :: Int) ..] ["
        <> T.intercalate ", " (map paren cs)
        <> "], not ok] ++ [\"CE_NONE\"]))"
  where
    paren c = "(" <> c <> ")"

-- | The failing-conjunct index out of a 'ceMarkerSrc' run's output.
parseCeIndex :: Text -> Maybe Int
parseCeIndex out = do
    let rest = snd (T.breakOn "CE_" out)
    digits <-
        T.unpack . T.takeWhile (`elem` ("0123456789" :: String)) <$> stripCe rest
    readMaybe digits
  where
    stripCe = T.stripPrefix "CE_"

-- | The sentence the model sees: the failing example, plus the value if probed.
counterexampleLine :: Text -> Maybe Text -> Text
counterexampleLine conjunct mActual =
    "This required example fails: `"
        <> conjunct
        <> "`"
        <> maybe "." (\v -> " — your code computes `" <> v <> "`.") mActual

{- | The three-valued verdict (R5-T5): 'CheckFailed' only on a recomputed
GRADE_FAIL (with the counterexample when one pins); a check that reached no
verdict is 'CheckUncheckable' with what-to-run guidance, never a failure claim.
-}
checkVerdict3With ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO (CheckResult, Maybe Text)
checkVerdict3With call check = do
    (_, out) <- runMarkerWith call (markerSrc check)
    case classifyCheck out of
        CheckPassed -> pure (CheckPassed, Nothing)
        CheckFailed -> (,) CheckFailed <$> counterexampleFor call check
        CheckUncheckable ->
            pure
                ( CheckUncheckable
                , Just
                    ( "the covering check `"
                        <> check
                        <> "` could not run to a verdict against the current \
                           \cells — define the deliverable it names, run the \
                           \cell clean, then finish."
                    )
                )

{- | Boolean verdict plus counterexample in one call: run the check marker; on
failure name the first failing example and probe its @==@ LHS for the actual
value. Every probe degrades to 'Nothing' rather than blocking the verdict.
-}
checkVerdictWith ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO (Bool, Maybe Text)
checkVerdictWith call check = do
    (ok, _) <- runMarkerWith call (markerSrc check)
    if ok
        then pure (True, Nothing)
        else (,) False <$> counterexampleFor call check

{- | The failing-example line for a check already known to fail (see
'checkVerdictWith'); 'Nothing' when no conjunct can be pinned.
-}
counterexampleFor ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO (Maybe Text)
counterexampleFor call check = do
    let cs = conjuncts check
    (_, ceOut) <- runMarkerWith call (ceMarkerSrc cs)
    case parseCeIndex ceOut >>= (\i -> lookup i (zip [0 ..] cs)) of
        Nothing -> pure Nothing
        Just c -> do
            mActual <- case probeExpr c of
                Nothing -> pure Nothing
                Just lhs -> do
                    (_, out) <- runMarkerWith call ("print (" <> lhs <> ")")
                    pure (probedValue out)
            pure (Just (counterexampleLine c mActual))

{- | A probe's printed value, or 'Nothing' when the probe itself errored. The
marker output is the execute-cell JSON envelope, so dig out the first output's
@oiOutput@; fall back to stripping the raw text when it is not that shape.
-}
probedValue :: Text -> Maybe Text
probedValue out
    | T.null t || "error" `T.isInfixOf` T.toLower t = Nothing
    | otherwise = Just (T.take 120 t)
  where
    t = T.strip (fromMaybe stripped (oiOutputOf out))
    stripped = T.strip (T.filter (/= '"') (T.replace "\\n" "\n" out))

-- | The first @oiOutput@ inside an execute-cell result rendered as JSON text.
oiOutputOf :: Text -> Maybe Text
oiOutputOf out = do
    v <- A.decode (BL.fromStrict (encodeUtf8 out))
    o <- asObject v
    outputs <- KM.lookup "outputs" o
    first <- case outputs of
        Array a -> case toList a of
            (x : _) -> Just x
            [] -> Nothing
        _ -> Nothing
    fo <- asObject first
    case KM.lookup "oiOutput" fo of
        Just (String s) -> Just s
        _ -> Nothing
  where
    asObject (Object o) = Just o
    asObject _ = Nothing

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
