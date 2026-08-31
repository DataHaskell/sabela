{- |
Technique: check execution and verdict extraction [Gating/Repair].
Guarantee: a verdict is read only from an executed marker cell; otherwise 'NoVerdict'.
Entry: 'checkVerdict3With'. Next: Siza.Agent.Loop.Verdict.
-}
module Siza.Agent.Check (
    CheckResult (..),
    MarkerRun (..),
    NoVerdict (..),
    caseScrutinee,
    probeExpr,
    ceMarkerSrc,
    checkVerdict3With,
    checkVerdictWith,
    classifyCheck,
    classifyMarker,
    classifyTryBool,
    counterexampleFor,
    conjuncts,
    counterexampleLine,
    eqLhs,
    degenerateCheck,
    degenerateNote,
    extractTestExpr,
    feedbackContinuation,
    interpretConfirm,
    markerOutput,
    markerSrc,
    noVerdictNote,
    noVerdicts,
    parseCeIndex,
    runMarkerWith,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum, isLower)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (ToolName)
import Sabela.AI.Types (ToolOutcome (ToolOk))
import Siza.Agent.Check.Marker (
    MarkerRun (..),
    markerOutput,
    markerSrc,
    runMarkerWith,
 )
import Siza.Agent.CheckExtract (
    extractTestExpr,
    feedbackContinuation,
    interpretConfirm,
 )
import Text.Read (readMaybe)

data CheckResult
    = CheckPassed
    | CheckFailed
    | CheckUncheckable
    | CheckNotApplicable
    deriving (Eq, Show)

{- | Why a turn reached no verdict. A closed vocabulary, so "no check applies"
can never be rendered without saying which of these was the case.
-}
data NoVerdict
    = NoCellCommitted
    | NoExecutableCell Int
    | NoCheckProposed
    | CheckDiscarded Text
    | CheckReadsNothing Text
    | CheckDidNotRun Text
    deriving (Eq, Show)

noVerdictNote :: NoVerdict -> Text
noVerdictNote NoCellCommitted =
    "no cell has been committed this episode, so there was nothing to check"
noVerdictNote (NoExecutableCell n) =
    "the "
        <> T.pack (show n)
        <> " cell(s) committed this episode produced no executable, substantive \
           \result, so there was nothing to check"
noVerdictNote NoCheckProposed =
    "no covering check was proposed for this deliverable"
noVerdictNote (CheckDiscarded why) =
    "the proposed check was discarded: " <> why
noVerdictNote (CheckReadsNothing check) =
    "the proposed check `" <> check <> "` reads no notebook binding"
noVerdictNote (CheckDidNotRun check) =
    "the covering check `" <> check <> "` did not run to a verdict"

-- | Every reason, for a spec that must enumerate them.
noVerdicts :: [NoVerdict]
noVerdicts =
    [ NoCellCommitted
    , NoExecutableCell 2
    , NoCheckProposed
    , CheckDiscarded "it references nothing this task committed"
    , CheckReadsNothing "True"
    , CheckDidNotRun "x == 1"
    ]

classifyCheck :: Text -> CheckResult
classifyCheck out
    | "GRADE_PASS" `T.isInfixOf` out = CheckPassed
    | "GRADE_FAIL" `T.isInfixOf` out = CheckFailed
    | otherwise = CheckUncheckable

{- | A marker's verdict. Only a run that reached the kernel can carry one; a
refused scratch cell is uncheckable however its rejection reads.
-}
classifyMarker :: MarkerRun -> CheckResult
classifyMarker (MarkerRefused _) = CheckUncheckable
classifyMarker (MarkerRan out) = classifyCheck out

{- | Classify a `try` of a bare Boolean expression. `try` refuses IO, so the
vetting probe is the expression itself rather than a putStrLn marker.
-}
classifyTryBool :: Either Text ToolOutcome -> CheckResult
classifyTryBool (Right (ToolOk v))
    | fieldOfValue "type" v == "Bool" = case T.strip (fieldOfValue "stdout" v) of
        "True" -> CheckPassed
        "False" -> CheckFailed
        _ -> CheckUncheckable
classifyTryBool _ = CheckUncheckable

fieldOfValue :: Text -> Value -> Text
fieldOfValue k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
fieldOfValue _ _ = ""

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

probeExpr :: Text -> Maybe Text
probeExpr c = case eqLhs c of
    Just lhs -> Just lhs
    Nothing -> caseScrutinee c

ceMarkerSrc :: [Text] -> Text
ceMarkerSrc cs =
    "putStrLn (concat (take 1 ([\"CE_\" ++ show i | (i, ok) <- zip [(0 :: Int) ..] ["
        <> T.intercalate ", " (map paren cs)
        <> "], not ok] ++ [\"CE_NONE\"])))"
  where
    paren c = "(" <> c <> ")"

parseCeIndex :: Text -> Maybe Int
parseCeIndex out = do
    let rest = snd (T.breakOn "CE_" out)
    digits <-
        T.unpack . T.takeWhile (`elem` ("0123456789" :: String)) <$> stripCe rest
    readMaybe digits
  where
    stripCe = T.stripPrefix "CE_"

counterexampleLine :: Text -> Maybe Text -> Text
counterexampleLine conjunct mActual =
    "This required example fails: `"
        <> conjunct
        <> "`"
        <> maybe "." (\v -> " — your code computes `" <> v <> "`.") mActual

checkVerdict3With ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO (CheckResult, Maybe Text)
checkVerdict3With call check = do
    run <- runMarkerWith call (markerSrc check)
    case classifyMarker run of
        CheckPassed -> pure (CheckPassed, Just check)
        CheckFailed -> (,) CheckFailed <$> counterexampleFor call check
        _ -> pure (CheckUncheckable, Just (uncheckableNote check run))

{- | Why the check reached no verdict, carrying the notebook's own words when
the scratch cell was refused.
-}
uncheckableNote :: Text -> MarkerRun -> Text
uncheckableNote check run = noVerdictNote (CheckDidNotRun check) <> because
  where
    because = case run of
        MarkerRefused why -> "; the notebook refused its scratch cell: " <> why
        MarkerRan _ -> "; it named no value the cells make observable"

-- | Two-way variant kept for the specs; 'checkVerdict3With' is the live entry.
checkVerdictWith ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO (Bool, Maybe Text)
checkVerdictWith call check = do
    run <- runMarkerWith call (markerSrc check)
    if classifyMarker run == CheckPassed
        then pure (True, Nothing)
        else (,) False <$> counterexampleFor call check

counterexampleFor ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO (Maybe Text)
counterexampleFor call check = do
    let cs = conjuncts check
    ceRun <- runMarkerWith call (ceMarkerSrc cs)
    case parseCeIndex (markerOutput ceRun) >>= (\i -> lookup i (zip [0 ..] cs)) of
        Nothing -> pure Nothing
        Just c -> do
            mActual <- case probeExpr c of
                Nothing -> pure Nothing
                Just lhs -> do
                    run <- runMarkerWith call ("print (" <> lhs <> ")")
                    pure (probedValue (markerOutput run))
            pure (Just (counterexampleLine c mActual))

probedValue :: Text -> Maybe Text
probedValue out
    | T.null t || "error" `T.isInfixOf` T.toLower t = Nothing
    | otherwise = Just (T.take 120 t)
  where
    t = T.strip (T.filter (/= '"') (T.replace "\\n" "\n" out))

{- | What 'degenerateCheck' read, and what the caller then did. The check is
never evaluated on this path, so nothing is claimed about its value.
-}
degenerateNote :: Text
degenerateNote = "the check names no lowercase identifier, so it was not run"

degenerateCheck :: Text -> Bool
degenerateCheck t = null (identifiers t)
  where
    identifiers =
        filter isName
            . T.split (\c -> not (isAlphaNum c || c == '_' || c == '\''))
    isName w = maybe False (\(c, _) -> isLower c || c == '_') (T.uncons w)
