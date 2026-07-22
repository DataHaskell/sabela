module Eval.Task (
    Task (..),
    Grader (..),
    Verdict (..),
    taskTest,
    verifyDiff,
    verdictFor,
    proposeTest,
    grade,
    gradeVerify,
    fitVerdict,
    fitVerify,
    runMarkerWith,
    markerSrc,
    renderVerdict,
    outputHasVerdict,
    stepsVerdict,
    gradeRenderTask,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (ToolErr, ToolOk))
import Siza.Transport (Conn, callTool)

import Eval.FitCheck (FitOutcome (..), fitOutcome)
import Eval.Render (gradeRender, textField)
import Eval.Tools (renderOutcome)
import Siza.Agent.Check (
    CheckResult (..),
    checkVerdict3With,
    markerSrc,
    runMarkerWith,
 )

data Task = Task
    { taskId :: Text
    , taskPrompt :: Text
    , taskGrader :: Grader
    }

data Grader
    = ByValue Text
    | ByRender
    | ByOutput
    | ByOutputHas [Text]
    | BySteps [Grader]
    | {- | Recompute the reported expression's total squared error over the
      sample; pass within the tolerance (a search-shaped task's covering check).
      -}
      ByFit [(Double, Double)] Double
    | Untested
    deriving (Eq, Show)

taskTest :: Task -> Maybe Text
taskTest task = case taskGrader task of
    ByValue check -> Just check
    _ -> Nothing

data Verdict
    = Surfaced
    | Withheld Text
    | ProposeTest Text
    deriving (Eq, Show)

verifyDiff :: Bool -> Maybe Text -> Bool -> Verdict
verifyDiff compiled mtest testGreen
    | not compiled = Withheld "does not compile"
    | otherwise = case mtest of
        Just _
            | testGreen -> Surfaced
            | otherwise -> Withheld "covering test failed"
        Nothing -> ProposeTest (T.pack "")

verdictFor :: Task -> Bool -> Bool -> Verdict
verdictFor task compiled testGreen =
    case verifyDiff compiled (taskTest task) testGreen of
        ProposeTest _ -> ProposeTest (proposeTest task)
        v -> v

proposeTest :: Task -> Text
proposeTest task =
    "-- proposed covering test for `"
        <> taskId task
        <> "` — confirm or edit, then run:\n"
        <> "putStrLn (if ("
        <> taskId task
        <> " == undefined) then \"GRADE_PASS\" else \"GRADE_FAIL\")"

grade :: Conn -> Text -> Task -> IO (Verdict, Text)
grade conn base task = gradeWith conn base task (taskGrader task)

{- | The loop's three-valued verify verdict (R5-T5): ByValue carries a named
counterexample, ByFit routes extraction misses to 'CheckUncheckable', and an
'Untested' task must never fabricate a failure (run-20260720 spiral).
-}
gradeVerify :: Conn -> Text -> Task -> IO (CheckResult, Maybe Text)
gradeVerify conn base task = case taskGrader task of
    ByValue check -> checkVerdict3With (callTool conn base) check
    Untested -> pure (CheckPassed, Nothing)
    ByFit points tol -> fitVerify points tol <$> notebookOutputs conn base
    _ -> do
        (v, _) <- grade conn base task
        pure (if v == Surfaced then CheckPassed else CheckFailed, Nothing)

{- | Three-valued fit verify: a denial is emitted iff a recomputed nonzero
error exists for a reported expression; an extraction miss says what to run
and never leaks the sample's answer.
-}
fitVerify :: [(Double, Double)] -> Double -> Text -> (CheckResult, Maybe Text)
fitVerify points tol out = case fitOutcome points tol out of
    FitConfirmed _ _ -> (CheckPassed, Nothing)
    FitRefuted err e ->
        ( CheckFailed
        , Just
            ( "the reported expression `"
                <> e
                <> "` has recomputed total squared error "
                <> tShowD err
                <> " over the sample — it does not fit."
            )
        )
    FitUnconfirmed reason ->
        ( CheckUncheckable
        , Just
            ( "not yet confirmed: "
                <> reason
                <> ". Run a cell that prints the best expression and its total \
                   \squared error, e.g. `Best expression: <expr>, total squared \
                   \error: <err>`."
            )
        )

-- | Every scanned cell's rendered output, the text 'fitVerify' vets.
notebookOutputs :: Conn -> Text -> IO Text
notebookOutputs conn base = do
    ids <- take renderScanCap <$> codeCellIds conn base
    outs <- mapM (executeOutcome conn base) ids
    pure (T.unlines [renderOutcome (Right o) | o <- outs, hasOutput o])

gradeWith :: Conn -> Text -> Task -> Grader -> IO (Verdict, Text)
gradeWith conn base task grader = case grader of
    Untested -> pure (ProposeTest (proposeTest task), "no covering test; proposed one")
    ByRender -> gradeRenderTask conn base
    ByValue check -> do
        (green, out) <- runMarker conn base (markerSrc check)
        pure (verifyDiff True (Just check) green, out)
    ByOutput -> gradeOutputTask conn base
    ByOutputHas needles -> gradeOutputHas conn base needles
    ByFit points tol -> gradeFit conn base points tol
    BySteps stages -> gradeSteps conn base task stages

runMarker :: Conn -> Text -> Text -> IO (Bool, Text)
runMarker conn base = runMarkerWith (callTool conn base)

gradeOutputTask :: Conn -> Text -> IO (Verdict, Text)
gradeOutputTask conn base = do
    ids <- take renderScanCap <$> codeCellIds conn base
    outs <- mapM (executeOutcome conn base) ids
    pure $ case filter hasOutput outs of
        (o : _) -> (Surfaced, renderOutcome (Right o))
        [] -> (Withheld "no cell produced output", "no output")

hasOutput :: ToolOutcome -> Bool
hasOutput (ToolOk (Object o)) = case KM.lookup "outputs" o of
    Just (Array a) -> not (null a)
    _ -> False
hasOutput _ = False

gradeOutputHas :: Conn -> Text -> [Text] -> IO (Verdict, Text)
gradeOutputHas conn base needles = do
    ids <- take renderScanCap <$> codeCellIds conn base
    outs <- mapM (executeOutcome conn base) ids
    pure (outputHasVerdict needles outs)

{- | Scan the notebook's cell outputs for the run's reported expression and
recompute its total squared error over the sample: any expression fitting within
the tolerance surfaces, so the check never overfits to one known answer string.
-}
gradeFit :: Conn -> Text -> [(Double, Double)] -> Double -> IO (Verdict, Text)
gradeFit conn base points tol =
    fitVerdict points tol <$> notebookOutputs conn base

{- | Turn the recomputed three-valued fit outcome into a grading verdict: only
a confirmed fit surfaces; refuted and unconfirmed are withheld with the reason.
-}
fitVerdict :: [(Double, Double)] -> Double -> Text -> (Verdict, Text)
fitVerdict points tol text = case fitOutcome points tol text of
    FitConfirmed err _ ->
        (Surfaced, "fit error " <> tShowD err <> " within tolerance")
    FitRefuted err _ ->
        ( Withheld ("fit error " <> tShowD err <> " exceeds tolerance")
        , "fit error " <> tShowD err
        )
    FitUnconfirmed reason -> (Withheld reason, reason)

tShowD :: Double -> Text
tShowD = T.pack . show

outputHasVerdict :: [Text] -> [ToolOutcome] -> (Verdict, Text)
outputHasVerdict needles outs = case filter hit outs of
    (o : _) -> (Surfaced, renderOutcome (Right o))
    [] -> (Withheld ("output missing: " <> T.intercalate ", " needles), "no match")
  where
    hit o = hasOutput o && all (`T.isInfixOf` renderOutcome (Right o)) needles

gradeSteps :: Conn -> Text -> Task -> [Grader] -> IO (Verdict, Text)
gradeSteps conn base task stages =
    stepsVerdict <$> mapM (gradeWith conn base task) stages

stepsVerdict :: [(Verdict, Text)] -> (Verdict, Text)
stepsVerdict rs = case filter ((/= Surfaced) . fst . snd) numbered of
    ((n, (v, ev)) : _) -> (v, "step " <> tShowInt n <> " withheld: " <> ev)
    [] -> (Surfaced, T.intercalate "\n" [step n ev | (n, (_, ev)) <- numbered])
  where
    numbered = zip [1 :: Int ..] rs
    step n ev = "step " <> tShowInt n <> ": " <> ev

tShowInt :: Int -> Text
tShowInt = T.pack . show

gradeRenderTask :: Conn -> Text -> IO (Verdict, Text)
gradeRenderTask conn base = do
    ids <- take renderScanCap <$> codeCellIds conn base
    pairs <- mapM (sourceAndOutcome conn base) ids
    pure (renderVerdict pairs)

renderScanCap :: Int
renderScanCap = 8

renderVerdict :: [(Text, ToolOutcome)] -> (Verdict, Text)
renderVerdict pairs =
    case [ev | (ok, ev) <- graded, ok] of
        (ev : _) -> (Surfaced, ev)
        [] -> (Withheld "no cell rendered an SVG", lastEvidence)
  where
    graded = [gradeRender src oc | (src, oc) <- pairs]
    lastEvidence = case reverse graded of
        ((_, ev) : _) -> ev
        [] -> "no code cell produced output to grade"

codeCellIds :: Conn -> Text -> IO [Int]
codeCellIds conn base = codeCells <$> listCells conn base

sourceAndOutcome :: Conn -> Text -> Int -> IO (Text, ToolOutcome)
sourceAndOutcome conn base cid =
    (,) <$> cellSource conn base cid <*> executeOutcome conn base cid

cellSource :: Conn -> Text -> Int -> IO Text
cellSource conn base cid = do
    r <- callTool conn base ReadCell (object ["cell_id" .= cid])
    pure $ case r of
        Right (ToolOk (Object o)) -> textField "source" o
        _ -> ""

executeOutcome :: Conn -> Text -> Int -> IO ToolOutcome
executeOutcome conn base cid = do
    r <- callTool conn base ExecuteCell (object ["cell_id" .= cid])
    pure $ case r of
        Right oc -> oc
        Left e -> ToolErr (object ["error" .= e])

codeCells :: Value -> [Int]
codeCells (Array a) =
    sortOn
        Down
        [ round s
        | Object c <- toList a
        , textField "type" c == "CodeCell"
        , Just (Number s) <- [KM.lookup "id" c]
        ]
codeCells (Object o) = maybe [] codeCells (KM.lookup "cells" o)
codeCells _ = []

listCells :: Conn -> Text -> IO Value
listCells conn base = do
    r <- callTool conn base ListCells (object [])
    pure $ case r of
        Right (ToolOk v) -> v
        _ -> object []
