module Eval.Task (
    Task (..),
    Grader (..),
    Verdict (..),
    tasks,
    findTask,
    taskTest,
    verifyDiff,
    verdictFor,
    proposeTest,
    grade,
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
import Data.List (find, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (ToolErr, ToolOk))
import Siza.Transport (Conn, callTool)

import Eval.Render (gradeRender, textField)
import Eval.Tools (renderOutcome, withInsertDefaults)

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
    | Untested
    deriving (Show, Eq)

taskTest :: Task -> Maybe Text
taskTest task = case taskGrader task of
    ByValue check -> Just check
    _ -> Nothing

data Verdict
    = Surfaced
    | Withheld Text
    | ProposeTest Text
    deriving (Show, Eq)

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

tasks :: [Task]
tasks =
    [ Task
        "double"
        "Define a function `double :: Int -> Int` that returns twice its argument."
        (ByValue "double 21 == 42")
    , Task
        "applyTwice"
        "Define `applyTwice :: (a -> a) -> a -> a` that applies a function to a value twice."
        (ByValue "applyTwice (+3) (10 :: Int) == 16")
    , Task
        "safeDiv"
        "Define `safeDiv :: Int -> Int -> Maybe Int` returning Nothing when the divisor is 0, otherwise Just the quotient."
        (ByValue "safeDiv 10 0 == Nothing && safeDiv 10 2 == Just 5")
    , Task
        "color"
        "Define a type `Color` with constructors Red, Green and Blue, deriving Show, Eq, Enum and Bounded. Then define `allColors :: [Color]` listing every colour using minBound/maxBound."
        (ByValue "allColors == [Red, Green, Blue]")
    , Task
        "mapMaybe"
        "Define `mapMaybe' :: (a -> Maybe b) -> [a] -> [b]` that applies the function to each element and keeps only the Just results."
        ( ByValue
            "mapMaybe' (\\x -> if even x then Just (x * x) else Nothing) [1..6 :: Int] == [4, 16, 36]"
        )
    , Task
        "treeFunctor"
        "Define `data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)` and a Functor instance for Tree that maps over the stored values."
        (ByValue "fmap (+1) (Node Leaf (1 :: Int) Leaf) == Node Leaf 2 Leaf")
    , Task
        "quarterlyBars"
        "Plot these quarterly sales figures as a bar chart and show the chart in the notebook: Q1 12, Q2 18, Q3 9, Q4 15. Use the granite plotting library."
        ByRender
    , Task
        "revenueTotal"
        "A CSV file `revenue.csv` with columns `month` and `revenue` is in the working directory. Using the dataframe library, load it into a DataFrame and define `revenueTotal :: Double` as the total revenue across all months."
        (ByValue "abs (revenueTotal - 600) < 0.001")
    , Task
        "revenueChart"
        "A CSV file `revenue.csv` with columns `month` and `revenue` is in the working directory. Using the dataframe library, load it into a DataFrame, then plot revenue by month as a bar chart with the granite library and show the chart in the notebook."
        ByRender
    , Task
        "fixBroken"
        "This function is meant to sum a list of Ints but does not compile:\n\n    total xs = foldr (+) xs\n\nFix it and define `total :: [Int] -> Int` so it returns the sum of the list."
        (ByValue "total [1, 2, 3, 4] == 10 && total [] == 0")
    ]

findTask :: Text -> Maybe Task
findTask tid = find ((== tid) . taskId) tasks

grade :: Conn -> Text -> Task -> IO (Verdict, Text)
grade conn base task = gradeWith conn base task (taskGrader task)

gradeWith :: Conn -> Text -> Task -> Grader -> IO (Verdict, Text)
gradeWith conn base task grader = case grader of
    Untested -> pure (ProposeTest (proposeTest task), "no covering test; proposed one")
    ByRender -> gradeRenderTask conn base
    ByValue check -> do
        (green, out) <- runMarker conn base (markerSrc check)
        pure (verifyDiff True (Just check) green, out)
    ByOutput -> gradeOutputTask conn base
    ByOutputHas needles -> gradeOutputHas conn base needles
    BySteps stages -> gradeSteps conn base task stages

markerSrc :: Text -> Text
markerSrc check =
    "putStrLn (if (" <> check <> ") then \"GRADE_PASS\" else \"GRADE_FAIL\")"

runMarker :: Conn -> Text -> Text -> IO (Bool, Text)
runMarker conn base = runMarkerWith (callTool conn base)

{- | Run a grading marker and report whether it greened, INJECTING the tool
caller so the path is testable without a live server. Grades OFF-notebook: the
marker has to run in the live session to see the notebook's bindings, but it is
deleted afterwards so the notebook is never left holding a noisy
GRADE_PASS/GRADE_FAIL acceptance cell.
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

maxId :: Value -> Int
maxId (Array a) =
    maximum
        (0 : [round s | Object c <- toList a, Just (Number s) <- [KM.lookup "id" c]])
maxId (Object o) = maybe 0 maxId (KM.lookup "cells" o)
maxId _ = 0
