{-# LANGUAGE OverloadedStrings #-}

module Test.TaskSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text, isInfixOf)
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

import Eval.Task (
    Grader (..),
    Task (..),
    Verdict (..),
    findTask,
    markerSrc,
    outputHasVerdict,
    renderVerdict,
    runMarkerWith,
    stepsVerdict,
    taskTest,
    tasks,
 )

outcomeWith :: Text -> Text -> ToolOutcome
outcomeWith mime payload =
    ToolOk (object ["ok" .= True, "outputs" .= [item mime payload]])
  where
    item m p = object ["oiMime" .= m, "oiOutput" .= p]

noOutput :: ToolOutcome
noOutput = ToolOk (object ["ok" .= True, "outputs" .= ([] :: [Text])])

drawnSvg :: Text
drawnSvg =
    "<svg xmlns=\"http://www.w3.org/2000/svg\"><rect width=\"10\" height=\"10\"/></svg>"

isByValue :: Grader -> Bool
isByValue (ByValue _) = True
isByValue _ = False

spec :: Spec
spec = describe "plotting/dataframe tasks" $ do
    describe "the dataset carries the new tasks" $ do
        it "has a granite bar-chart task graded by render" $ do
            (taskGrader <$> findTask "quarterlyBars") `shouldBe` Just ByRender
        it "has a dataframe-summary task graded by value over its binding" $ do
            isJust (findTask "revenueTotal") `shouldBe` True
            (isByValue . taskGrader <$> findTask "revenueTotal") `shouldBe` Just True
            (taskTest =<< findTask "revenueTotal")
                `shouldSatisfy` maybe False ("revenueTotal" `isInfixOf`)
        it "has a dataframe-to-plot task graded by render" $
            (taskGrader <$> findTask "revenueChart") `shouldBe` Just ByRender
        it "prompts the dataframe tasks toward the dataframe library" $
            (taskPrompt <$> findTask "revenueTotal")
                `shouldSatisfy` maybe False ("dataframe" `isInfixOf`)
        it "keeps the original six plus the three additions" $
            length tasks `shouldSatisfy` (>= 9)

    describe "renderVerdict (pure)" $ do
        it "surfaces as soon as one cell draws a real SVG" $
            fst
                ( renderVerdict
                    [("displaySvg (T.unpack chart)", outcomeWith "image/svg+xml" drawnSvg)]
                )
                `shouldBe` Surfaced
        it "withholds when no candidate cell rendered" $
            fst (renderVerdict [("revenueTotal = 600", outcomeWith "text/plain" "600.0")])
                `shouldSatisfy` isWithheld
        it "withholds on an empty notebook" $
            fst (renderVerdict []) `shouldSatisfy` isWithheld
        it "rejects a cell that counterfeits the SVG in its source" $
            -- the payload is a real SVG but the SOURCE embeds <svg, so it is faked
            fst
                ( renderVerdict
                    [("displayHtml \"<svg><rect/></svg>\"", outcomeWith "text/html" drawnSvg)]
                )
                `shouldSatisfy` isWithheld
        it "finds the drawing cell among several non-drawing ones" $
            fst
                ( renderVerdict
                    [ ("import Granite.Svg", outcomeWith "text/plain" "")
                    , ("revenueTotal = 600", outcomeWith "text/plain" "600.0")
                    , ("displaySvg (T.unpack chart)", outcomeWith "image/svg+xml" drawnSvg)
                    ]
                )
                `shouldBe` Surfaced

    describe "outputHasVerdict (pure)" $ do
        it "surfaces the cell whose output contains every needle" $
            fst
                ( outputHasVerdict
                    ["Jan", "Feb", "Mar"]
                    [outcomeWith "text/plain" "month: Jan Feb Mar"]
                )
                `shouldBe` Surfaced
        it "withholds when a needle is missing" $
            fst (outputHasVerdict ["600"] [outcomeWith "text/plain" "100.0 200.0 300.0"])
                `shouldSatisfy` isWithheld
        it "withholds when the cell produced no output" $
            fst (outputHasVerdict ["Jan"] [noOutput]) `shouldSatisfy` isWithheld
        it "finds the matching cell among several" $
            fst
                ( outputHasVerdict
                    ["600"]
                    [outcomeWith "text/plain" "loading", outcomeWith "text/plain" "total 600.0"]
                )
                `shouldBe` Surfaced

    describe "stepsVerdict (pure)" $ do
        it "surfaces only when every stage surfaced" $
            fst (stepsVerdict [(Surfaced, "load"), (Surfaced, "plot")]) `shouldBe` Surfaced
        it "withholds and names the first failing step" $ do
            let (v, msg) = stepsVerdict [(Surfaced, "load"), (Withheld "no svg", "plot")]
            v `shouldSatisfy` isWithheld
            msg `shouldSatisfy` ("step 2" `isInfixOf`)
        it "reports the earliest failure when several fail" $
            snd (stepsVerdict [(Withheld "a", "x"), (Withheld "b", "y")])
                `shouldSatisfy` ("step 1" `isInfixOf`)

    describe "runMarkerWith (grade off-notebook)" $
        it "deletes the marker cell after grading so the notebook keeps no GRADE cell" $ do
            calls <- newIORef []
            let fake tn args = do
                    modifyIORef' calls (++ [(tn, args)])
                    pure . Right $ case tn of
                        ListCells ->
                            ToolOk (object ["cells" .= [object ["id" .= (5 :: Int)]]])
                        ExecuteCell ->
                            ToolOk
                                (object ["outputs" .= [object ["oiOutput" .= ("GRADE_PASS" :: Text)]]])
                        _ -> ToolOk (object [])
            (green, _) <- runMarkerWith fake (markerSrc "True")
            green `shouldBe` True
            seen <- readIORef calls
            map fst seen `shouldBe` [InsertCell, ListCells, ExecuteCell, DeleteCell]
            lookup DeleteCell seen `shouldBe` Just (object ["cell_id" .= (5 :: Int)])

isWithheld :: Verdict -> Bool
isWithheld (Withheld _) = True
isWithheld _ = False
