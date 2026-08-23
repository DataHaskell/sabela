module Test.NotebookWidgetSpec (spec) where

import Data.List (isInfixOf)
import Sabela.Notebook.Chart (barChart)
import Sabela.Notebook.Event (accumB, mapE, merge, stepper)
import Sabela.Notebook.Widget
import Sabela.Notebook.Widget.Kit
import Test.Hspec

-- | A widget with two buttons, the running example.
tally :: Ui Int
tally = do
    up <- pushButton "+"
    downwards <- pushButton "-"
    n <- sample (accumB 0 (merge (mapE (const (+ 1)) up) (mapE (const (subtract 1)) downwards)))
    say ("count: " ++ show n)
    pure n

-- | A store as the browser would leave it: slot name to occurrence log.
pressedAt :: [Double] -> String
pressedAt ts = show [(t, "") | t <- ts]

spec :: Spec
spec = do
    describe "a widget is a function from what the reader did" $ do
        it "reads zero before anything has happened" $
            valueOf "t" [] tally `shouldBe` 0
        it "counts the presses in the log" $
            valueOf "t" [("t:+", pressedAt [1, 2, 3])] tally `shouldBe` 3
        it "folds both controls in time order" $
            valueOf "t" [("t:+", pressedAt [1, 2, 3]), ("t:-", pressedAt [4])] tally
                `shouldBe` 2
        it "ignores a slot belonging to another widget" $
            valueOf "t" [("other:+", pressedAt [1, 2])] tally `shouldBe` 0
        it "treats an unreadable log as no interactions" $
            valueOf "t" [("t:+", "not a log at all")] tally `shouldBe` 0

    describe "controls keep their identity" $ do
        it "names a slot per control" $
            slotsOf "t" tally `shouldBe` ["t:+", "t:-"]
        it "separates two controls that share a label" $
            slotsOf "t" (pushButton "go" >> pushButton "go")
                `shouldBe` ["t:go", "t:go#2"]
        it "keyed scopes every control inside it" $
            slotsOf "t" (keyed "left" (pushButton "go") >> pushButton "go")
                `shouldBe` ["t:left/go", "t:go"]
        it "keyed reaches into a nested layout" $
            slotsOf "t" (keyed "box" (across (pushButton "go")))
                `shouldBe` ["t:box/go"]

    describe "the program is data, so more than one interpreter reads it" $ do
        it "describes its structure without a browser" $
            describeUi "t" tally
                `shouldBe` ["control t:+ press", "control t:- press", "say \"count: 0\""]
        it "renders the controls it described" $ do
            let (html, _) = renderWidget tally "t" "7" []
            html `shouldSatisfy` isInfixOf "data-slot='t:+'"
            html `shouldSatisfy` isInfixOf "data-slot='t:-'"
        it "carries each control's log to the browser" $ do
            let (html, _) = renderWidget tally "t" "7" [("t:+", pressedAt [1])]
            html `shouldSatisfy` isInfixOf "data-log='[(1.0,&quot;&quot;)]'"
        it "boots the runtime against its own element" $ do
            let (html, _) = renderWidget tally "t" "7" []
            html `shouldSatisfy` isInfixOf "sabelaUi({cid:7,root:'sbw_7_t'})"

    describe "rendering is safe against what a reader can type" $ do
        it "escapes text" $ do
            let (html, _) = renderWidget (say "<script>alert(1)</script>") "t" "1" []
            html `shouldSatisfy` isInfixOf "&lt;script&gt;"
            html `shouldNotSatisfy` isInfixOf "<script>alert"
        it "escapes what came back from the browser" $ do
            let typed = show [(1 :: Double, "' onerror='x")]
                (html, _) = renderWidget (typedText "name" "") "t" "1" [("t:name/name", typed)]
            html `shouldNotSatisfy` isInfixOf "' onerror='x"

    describe "drawings and behaviours sit in the same program" $ do
        it "puts a chart in the widget" $ do
            let (html, _) = renderWidget (paint (barChart [("a", 1)])) "t" "1" []
            html `shouldSatisfy` isInfixOf "<svg"
        it "samples a behaviour at the reader's latest moment" $
            valueOf "t" [("t:go", pressedAt [1, 5])] (pushButton "go" >> now)
                `shouldBe` 5

    describe "the kit answers the common asks in one call" $ do
        it "counter counts" $
            valueOf "c" [("c:counter/+", pressedAt [1, 2])] (counter "counter" 0) `shouldBe` 2
        it "pickOne starts on the first option" $
            valueOf "p" [] (pickOne "fruit" ["apple", "pear"]) `shouldBe` "apple"
        it "pickOne takes the reader's last choice" $
            valueOf "p" [("p:fruit/fruit", show [(1 :: Double, "pear")])] (pickOne "fruit" ["apple", "pear"])
                `shouldBe` "pear"
        it "numberBox reads the slider" $
            valueOf "n" [("n:size/size", show [(2 :: Double, "7.5")])] (numberBox "size" 0 10 1)
                `shouldBe` 7.5
        it "onOff reads the switch" $
            valueOf "o" [("o:live/live", show [(1 :: Double, "True")])] (onOff "live" False)
                `shouldBe` True

    describe "stepper and the log agree" $
        it "keeps the latest occurrence" $
            valueOf
                "t"
                [("t:name/name", show [(1 :: Double, "a"), (2, "b")])]
                (typedText "name" "")
                `shouldBe` "b"
