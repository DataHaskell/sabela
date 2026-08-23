-- | Whole widgets, one call each, named after what they do.
--
-- Reach for these first. Each one is an ordinary program in the vocabulary of
-- "Sabela.Notebook.Widget", so when you want something they do not do, read the
-- source of the nearest one and write your own in the same words.
--
-- > chosen <- mkWidget (htmlWidget "fruit" (renderWidget (pickOne "fruit" ["apple", "pear"])))
module Sabela.Notebook.Widget.Kit (
    counter,
    pickOne,
    numberBox,
    onOff,
    typedText,
) where

import Sabela.Notebook.Event (accumB, mapE, merge, stepper)
import Sabela.Notebook.Widget.Dsl
import Sabela.Notebook.Widget.Types (Ui)

-- | A number with buttons either side of it, and the count it stands at.
--
-- > n <- mkWidget (htmlWidget "hits" (renderWidget (counter "hits" 0)))
counter :: String -> Int -> Ui Int
counter labelText start = keyed labelText $ do
    up <- pushButton "+"
    downwards <- pushButton "-"
    n <- sample (accumB start (merge (mapE (const (+ 1)) up) (mapE (const (subtract 1)) downwards)))
    say (labelText ++ ": " ++ show n)
    pure n

-- | A list to pick from, and the choice standing.
pickOne :: String -> [String] -> Ui String
pickOne labelText options = keyed labelText $ do
    picked <- choiceOf labelText options first
    sample (stepper first picked)
  where
    first = case options of
        (o : _) -> o
        [] -> ""

-- | A slider between two numbers, and the number standing.
numberBox :: String -> Double -> Double -> Double -> Ui Double
numberBox labelText lo hi start = keyed labelText $ do
    moved <- numberSlider labelText lo hi start
    sample (stepper start moved)

-- | A switch, and whether it is on.
onOff :: String -> Bool -> Ui Bool
onOff labelText start = keyed labelText $ do
    flipped <- switch labelText start
    sample (stepper start flipped)

-- | A box to type in, and what stands in it.
typedText :: String -> String -> Ui String
typedText labelText start = keyed labelText $ do
    typed <- textField labelText start
    sample (stepper start typed)
