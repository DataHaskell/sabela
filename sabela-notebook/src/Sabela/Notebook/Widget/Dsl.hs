{-# LANGUAGE ScopedTypeVariables #-}

-- | The words a widget is written in.
--
-- Every control hands back an 'Event': the times the reader used it, and what
-- they left behind. Turn that into a value with the FRP combinators, then draw
-- with 'say' or 'paint'.
--
-- > tally :: Ui ()
-- > tally = do
-- >   up   <- pushButton "+"
-- >   down <- pushButton "-"
-- >   n    <- sample (accumB (0 :: Int) (merge (fmap (const (+ 1)) up) (fmap (const (subtract 1)) down)))
-- >   say ("count: " ++ show n)
--
-- The names avoid @button@, @slider@, @dropdown@, @checkbox@ and @textInput@,
-- which Sabela defines in every session and which would shadow an import.
module Sabela.Notebook.Widget.Dsl (
    say,
    paint,
    paintOn,
    pushButton,
    textField,
    numberSlider,
    switch,
    choiceOf,
    across,
    down,
    keyed,
    now,
    sample,
    numbersOf,
    latest,
) where

import Data.Maybe (mapMaybe)
import Sabela.Notebook.Behavior (Behavior, Time, at)
import Sabela.Notebook.Event (Event, eventFromList, mapE, occurrencesOf)
import Sabela.Notebook.Picture (Canvas, Picture, defaultCanvas)
import Sabela.Notebook.Widget.Free (hoistFree, liftF)
import Sabela.Notebook.Widget.Types (
    Control (..),
    ControlKind (..),
    Layout (..),
    Ui,
    UiF (..),
 )

-- | A line of text.
say :: String -> Ui ()
say s = liftF (Say s ())

-- | A drawing, at the default size. Charts from "Sabela.Notebook.Chart" go here.
paint :: Picture -> Ui ()
paint = paintOn defaultCanvas

-- | A drawing at a size you choose.
paintOn :: Canvas -> Picture -> Ui ()
paintOn canvas picture = liftF (Draw canvas picture ())

-- | A button. It occurs each time the reader presses it, carrying nothing.
pushButton :: String -> Ui (Event ())
pushButton labelText =
    fmap (mapE (const ())) (control labelText Press)

-- | A box to type in. It occurs with the text as it stands.
textField :: String -> String -> Ui (Event String)
textField labelText initial = control labelText (Typing initial)

-- | A slider between two numbers. It occurs with the number chosen.
numberSlider :: String -> Double -> Double -> Double -> Ui (Event Double)
numberSlider labelText lo hi initial =
    fmap numbersOf (control labelText (Sliding lo hi initial))

-- | An on/off switch.
switch :: String -> Bool -> Ui (Event Bool)
switch labelText initial =
    fmap (mapE (== "True")) (control labelText (Switching initial))

-- | A list to pick from. It occurs with the choice.
choiceOf :: String -> [String] -> String -> Ui (Event String)
choiceOf labelText options initial =
    control labelText (Choosing options initial)

-- | Lays its contents out side by side.
across :: Ui a -> Ui a
across = wrapped Across

-- | Lays its contents out one above the next.
down :: Ui a -> Ui a
down = wrapped Down

wrapped :: Layout -> Ui a -> Ui a
wrapped layout inner = do
    liftF (Open layout ())
    a <- inner
    liftF (Close ())
    pure a

-- | Gives every control inside a name of its own, so that adding or hiding a
-- neighbour cannot make one forget what the reader did. Worth doing whenever
-- two controls share a label, or a control appears only sometimes.
keyed :: String -> Ui a -> Ui a
keyed prefix = hoistFree scope
  where
    scope :: UiF x -> UiF x
    scope (Ask spec k) = Ask spec{controlKey = prefix ++ "/" ++ controlKey spec} k
    scope other = other

-- | The moment of the reader's last interaction. Behaviours are read here.
now :: Ui Time
now = liftF (Now id)

-- | Reads a behaviour at 'now'.
sample :: Behavior a -> Ui a
sample b = fmap (at b) now

-- | Keeps only the occurrences that are numbers.
numbersOf :: Event String -> Event Double
numbersOf = eventFromList . mapMaybe readNumber . occurrencesOf
  where
    readNumber (t, s) = case reads s of
        [(v, "")] -> Just (t, v)
        _ -> Nothing

-- | The most recent occurrence, or a fallback if there has not been one.
latest :: a -> Event a -> a
latest fallback e = case occurrencesOf e of
    [] -> fallback
    xs -> snd (last xs)

control :: String -> ControlKind -> Ui (Event String)
control labelText kind =
    liftF (Ask (Control{controlKey = labelText, controlLabel = labelText, controlKind = kind}) id)
