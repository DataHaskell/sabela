{- |
Module      : Sabela.Notebook.Frp
Description : One import for Functional Reactive Programming in a Sabela notebook.

This is the friendly front door. @import Sabela.Notebook.Frp@ and you get
everything you need to describe values that change over time:

* 'Behavior' — a value that varies with the clock ('time'), which you can do
  ordinary maths on (@2 * time@, @sin time@).
* 'Event' — things that happen at particular moments.
* combinators to connect the two ('stepper', 'switcher', 'snapshot', …).

= The shortest possible example

>>> let wave = sin time          -- a behaviour: a sine wave
>>> map (at wave) [0, pi/2, pi]  -- look at it at three instants
[0.0,1.0,1.2246...e-16]

= Where to go next

Read 'Sabela.Notebook.Behavior' to understand behaviours, then
'Sabela.Notebook.Event' for events. Both are written for people new to
programming, with runnable examples on every function.
-}
module Sabela.Notebook.Frp (
    -- * Values that change over time
    module Sabela.Notebook.Behavior,

    -- * Things that happen at moments
    module Sabela.Notebook.Event,

    -- * Calculus on behaviours
    integral,
    integralFrom,
    derivative,
) where

import Sabela.Notebook.Behavior
import Sabela.Notebook.Event

{- | The running total of a behaviour over time — its /area so far/, measured from
time @0@.

If a behaviour is a speed, its @integral@ is the distance travelled. If it is an
acceleration, the @integral@ is the speed. Integrate twice and you get position
from gravity — that is how the bouncing-ball demo works.

>>> let speed = always 2        -- moving at a constant 2 units/second
>>> map (at (integral speed)) [0, 1, 2, 3]
[0.0,2.0,4.0,6.0]

Note: this is a simple, easy-to-read approximation (it adds up small slices of
size @dt = 0.005@ from @0@ up to the time you ask about). It is perfect for
learning and for short animations. It is /not/ fast for very large times — a
production version would keep a running total instead of re-adding from zero each
time. We keep the simple version here precisely so you can read and understand it.
-}
integral :: Behavior Double -> Behavior Double
integral = integralFrom 0

{- | Like 'integral', but start adding up from a chosen moment @t0@ instead of
from time @0@.

This matters when you combine integration with 'switcher'. When a ball bounces,
you switch to a /new/ motion that should start accumulating from the bounce
instant — not from time zero. Use @integralFrom tBounce@ for the new piece so its
running total restarts at the right moment.

>>> map (at (integralFrom 2 (always 2))) [2, 3, 4]   -- area only counts from t=2
[0.0,2.0,4.0]
-}
integralFrom :: Time -> Behavior Double -> Behavior Double
integralFrom t0 b = Behavior area
  where
    dt = 0.005
    area t =
        let n = max 0 (floor ((t - t0) / dt)) :: Int
         in sum [at b (t0 + fromIntegral i * dt) * dt | i <- [0 .. n - 1]]

{- | The /rate of change/ of a behaviour — the opposite of 'integral'. If a
behaviour is a position, its 'derivative' is the speed.

>>> map (round . at (derivative (2 * time))) [0, 1, 2]   -- slope of 2*t is 2
[2,2,2]
-}
derivative :: Behavior Double -> Behavior Double
derivative b = Behavior (\t -> (at b (t + h) - at b (t - h)) / (2 * h))
  where
    h = 0.001
