{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Sabela.Notebook.Behavior
Description : Values that change over time (the heart of FRP).

= What is a behaviour?

A __behaviour__ is a value that changes as time passes. Think of it as a tiny
machine: you ask it "what is your value at time @t@?" and it answers.

> position, brightness, the temperature outside, the hands of a clock

are all behaviours. In this library a behaviour is /literally/ a function from
time to a value:

> Behavior a   ==   (the current time)  ->  (a value of type a)

You never write that function by hand. Instead you start from two ready-made
behaviours — 'time' and 'always' — and combine them with ordinary arithmetic.

= A 30-second tour

>>> at time 2.0            -- the clock: at time 2, the value is 2
2.0
>>> at (always 7) 99       -- a constant: ignores the clock, always 7
7
>>> at (2 * time + 1) 5    -- yes, you can do maths on behaviours!
11.0
>>> at (sin time) 0        -- and call sin, cos, sqrt, ...
0.0

The last two lines are the magic of FRP: @2 * time + 1@ looks like ordinary
maths, but it describes a value that /moves/. We only find out the actual number
later, by 'at'-sampling it at a chosen time.

= How to read the types

* @'Behavior' a@  — a value of type @a@ that varies over time.
* @'Time'@        — just a number (seconds), so @Behavior Double@ is the common case.
* @'at' b t@      — \"look at behaviour @b@ at time @t@\"; gives you a plain @a@.
-}
module Sabela.Notebook.Behavior (
    -- * The core type
    Time,
    Behavior (..),

    -- * The two starting behaviours
    time,
    always,

    -- * Looking at a behaviour
    sampleBetween,
) where

-- 'liftA2' comes from the Prelude (base ≥ 4.18 / GHC ≥ 9.6).

-- | Time is measured in seconds, as an ordinary number.
type Time = Double

{- | A value that changes over time.

Under the hood a @Behavior a@ is just a function @'Time' -> a@: give it a time
and it gives you the value at that instant. The record field 'at' /is/ that
function, so @'at' b t@ asks behaviour @b@ for its value at time @t@.

You rarely build one with the constructor directly — combine 'time', 'always',
and arithmetic instead.
-}
newtype Behavior a = Behavior
    { at :: Time -> a
    -- ^ @at b t@ — the value of behaviour @b@ at time @t@.
    }

{- | The clock. Its value at any time is that time itself.

>>> at time 3.5
3.5

Everything that moves is, ultimately, built from @time@.
-}
time :: Behavior Time
time = Behavior id

{- | A behaviour that never changes — it ignores the clock and is always @x@.

>>> at (always "hello") 42
"hello"
-}
always :: a -> Behavior a
always x = Behavior (const x)

{- | Take @n+1@ evenly spaced snapshots of a behaviour between two times.

Handy for plotting or just peeking at how a value moves.

>>> sampleBetween 0 4 4 time
[(0.0,0.0),(1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0)]
-}
sampleBetween :: Time -> Time -> Int -> Behavior a -> [(Time, a)]
sampleBetween t0 t1 n b =
    [ (t, at b t)
    | i <- [0 .. n]
    , let t = t0 + (t1 - t0) * fromIntegral i / fromIntegral n
    ]

{- $instances
The instances below are what let you write @2 * time + 1@ or @sin time@. Each one
just does the obvious thing /at every instant/: to add two behaviours, sample
both at the same time and add the results.
-}

-- | Apply a function to a behaviour's value at every instant.
instance Functor Behavior where
    fmap :: (a -> b) -> Behavior a -> Behavior b
    fmap f (Behavior g) = Behavior (f . g)

{- | Combine behaviours pointwise. @'pure' x@ is the constant behaviour (same as
'always'); @bf '<*>' bx@ samples both at the same instant and applies one to the
other. This is what makes @liftA2 (+)@ — i.e. @(+)@ on behaviours — work.
-}
instance Applicative Behavior where
    pure :: a -> Behavior a
    pure = always

    (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b
    Behavior f <*> Behavior x = Behavior (\t -> f t (x t))

{- | Arithmetic on behaviours happens /pointwise/: @a + b@ is the behaviour whose
value at time @t@ is @at a t + at b t@. So you can treat a @Behavior Double@ like
an ordinary number.

Note: these laws are only as exact as 'Double' itself (which has its own quirks
like @NaN@), but for everyday animation maths they behave just as you'd expect.
-}
instance (Num a) => Num (Behavior a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = always . fromInteger

-- | Division (and fractional literals) on behaviours, pointwise.
instance (Fractional a) => Fractional (Behavior a) where
    (/) = liftA2 (/)
    fromRational = always . fromRational

{- | @sin@, @cos@, @sqrt@, @exp@, @pi@ … on behaviours, pointwise. This is what
lets @sin time@ describe a wave.
-}
instance (Floating a) => Floating (Behavior a) where
    pi = always pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh
