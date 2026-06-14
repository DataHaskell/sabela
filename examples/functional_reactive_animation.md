# Functional Reactive Animation

In 1997 Conal Elliott and Paul Hudak asked what an animation actually is.

The usual answer is a loop: sixty times a second, clear the screen, advance some
mutable state, redraw. That code is all about *how* to sample and render. It
buries the *what*: a ball that falls, a circle that pulses, a colour that
changes when you click.

Fran (Functional Reactive Animation) answered differently, with two abstractions
that have simple *continuous-time* meanings:

- A `Behavior a` is a value that varies continuously over time, semantically a
  function `Time -> a`. A moving position, a pulsing radius, a changing tint.
- An `Event a` is a stream of discrete occurrences, each a time paired with a
  value, semantically `[(Time, a)]`. A click, a key, a collision.

This notebook rebuilds the core of Fran from scratch, the actual denotational
model from the paper, and then *runs* it. We plot behaviours, integrate them
into physics, react to events with `switcher`, and render animated SVGs. About
40 lines of Haskell, plus pictures.

> One caveat. The list-based `Event` and the pull-sampled `Behavior` below are a
> faithful denotation, good for learning and finite demos, not a scalable
> reactive runtime. They show you the meaning, not the production machine.
> (Sabela also ships a batteries-included version of all this as the
> `Sabela.Notebook` modules; see `examples/frp-tutorial.md`. Here we build it
> ourselves, to understand it.)

First the imports. We use `granite` for the static plots and `text` for its
string type.

```haskell
-- cabal: build-depends: text, granite

import Granite.Svg
import qualified Data.Text as T
import Control.Applicative (liftA2)
import Data.List (intercalate)
import Text.Printf (printf)

-- Time is just a real number. The whole model rests on this one synonym.
type Time = Double
```

## 1. Behaviours: values that flow with time

The first abstraction is the behaviour, and its meaning is about as simple as it
gets: a behaviour is a function of time.

>  Behavior a   denotes   Time -> a

We make that denotation the literal representation. The field `at` is the
meaning function from the paper; it samples a behaviour at an instant. Two
primitive behaviours generate everything else: `time` (the clock) and
`constantB x` (pinned to `x` forever).

We keep the type and all its instances in one cell on purpose. In GHCi a type
and its instances must recompile together, or a later edit spawns a second,
incompatible `Behavior`.

```haskell
newtype Behavior a = Behavior { at :: Time -> a }

instance Functor Behavior where
    fmap f (Behavior b) = Behavior (f . b)

instance Applicative Behavior where
    pure                      = Behavior . const
    Behavior f <*> Behavior x = Behavior (\t -> f t (x t))

instance Num a => Num (Behavior a) where
    (+) = liftA2 (+) ; (-) = liftA2 (-) ; (*) = liftA2 (*)
    abs = fmap abs ; signum = fmap signum ; negate = fmap negate
    fromInteger = Behavior . const . fromInteger

instance Fractional a => Fractional (Behavior a) where
    (/) = liftA2 (/)
    fromRational = Behavior . const . fromRational

instance Floating a => Floating (Behavior a) where
    pi = Behavior (const pi)
    exp = fmap exp ; log = fmap log
    sin = fmap sin ; cos = fmap cos
    asin = fmap asin ; acos = fmap acos ; atan = fmap atan
    sinh = fmap sinh ; cosh = fmap cosh
    asinh = fmap asinh ; acosh = fmap acosh ; atanh = fmap atanh

time :: Behavior Time
time = Behavior id

constantB :: a -> Behavior a
constantB = pure
```

Sample a few by hand with `at`. The clock reads 2.5, a constant ignores the
clock, and `fmap` lifts a function over time:

```haskell
(at time 2.5, at (constantB "frozen") 99, at (fmap (*10) time) 4)
```

## 2. Lifting: ordinary arithmetic on time-varying values

This is the part that makes Fran click. `Behavior` is exactly the **reader
functor** `Time -> a`, so it is automatically `Functor` and `Applicative` (and,
categorically, `Representable` with `Time` as its index). The `Num`/`Fractional`/
`Floating` instances above lift each operation pointwise through that
`Applicative`: to add two behaviours, sample both at the same instant and add the
results.

The payoff: a behaviour is a first-class number. `2 * time + 1`, `sin time`,
`1 / (1 + time)` read like ordinary maths, and each one is a behaviour you sample
later. (The lifted laws are only as exact as `Double` itself, of course.)

```haskell
wiggle = sin (2 * pi * time)   -- a 1 Hz sine wave
ramp   = 2 * time + 1          -- a rising line
decay  = 1 / (1 + time)        -- a falling curve
```

Nothing has been drawn yet; these are just functions. Sample them:

```haskell
(at ramp 5, map (at wiggle) [0, 0.25, 0.5], at decay 9)
```

## 3. From meaning to pixels: sampling

A behaviour is defined at *every* real instant. To show one we finally do the
thing Fran works so hard to defer: sample it at a finite set of times. That's
the renderer's job, in one small function `sampleB`. The behaviours know nothing
about frame rates, so a different renderer could sample differently without
touching them.

```haskell
-- sampleB :: Time -> Time -> Int -> Behavior a -> [(Time, a)]
-- (No explicit Behavior in the signature: letting inference flow the type from
--  `at` keeps every helper pinned to the same Behavior, even after edits.)
sampleB t0 t1 n b = [ (t, at b t) | i <- [0 .. n], let t = t0 + (t1 - t0) * fromIntegral i / fromIntegral n ]

-- plotB :: String -> Time -> Time -> Int -> [(String, Behavior Double)] -> IO ()
plotB ttl t0 t1 n curves = displaySvg $ T.unpack $ lineGraph [ (T.pack nm, sampleB t0 t1 n b) | (nm, b) <- curves ] defPlot { plotTitle = T.pack ttl }
```

`wiggle`, sampled 200 times over three seconds and drawn:

```haskell
plotB "wiggle = sin (2*pi*time)" 0 3 200 [("wiggle", wiggle)]
```

## 4. Integration: behaviours that remember

Lifting gives us algebra on behaviours. Physics needs memory too: velocity is
the accumulated effect of acceleration, position the accumulated effect of
velocity. `integral` turns a behaviour into its running time-integral from 0.

Ours is the renderer's numerical version of the paper's exact one, a left
Riemann sum in small steps. It's simple and readable, *not* fast: each sample
re-adds from zero, so it's `O(t/dt)` per sample. Fine for learning and short
clips; a production version would carry a running total (an `accumB`/scan). We
build it from `fmap`/`time` rather than the `Behavior` constructor so it stays
pinned to the same `Behavior`.

```haskell
integral b = fmap (\t -> let { dt = 0.005 ; n = max 0 (floor (t / dt)) :: Int } in sum [ at b (fromIntegral i * dt) * dt | i <- [0 .. n - 1] ]) time
```

Now the equations of motion read like a physics textbook. A ball thrown straight
up at 15 m/s:

```haskell
gravity  = constantB 9.8
velocity = constantB 15 - integral gravity
height   = integral velocity
```

Height peaks where velocity crosses zero (the apex, ~1.53 s). No simulation
loop. We declared the relationships and let `at` evaluate them:

```haskell
plotB "Projectile: height and velocity vs time" 0 3 200 [("height", height), ("velocity", velocity)]
```

## 5. Animation: a behaviour you can watch

So far we've plotted behaviours. But the paper's title word is animation: a
behaviour of *pictures*, sampled fast enough that the eye sees motion.

`animateCircle` takes three behaviours (the x, y, and radius of a circle),
samples each over a time window, and emits an SVG whose `<animate>` elements
replay the samples in the browser, looping. The animation's meaning is still
just functions of time; the SVG is one particular sampling of it.

```haskell
-- Sample a Double-behaviour at n+1 instants over [0,dur] as a SMIL "v0;v1;...".
frameVals dur n b = intercalate ";" [ printf "%.2f" (at b (dur * fromIntegral i / fromIntegral n) :: Double) :: String | i <- [0 .. n] ]

-- One SMIL <animate> driving an attribute from the sampled values, looping.
animTag attr dur n b = concat ["<animate attributeName=\"", attr, "\" values=\"", frameVals dur n b, "\" dur=\"", show (dur :: Double), "s\" repeatCount=\"indefinite\" calcMode=\"linear\"/>"]
```

```haskell
-- animateCircle w h dur n fill bx by br: an SVG circle whose centre (bx,by) and
-- radius (br) are the given behaviours, sampled over [0,dur] and looped forever.
animateCircle w h dur n fill bx by br = displayHtml $ concat [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"", show (w :: Int), "\" height=\"", show (h :: Int), "\" viewBox=\"0 0 ", show w, " ", show h, "\">", "<rect width=\"100%\" height=\"100%\" fill=\"#0b1021\"/>", "<circle fill=\"", fill, "\">", animTag "cx" dur n bx, animTag "cy" dur n by, animTag "r" dur n br, "</circle></svg>" ]
```

Breathing: a fixed centre, radius `40 + 25 sin t`, one breath per 3 s loop:

```haskell
animateCircle 320 200 3 90 "#7dd3fc" (constantB 160) (constantB 100) (40 + 25 * sin (2 * pi * time / 3))
```

Uniform circular motion: the centre traces `(cos t, sin t)`, a 4 s loop:

```haskell
animateCircle 320 200 4 120 "#fca5a5" (constantB 160 + 80 * cos (2 * pi * time / 4)) (constantB 100 + 70 * sin (2 * pi * time / 4)) (constantB 14)
```

## 6. Events and reactivity

Behaviours flow smoothly, but real interaction is discrete: a click, a key, a
collision. Fran's second type, `Event a`, is a stream of timed occurrences,
semantically `[(Time, a)]`. It's a `Functor` over its payload and a `Monoid`
under time-ordered merge (left-biased when two occurrences share an instant). It
is deliberately *not* `Applicative`/`Monad`: there's no causal way to combine two
independent streams' values.

Events earn their keep through `switcher`. `switcher b evs` behaves as `b` until
an occurrence delivers a new behaviour, then switches to it, and again at the
next occurrence. At the switch instant the new behaviour is already in force.
`untilB` is the one-shot version. (Both assume the occurrence list is in time
order; `last`/`head` pick by list position.)

```haskell
newtype Event a = Event { occs :: [(Time, a)] }

-- a single occurrence at time te
occurAt te x = Event [(te, x)]

-- start as b0, then switch to the latest behaviour whose time has passed
switcher b0 evs = fmap (\t -> at (pick t) t) time where pick t = last (b0 : [ b | (te, b) <- occs evs, te <= t ])

-- switch once, on the first occurrence, then stay
untilB b0 evs = fmap (\t -> at (pick t) t) time where pick t = case [ b | (te, b) <- occs evs, te <= t ] of { (b : _) -> b ; [] -> b0 }
```

A reactive behaviour: constant 0, then switch to 5 at t=2, then to `ramp` at t=4:

```haskell
plotB "switcher: 0, then 5 at t=2, then ramp at t=4" 0 6 300 [("reactive", switcher (constantB 0) (Event [(2, constantB 5), (4, ramp)]))]
```

## 7. Capstone: the bouncing ball

The bouncing ball is FRP's *hello, world*, and now we have every piece. Between
bounces the ball is in free fall: a single parabola (`arc`). At each bounce the
velocity flips and loses energy, a discrete event, so we use `switcher` to hand
the ball a fresh parabola each time it hits the floor.

We use closed-form arcs (not `integral`) for each segment so the motion is exact
and cheap, and switch between them on the computed bounce times. Continuous
physics and discrete reaction, in one declarative expression:

```haskell
g  = 9.8 :: Double
restitution = 0.7 :: Double
h0 = 1.0 :: Double

-- one parabolic free-fall arc, from time t0, height y0, up-velocity v0
arc t0 y0 v0 = constantB y0 + constantB v0 * (time - constantB t0) - constantB (0.5 * g) * (time - constantB t0) * (time - constantB t0)

-- the bounce schedule: (impactTime, height 0, rebound velocity), damped each bounce
bouncesFrom t v = (t, 0, v) : bouncesFrom (t + 2 * v / g) (restitution * v)
segments = take 8 ((0, h0, 0) : bouncesFrom (sqrt (2 * h0 / g)) (restitution * g * sqrt (2 * h0 / g)))

-- reactive height: start with the drop, switch to a fresh arc at each bounce, clamp at the floor
ballHeight = fmap (max 0) (switcher (arc 0 h0 0) (Event [ (t, arc t y v) | (t, y, v) <- drop 1 segments ]))
```

The classic decaying-bounce profile:

```haskell
plotB "Bouncing ball: height vs time" 0 3 500 [("height", ballHeight)]
```

…and the same behaviour animated, `cy = ground - scale * height`. Watch it
bounce:

```haskell
animateCircle 320 220 3 150 "#fde047" (constantB 160) (constantB 200 - constantB 160 * ballHeight) (constantB 12)
```

## 8. Why this mattered

Look back at what we didn't write: no game loop, no `setInterval`, no mutable
frame counter, no manual redraw. We wrote behaviours as functions of continuous
time, lifted ordinary maths over them, integrated them into physics, and reacted
to discrete events with `switcher`. A thin renderer sampled the result.

That separation of *what* from *when to sample* is Fran's lasting contribution.
The continuous-time, denotational view spread well beyond Haskell:

- Yampa and reactive-banana - direct Haskell descendants, refining Fran's
  semantics (and fixing its space/time leaks).
- Elm - began as FRP, then dropped it in 0.17 (2016) for The Elm Architecture.
- React, RxJS, Solid, Svelte - "UI as a function of state over time" grew from
  the same root, but to be precise these are *reactive* libraries, not FRP in
  Elliott's continuous-time, denotational sense.

Elliott later refined the model (*Push-Pull Functional Reactive Programming*,
2009) to make events efficient. But the core insight you just rebuilt (a
behaviour is a function of time; an animation is a behaviour of pictures) is
unchanged.

*Further reading:* Elliott & Hudak, "Functional Reactive Animation," ICFP 1997;
Conal Elliott, "Push-Pull Functional Reactive Programming," Haskell Symposium 2009.
