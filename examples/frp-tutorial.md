# Functional Reactive Programming in Sabela

Creating programs that use and produce multimedia requires reasoning about
when things happen and why they happen. For example, a game is a series of
intricate rules that govern what happens when a character moves forward,
collects a valuable item, or interacts with another character.
Writing this kind of code gets complicated quickly since we think of programming
as the process of writing sequential computer instructions. All the logic
in a game is typically written into a large loop that, at every iteration,
checks if conditions of interest are met.

Functional reactive programming promises to make writing such code clearer.

It does this by describing the things that change over time, and the events that
happen, as ordinary values. Instead of maintaining the loop yourself, you write
down how each value relates to time and let the runtime do the sampling.

The model was proposed by Conal Elliott and Paul Hudak when they asked what an
animation actually is, and answered with two abstractions: behaviours and events.

A *behaviour* is a value that varies continuously over time.
An *event* is something that happens at a moment.
Their paper, Functional Reactive Animation, is the root that Yampa,
reactive-banana, and (more loosely) React and Elm grew from.

This notebook is that model. It ships with every Sabela notebook, so there is
nothing to install, and it assumes no Haskell or FRP going in. Run each cell and
watch.

## The north star

Here is the destination: a blue circle that slides up and down, in one line.

```haskell
animate 3 (\t -> fill blue (circle (150, 150 + 80 * sin t) 30))
```

It will not run yet, because nothing has been imported. The rest of the notebook
works out what `animate`, `time`, and a picture are. Import first.

```haskell
import Sabela.Notebook
```

The circle above moves now. Behaviours, pictures, animation, in that order.

## 1. Behaviours: values that flow with time

Underneath, a behaviour is a function from time to a value, but you rarely write
that function yourself. You start instead from two behaviours that are given to
you: `time`, which is the clock, and `always x`, which holds the value `x` for all
time. Every other behaviour is built from these with ordinary arithmetic, and when
you want to know what one of them is at a particular instant, you sample it with
`at`.

```haskell
(at time 2.5, at (always 7) 99, at (2 * time + 1) 5)
```

The expression `2 * time + 1` looks like ordinary arithmetic, which is the entire point.
Because `time` is a behaviour, the whole expression becomes a behaviour
as well: a value that changes as the clock advances. Functions like `sin`, `cos`,
and `sqrt` carry over in the same way, so `sin time` describes a wave. None of it
is computed until you sample it at some instant.

## 2. Drawing pictures

Before anything can move it first has to be drawn. A picture is a recipe for a drawing,
so building one paints nothing on its own. To actually see it, you hand it to `picture`.

```haskell
picture (fill red (circle (150, 150) 80))
```

Coordinates start at the top-left: `x` grows right, `y` grows down. Stack shapes
with `<>` (later ones draw on top), recolour with `fill`, move them with
`translate`.

```haskell
picture (fill blue (rectangle (40, 60) 220 120) <> translate (150, 120) (fill white (circle (0, 0) 30)))
```

There are more shapes (`line`, `polygon`, `text`) and more ways to dress them up
(`stroke`, `scale`, `rotate`). The `Sabela.Notebook.Picture` docs list them.

## 3. Animation: a picture that depends on time

Putting behaviours and pictures together gives us the north star from the start of
the notebook. An animation is just a picture that depends on time: you write a
function from a time to a picture, hand it to `animate`, and say how many seconds
it should run for.

```haskell
animate 3 (\t -> fill blue (circle (150, 150 + 80 * sin t) 30))
```

Behind the scenes Sabela samples thirty pictures a second and plays them in the
browser like a flip-book. Haskell itself is not running during playback, which is
why the animation stays smooth.

Sometimes it is clearer to build the motion as a behaviour first and animate that,
which is what `animateB` is for: it plays a behaviour of pictures. Here the radius
breathes in and out.

```haskell
animateB 3 (fmap (\r -> fill orange (circle (150, 150) r)) (40 + 20 * sin (2 * time)))
```

## 4. Integration: a bit of physics

The `integral` of a behaviour is its running total over time. When the behaviour
is a speed, its integral is the distance travelled, and that is already enough to
model gravity. In the cell below the upward speed starts at 180 and falls
steadily, so the ball rises, slows, and then drops back.

```haskell
animateB 2 (fmap (\h -> fill green (circle (150, 250 - h) 20)) (integral (180 - 180 * time)))
```

Notice that there is no simulation loop anywhere here. The relationship between
speed and height is stated once, and the renderer takes care of sampling it as
time runs on.

## 5. Events and reactivity

Where behaviours flow continuously, events are discrete and happen at particular
moments. An `Event` is a list of those moments paired with values, which you build
with `eventFromList`. The combinator that does the most work here is `switcher`,
which follows one behaviour until an event arrives and hands it a new behaviour to
follow from then on. In the cell below a blue circle on the left switches to a red
one on the right at `t = 2`.

```haskell
animateB 4 (switcher (always (fill blue (circle (80, 150) 30))) (eventFromList [(2, always (fill red (circle (220, 150) 30)))]))
```

The `Event` and `Behavior` used here are a small teaching model, deliberately
simple enough to read through in one sitting. A performance-sensitive interface
would reach for push-based machinery underneath, though it exposes the same
vocabulary you are learning now. Alongside `switcher` you will find `merge` for
combining streams, `filterE` for keeping only some events, `stepper` for holding
on to the latest value, and `snapshot` and `tag` for reading a behaviour at the
moment an event fires.

## 6. Widgets: interaction

Events need not be written out by hand. A widget is an on-screen control that
produces them as you interact with it, and calling `display` shows the widget and
returns its current value. Try dragging the slider below.

```haskell
display (slider "size" 50 10 120)
```

A widget is only useful when it drives something. Whenever you change it, Sabela
re-runs the cells that depend on it. In the next cell the slider's value becomes
the size of a circle, so moving the slider resizes the circle straight away.

```haskell
display (slider "size" 50 10 120) >>= \s -> picture (fill purple (circle (150, 150) (fromIntegral s)))
```

There is also `dropdown`, `checkbox`, `textInput`, and `button`.

## 7. Data streams

The same vocabulary extends to streaming data. You treat the incoming numbers as
an `Event`, keep a running total of them with `scanlE`, and plot the result with
`lineChart`.

```haskell
picture (lineChart defaultCanvas (occurrencesOf (scanlE (+) 0 (eventFromList (zip [1, 2, 3, 4, 5, 6] [3, 1, 4, 1, 5, 9])))))
```

Here `scanlE (+) 0` keeps the running sum, `occurrencesOf` turns the event back
into `(time, value)` points, and `lineChart` scales those points onto the canvas.
The very same `scanlE` that drives this small counter would drive a live dashboard
fed by real data.

## Where to go next

That is the whole vocabulary. Behaviours are values that vary over time, events
are things that happen at particular moments, pictures are drawings, and a handful
of combinators join them together. What makes it hold together is that there is
really only one idea underneath: `at`, `integral`, `switcher`, `animate`, and
`lineChart` are all ways of working with a value that depends on time, which is why
a counter, a falling ball, and a live chart turn out to be the same program with
different parts slotted in. In each case you describe how time relates to what is
drawn and leave the sampling to the renderer, so there is no loop to get wrong and
playback stays smooth while Haskell sits idle. It's a simplified version of
Elliott and Hudak's original model.
