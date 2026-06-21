{- |
Module      : Sabela.Notebook.Event
Description : Things that happen at particular moments (the other half of FRP).

= Behaviours vs. events

A 'Sabela.Notebook.Behavior.Behavior' always has a value (the temperature is
/always/ something). An __event__ is different: it is a list of __occurrences__,
each one a moment in time paired with a value. Nothing happens between
occurrences.

> a button being clicked, a key press, a sensor reading arriving, a ball bouncing

In this library an event is literally a time-ordered list:

> Event a   ==   [(Time, a)]      -- (when it happened, what it carried)

= A quick tour

>>> occurrencesOf (eventFromList [(1, "a"), (3, "b")])
[(1.0,"a"),(3.0,"b")]
>>> occurrencesOf (fmap reverse (eventFromList [(1,"ab")]))     -- map over the values
[(1.0,"ba")]
>>> occurrencesOf (merge (eventFromList [(1,'a')]) (eventFromList [(2,'b')]))
[(1.0,'a'),(2.0,'b')]

= Turning events into behaviours

Events and behaviours are two views of the same world, and you move between them:

* 'stepper' — \"hold\" the last value an event delivered, giving a behaviour.
* 'switcher' — /switch/ which behaviour is in force whenever an event fires.
* 'snapshot' \/ 'tag' — peek at a behaviour's value at the moment an event fires.

= A note on simultaneous events

If two occurrences share the exact same time, 'merge' keeps the left one first.
This library does not promise \"glitch-free\" simultaneity the way a production
reactive system (reactive-banana, sodium) does — it is a small, honest teaching
model, not a real-time runtime.
-}
module Sabela.Notebook.Event (
    -- * The core type
    Event (..),
    eventFromList,

    -- * Building and combining events
    never,
    merge,
    filterE,
    mapE,

    -- * Running totals
    accumE,
    scanlE,
    countE,

    -- * Events to behaviours
    stepper,
    switcher,
    accumB,

    -- * Reading a behaviour when an event fires
    snapshot,
    tag,
) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Sabela.Notebook.Behavior (Behavior (..), Time)

{- | A stream of things that happen at particular moments, in time order.

The record field 'occurrencesOf' gives you the underlying @[(Time, value)]@ list.
Build events with 'eventFromList' (which sorts for you) rather than the
constructor.
-}
newtype Event a = Event
    { occurrencesOf :: [(Time, a)]
    -- ^ The occurrences, earliest first.
    }

{- | Build an event from a list of @(time, value)@ pairs. The list is sorted into
time order for you, so you don't have to be careful about ordering.

>>> occurrencesOf (eventFromList [(3,'b'), (1,'a')])
[(1.0,'a'),(3.0,'b')]
-}
eventFromList :: [(Time, a)] -> Event a
eventFromList = Event . sortBy (comparing fst)

-- | The event that never happens.
never :: Event a
never = Event []

{- | Merge two events into one, keeping everything in time order. If two
occurrences happen at the exact same time, the one from the left event comes
first.

>>> occurrencesOf (merge (eventFromList [(1,'a'),(3,'c')]) (eventFromList [(2,'b')]))
[(1.0,'a'),(2.0,'b'),(3.0,'c')]
-}
merge :: Event a -> Event a -> Event a
merge (Event xs) (Event ys) = Event (go xs ys)
  where
    go [] bs = bs
    go as [] = as
    go (a : as) (b : bs)
        | fst a <= fst b = a : go as (b : bs)
        | otherwise = b : go (a : as) bs

{- | Keep only the occurrences whose value passes a test.

>>> occurrencesOf (filterE even (eventFromList [(1,1),(2,2),(3,3),(4,4)]))
[(2.0,2),(4.0,4)]
-}
filterE :: (a -> Bool) -> Event a -> Event a
filterE p (Event xs) = Event [o | o@(_, x) <- xs, p x]

{- | Change every occurrence's value with a function. This is exactly 'fmap'; the
name is here so beginners have an obvious word for it.

>>> occurrencesOf (mapE (+10) (eventFromList [(1,1),(2,2)]))
[(11.0,11),(12.0,12)]
-}
mapE :: (a -> b) -> Event a -> Event b
mapE = fmap

{- | Keep a running result as occurrences arrive: start from a seed, and for each
occurrence combine the running result with the new value, emitting the updated
result. This is @scanl@ for events — the workhorse behind counters, running
totals, and live averages.

>>> occurrencesOf (scanlE (+) 0 (eventFromList [(1,10),(2,20),(3,5)]))
[(1.0,10),(2.0,30),(3.0,35)]
-}
scanlE :: (b -> a -> b) -> b -> Event a -> Event b
scanlE f z (Event xs) = Event (go z xs)
  where
    go _ [] = []
    go acc ((t, x) : rest) = let acc' = f acc x in (t, acc') : go acc' rest

{- | Apply a stream of /update functions/ to a starting value, emitting each new
value. Like 'scanlE' but each occurrence already carries \"how to change the
state\".

>>> occurrencesOf (accumE 0 (eventFromList [(1,(+1)),(2,(*10))]))
[(1.0,1),(2.0,10)]
-}
accumE :: a -> Event (a -> a) -> Event a
accumE z = scanlE (\acc f -> f acc) z

{- | Count how many times an event has fired so far.

>>> occurrencesOf (countE (eventFromList [(1,'a'),(2,'b'),(3,'c')]))
[(1.0,1),(2.0,2),(3.0,3)]
-}
countE :: Event a -> Event Int
countE = scanlE (\n _ -> n + 1) 0

{- | \"Hold\" the most recent value an event delivered, as a behaviour. Before the
first occurrence the behaviour is the given starting value.

>>> let b = stepper 0 (eventFromList [(1,10),(3,20)])
>>> map (at b) [0,2,4]
[0,10,20]
-}
stepper :: a -> Event a -> Behavior a
stepper x0 (Event xs) = Behavior pick
  where
    pick t = case [v | (te, v) <- xs, te <= t] of
        [] -> x0
        vs -> last vs

{- | A running total as a /behaviour/: like 'accumE' but you can sample it at any
time. (It is 'stepper' on top of 'accumE'.)
-}
accumB :: a -> Event (a -> a) -> Behavior a
accumB z e = stepper z (accumE z e)

{- | Switch which behaviour is \"in force\" whenever the event fires. Start with
@b0@; each occurrence hands over a /new behaviour/ to follow from that moment on.

This is how an animation /reacts/: e.g. a ball follows one arc, then a bounce
event switches it to the next arc.

>>> let b = switcher (always 0) (eventFromList [(1, always 5), (2, always 9)])
>>> map (at b) [0, 1.5, 3]
[0,5,9]

At the exact instant of an occurrence the new behaviour is already in force.
-}
switcher :: Behavior a -> Event (Behavior a) -> Behavior a
switcher b0 (Event xs) = Behavior pick
  where
    pick t = case [b | (te, b) <- xs, te <= t] of
        [] -> at b0 t
        bs -> at (last bs) t

{- | When the event fires, read the behaviour's value at that instant and combine
the two with your function.

>>> let b = stepper 0 (eventFromList [(0,100),(2,200)])
>>> occurrencesOf (snapshot (,) b (eventFromList [(1,'a'),(3,'b')]))
[(1.0,(100,'a')),(3.0,(200,'b'))]
-}
snapshot :: (a -> b -> c) -> Behavior a -> Event b -> Event c
snapshot f b (Event xs) = Event [(t, f (at b t) x) | (t, x) <- xs]

{- | Replace each occurrence's value with the behaviour's value at that moment
(ignoring what the event carried). A handy special case of 'snapshot'.

>>> let b = stepper 0 (eventFromList [(0,100),(2,200)])
>>> occurrencesOf (tag b (eventFromList [(1,()),(3,())]))
[(1.0,100),(3.0,200)]
-}
tag :: Behavior a -> Event b -> Event a
tag = snapshot const

-- | Map over the values an event carries (same as 'mapE').
instance Functor Event where
    fmap f (Event xs) = Event [(t, f x) | (t, x) <- xs]

{- | Two events combine by 'merge'; the empty event is 'never'. (There is
deliberately no @Applicative@\/@Monad@ for @Event@: there is no sensible,
cause-respecting way to combine the /values/ of two independent streams.)
-}
instance Semigroup (Event a) where
    (<>) = merge

instance Monoid (Event a) where
    mempty = never
