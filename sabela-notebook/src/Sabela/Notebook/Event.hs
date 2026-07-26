module Sabela.Notebook.Event (
    Event (..),
    eventFromList,
    never,
    merge,
    filterE,
    mapE,
    accumE,
    scanlE,
    countE,
    stepper,
    switcher,
    accumB,
    snapshot,
    tag,
) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Sabela.Notebook.Behavior (Behavior (..), Time)

newtype Event a = Event
    { occurrencesOf :: [(Time, a)]
    }

eventFromList :: [(Time, a)] -> Event a
eventFromList = Event . sortBy (comparing fst)

never :: Event a
never = Event []

merge :: Event a -> Event a -> Event a
merge (Event xs) (Event ys) = Event (go xs ys)
  where
    go [] bs = bs
    go as [] = as
    go (a : as) (b : bs)
        | fst a <= fst b = a : go as (b : bs)
        | otherwise = b : go (a : as) bs

filterE :: (a -> Bool) -> Event a -> Event a
filterE p (Event xs) = Event [o | o@(_, x) <- xs, p x]

mapE :: (a -> b) -> Event a -> Event b
mapE = fmap

scanlE :: (b -> a -> b) -> b -> Event a -> Event b
scanlE f z (Event xs) = Event (go z xs)
  where
    go _ [] = []
    go acc ((t, x) : rest) = let acc' = f acc x in (t, acc') : go acc' rest

accumE :: a -> Event (a -> a) -> Event a
accumE z = scanlE (\acc f -> f acc) z

countE :: Event a -> Event Int
countE = scanlE (\n _ -> n + 1) 0

stepper :: a -> Event a -> Behavior a
stepper x0 (Event xs) = Behavior pick
  where
    pick t = case [v | (te, v) <- xs, te <= t] of
        [] -> x0
        vs -> last vs

accumB :: a -> Event (a -> a) -> Behavior a
accumB z e = stepper z (accumE z e)

switcher :: Behavior a -> Event (Behavior a) -> Behavior a
switcher b0 (Event xs) = Behavior pick
  where
    pick t = case [b | (te, b) <- xs, te <= t] of
        [] -> at b0 t
        bs -> at (last bs) t

snapshot :: (a -> b -> c) -> Behavior a -> Event b -> Event c
snapshot f b (Event xs) = Event [(t, f (at b t) x) | (t, x) <- xs]

tag :: Behavior a -> Event b -> Event a
tag = snapshot const

instance Functor Event where
    fmap f (Event xs) = Event [(t, f x) | (t, x) <- xs]

instance Semigroup (Event a) where
    (<>) = merge

instance Monoid (Event a) where
    mempty = never
