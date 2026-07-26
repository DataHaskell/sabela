{-# LANGUAGE InstanceSigs #-}

module Sabela.Notebook.Behavior (
    Time,
    Behavior (..),
    time,
    always,
    sampleBetween,
) where

type Time = Double

newtype Behavior a = Behavior
    { at :: Time -> a
    }

time :: Behavior Time
time = Behavior id

always :: a -> Behavior a
always x = Behavior (const x)

sampleBetween :: Time -> Time -> Int -> Behavior a -> [(Time, a)]
sampleBetween t0 t1 n b =
    [ (t, at b t)
    | i <- [0 .. n]
    , let t = t0 + (t1 - t0) * fromIntegral i / fromIntegral n
    ]

instance Functor Behavior where
    fmap :: (a -> b) -> Behavior a -> Behavior b
    fmap f (Behavior g) = Behavior (f . g)

instance Applicative Behavior where
    pure :: a -> Behavior a
    pure = always

    (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b
    Behavior f <*> Behavior x = Behavior (\t -> f t (x t))

instance (Num a) => Num (Behavior a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = always . fromInteger

instance (Fractional a) => Fractional (Behavior a) where
    (/) = liftA2 (/)
    fromRational = always . fromRational

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
