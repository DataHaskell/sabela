{-# LANGUAGE RankNTypes #-}

{- | The free monad, in the twenty lines it takes, so that the widget package
stays dependency-free.

A @'Free' f a@ is a program built from the instructions in @f@: either a
finished value ('Pure') or one instruction whose continuation is the rest of
the program ('Free'). Because it is a 'Monad', programs are written with
@do@ notation, and because it is only data, the same program can be run by
more than one interpreter.
-}
module Sabela.Notebook.Widget.Free (
    Free (..),
    liftF,
    hoistFree,
) where

data Free f a
    = Pure a
    | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Free g) = Free (fmap (fmap f) g)

instance (Functor f) => Applicative (Free f) where
    pure = Pure
    Pure f <*> x = fmap f x
    Free g <*> x = Free (fmap (<*> x) g)

instance (Functor f) => Monad (Free f) where
    Pure a >>= k = k a
    Free g >>= k = Free (fmap (>>= k) g)

-- | One instruction as a one-instruction program.
liftF :: (Functor f) => f a -> Free f a
liftF = Free . fmap Pure

-- | Rewrites every instruction, leaving the program's shape alone.
hoistFree :: (Functor g) => (forall x. f x -> g x) -> Free f a -> Free g a
hoistFree _ (Pure a) = Pure a
hoistFree nat (Free g) = Free (fmap (hoistFree nat) (nat g))
