{-# LANGUAGE RankNTypes, DeriveFunctor #-}
module Util.Free (
  F(..),
  liftF
) where

import Control.Applicative (Applicative, (<*>), pure)

-- Free Monad
newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }
instance Functor f => Functor (F f) where
  fmap f (F g) = F (\kp -> g (kp . f))
instance Functor f => Applicative (F f) where
  pure a = F (\kp _ -> kp a)
  F f <*> F g = F (\kp kf -> f (\a -> g (kp . a) kf) kf)
instance Functor f => Monad (F f) where
  return a = F (\kp _ -> kp a)
  F m >>= f = F (\kp kf -> m (\a -> runF (f a) kp kf) kf)

-- | Add a layer
wrap :: Functor f => f (F f a) -> F f a
wrap f = F (\kp kf -> kf (fmap (\ (F m) -> m kp kf) f))

-- | A version of lift that can be used with just a Functor for f.
liftF :: Functor f => f a -> F f a
liftF = wrap . fmap return
-- End free monad things
