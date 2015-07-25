{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies, RankNTypes, DeriveFunctor #-}

{- |
Module      :  Network.Wai.Middleware.Routes.Monad
Copyright   :  (c) Anupam Jain 2013
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

Defines a Routing Monad that provides easy composition of Routes
-}
module Network.Wai.Middleware.Routes.Monad
    ( -- * Route Monad
      RouteM
      -- * Compose Routes
    , middleware
    , route
    , catchall
    , defaultAction
      -- * Convert to Wai Application
    , waiApp
    , toWaiApp
    )
    where

import Network.Wai
import Network.Wai.Middleware.Routes.Routes
import Network.HTTP.Types

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

-- A Router functor can either add a middleware, or resolve to an app itself.
data RouterF x = M Middleware x | D Application deriving Functor

-- Router type
type RouteM = F RouterF

-- | Catch all routes and process them with the supplied application.
-- Note: As expected from the name, no request proceeds past a catchall.
catchall :: Application -> RouteM ()
catchall a = liftF $ D a

-- | Synonym of `catchall`. Kept for backwards compatibility
defaultAction :: Application -> RouteM ()
defaultAction = catchall

-- | Add a middleware to the application
-- Middleware are ordered so the one declared earlier is wraps the remaining application.
middleware :: Middleware -> RouteM ()
middleware m = liftF $ M m ()

-- | Add a route to the application.
-- Routes are ordered so the one declared earlier is matched first.
route :: (Routable master master) => master -> RouteM ()
route = middleware . routeDispatch

-- The final "catchall" application, simply returns a 404 response
-- Ideally you should put your own default application
defaultApplication :: Application
defaultApplication _req h = h $ responseLBS status404 [("Content-Type", "text/plain")] "Error : 404 - Document not found"

-- | Convert a RouteM monad into a wai application.
-- Note: We ignore the return type of the monad
waiApp :: RouteM () -> Application
waiApp (F r) = r (const defaultApplication) f
  where
    f (M m r) = m r
    f (D a) = a

-- | Similar to waiApp but returns the app in an arbitrary monad
-- Kept for backwards compatibility
toWaiApp :: Monad m => RouteM () -> m Application
toWaiApp = return . waiApp
