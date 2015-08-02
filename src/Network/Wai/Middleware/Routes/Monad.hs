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

import Util.Free (F(..), liftF)

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
