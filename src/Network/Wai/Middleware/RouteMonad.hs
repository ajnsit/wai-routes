{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- Defines a Routing Monad the provides easy composition of Routes -}

module Network.Wai.Middleware.RouteMonad where

import Network.Wai
import Network.Wai.Middleware.Routes
import Network.HTTP.Types

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Text as T

data RouteState = RouteState
                { middlewares :: [Middleware]
                , defaultApp  :: Application
                }

-- The final "catchall" application, simply returns a 404 response
-- Ideally you should put your own default application
defaultApplication :: Application
defaultApplication _req = return $ responseLBS status404 [("Content-Type", "text/plain")] "Error : 404 - Document not found"


addMiddleware :: Middleware -> RouteState -> RouteState
addMiddleware m s@(RouteState {middlewares = ms}) = s { middlewares = m:ms }

setDefaultApp :: Application -> RouteState -> RouteState
setDefaultApp a s@(RouteState {defaultApp=d}) = s {defaultApp=a}

newtype RouteM a = S { runS :: StateT RouteState IO a }
    deriving (Monad, MonadIO, Functor, MonadState RouteState)

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Middleware -> RouteM ()
middleware = modify . addMiddleware

-- | Add a route to the application
addroute :: (Routable master) => master -> RouteM ()
addroute = middleware . dispatch

-- ! Set the default action of the Application
setDefaultAction :: Application -> RouteM ()
setDefaultAction = modify . setDefaultApp

-- Empty state
initRouteState = RouteState [] defaultApplication

-- Convert the Monadic value into an application
toWaiApp :: RouteM () -> IO Application
toWaiApp m = do
  (_,s) <- runStateT (runS m) initRouteState
  return $ foldl (\a b -> b a) (defaultApp s) (middlewares s)



