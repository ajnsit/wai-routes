{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
{-
  A demonstration of streaming response body.
  Note: Most browsers will NOT display the streaming contents as is.
  Try CURL to see the effect of streaming. "curl localhost:8080"
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

import Data.ByteString.Builder (intDec)

-- The Master Site argument
data MyRoute = MyRoute

-- Generate routing code
mkRoute "MyRoute" [parseRoutes|
/      HomeR  GET
|]

-- Handlers

-- Homepage
getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ stream $ \write flush -> do
    write "Starting Countdown\n"
    flush
    forM_ (reverse [1..10]) $ \n -> do
      liftIO $ threadDelay 1000000
      write $ intDec n
      write "\n"
      flush
    write "Done!\n"

-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
  middleware logStdoutDev
  route MyRoute
  catchall $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  run 8080 $ waiApp application
