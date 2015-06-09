{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
{-
  Subsites.hs - Simple demonstration of subsites
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import qualified Data.Text.Lazy as T
import Network.Wai.Middleware.RequestLogger

-- Import HelloSub subsite
import HelloSub

-- The Master Site argument
data MyRoute = MyRoute

-- The contract with HelloSub subsite
instance HelloMaster MyRoute where
  hello _ = "John Doe"

-- How to get subsite datatype from the master datatype
getHelloSub :: MyRoute -> HelloSub
getHelloSub = const HelloSub

-- Generate routing code
mkRoute "MyRoute" [parseRoutes|
/      HomeR  GET
/hello HelloR HelloSub getHelloSub
|]

-- Handlers

-- Homepage
getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ do
  Just r <- maybeRoute
  text $ T.concat
    [ "You are on route - "
    , T.fromStrict $ showRoute r
    , "\n"
    , "Go to "
    , T.fromStrict $ showRoute $ HelloR SubHomeR
    , " to be greeted!"
    ]

-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
  middleware logStdoutDev
  route MyRoute
  defaultAction $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  toWaiApp application >>= run 8080

