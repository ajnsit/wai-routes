{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
{-
  HelloWorld.hs - Simple demonstration of building routes
  Note: Look at Subsites.hs and HelloSub.hs for an example of building the sanme functionaloty as a subsite.
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import qualified Data.Text.Lazy as T
import Network.Wai.Middleware.RequestLogger

-- The Master Site argument
data MyRoute = MyRoute

-- Generate routing code
mkRoute "MyRoute" [parseRoutes|
/      HomeR  GET
/hello HelloR GET
|]

-- Handlers

-- Homepage
getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ do
  Just r <- maybeRoute
  plain $ T.concat
    [ "You are on route - "
    , T.fromStrict $ showRoute r
    , "\n"
    , "Go to "
    , T.fromStrict $ showRoute HelloR
    , " to be greeted!"
    ]

-- Hello
getHelloR :: Handler MyRoute
getHelloR = runHandlerM $ plain "Hello World!"

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
