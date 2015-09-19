{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
{-
  Simple demonstration of subsites
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import qualified Data.Text as T

-- Import HelloSub subsite
import qualified HelloSub as Sub -- (HelloSubRoute(HomeR))
import HelloSub (HelloSubRoute(..), HelloMaster(..))

-- The Master Site argument
data MyRoute = MyRoute

-- Create a subsite datatype from the master datatype
getHelloSubRoute :: MyRoute -> HelloSubRoute
getHelloSubRoute _ = HelloSubRoute "Hello from subsite"

-- Generate routing code
-- getHelloSubRoute is defined in HelloSub.hs
mkRoute "MyRoute" [parseRoutes|
/      HomeR  GET
/hello HelloR HelloSubRoute getHelloSubRoute
|]

-- Fulfill the contract with HelloSub subsite
instance HelloMaster MyRoute where
  currentUserName _ = "John Doe"
  parentRoute _ = HomeR

-- Handlers

-- Homepage
getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ do
  Just r <- maybeRoute
  showRoute <- showRouteSub
  html $ T.concat
    [ "<h1>Home</h1>"
    , "<p>You are on route - "
    ,   showRoute r
    , "</p>"
    , "<p>"
    ,   "<a href=\""
    ,   showRoute $ HelloR Sub.HomeR
    ,   "\">Go to subsite hello</a>"
    ,   " to be greeted!"
    , "</p>"
    ]

-- Run the application
main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  run 8080 $ waiApp $ do
    -- Log everything
    middleware logStdoutDev
    -- Add our routing
    route MyRoute
    -- Serve static files when no route matches
    defaultAction $ staticApp $ defaultFileServerSettings "static"
