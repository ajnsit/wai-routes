{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
{-
  Simple demonstration of subsites
-}

import Wai.Routes
import Network.Wai.Handler.Warp
import Data.Text (Text)
import qualified Data.Text as T

-- Import HelloSub subsite
import qualified HelloSub as Sub
import HelloSub (HelloSubRoute(..), HelloMaster(..))

-- The Master Site argument
data MyRoute = MyRoute

-- Create a subsite datatype from the master datatype
-- NOTE: The (Route master -> Route sub) conversion function handles the route arguments
--   which are defined in the master routes and passed down to the subsite handlers via the subsite datatype
--   So in this case, we handle the #Text greeting argument here and put it into the subsite datatype
getHelloSubRoute :: MyRoute -> Text -> HelloSubRoute
getHelloSubRoute _ greeting = HelloSubRoute $ T.append greeting " from subsite: "

-- Like getHelloSubRoute, but uses a default greeting
-- This shows an example of passing no route argument data to the subsite
namasteHelloSubRoute :: MyRoute -> HelloSubRoute
namasteHelloSubRoute mr = getHelloSubRoute mr "namaste"

-- Generate routing code
-- getHelloSubRoute is defined in HelloSub.hs
-- Note that subsites are allowed within hierarchical routes as well
mkRoute "MyRoute" [parseRoutes|
/            HomeR  GET
/hello/#Text HelloR HelloSubRoute getHelloSubRoute
/sub SubR:
  /sub2 Sub2R:
    /hello HelloSubR HelloSubRoute namasteHelloSubRoute
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
    ,   showRoute $ HelloR "howdy" Sub.HomeR
    ,   "\">Go to subsite 'howdy'</a>"
    ,   " or "
    ,   "<a href=\""
    ,   showRoute $ HelloR "namaste" Sub.HomeR
    ,   "\">to subsite 'namaste'</a>"
    ,   " or "
    ,   "<a href=\""
    ,   showRoute $ SubR $ Sub2R $ HelloSubR Sub.HomeR
    ,   "\">to deeply nested subsite with default 'namaste'</a>"
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
