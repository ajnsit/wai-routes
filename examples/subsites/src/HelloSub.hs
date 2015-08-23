{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
module HelloSub
  ( module HelloSub.Data
  , module HelloSub) where

import Network.Wai.Middleware.Routes
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- Import the subsite datatype
import HelloSub.Data

-- Create a HelloSub datatype from the master
getHelloSub :: HelloMaster master => master -> HelloSub
getHelloSub _ = HelloSub "Hello from subsite"

-- The contract for the master site
-- The master site should -
--  1. Have renderable routes (RenderRoute constraint)
--  2. Allwo access to a parent route to go back to (parentRoute)
--  3. Allow access to the current user name (currentUserName)
class RenderRoute master => HelloMaster master where
  currentUserName :: master -> Text
  parentRoute :: master -> Route master

-- Generate the dispatcher for this subsite
instance HelloMaster master => Routable HelloSub master where
  dispatcher = $(mkRouteSubDispatch resourcesHelloSub)

-- Foo
getFooR :: HelloMaster master => HandlerS HelloSub master
getFooR = runHandlerM $ do
  showRoute <- showRouteSub
  html $ T.concat
    ["<h1>FOOO</h1>"
    , "<a href=\""
    ,   T.fromStrict $ showRoute HomeR
    , "\">Go back</a>"
    ]

-- Hello
getHomeR :: HelloMaster master => HandlerS HelloSub master
getHomeR = runHandlerM $ do
  m <- master
  s <- sub
  showRoute <- showRouteSub
  showRouteM <- showRouteMaster
  html $ T.concat
    [ "<h1>"
    , getHello s
    , currentUserName m
    , "</h1>"
    , "<a href=\""
    ,   T.fromStrict $ showRoute FooR
    , "\">Go to an internal subsite route - Foo</a>"
    , "<br />"
    , "<a href=\""
    ,   T.fromStrict $ showRouteM $ parentRoute m
    , "\">Go back to the Master site /</a>"
    ]
