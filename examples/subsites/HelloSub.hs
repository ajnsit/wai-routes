{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
module HelloSub
  ( module HelloSub.Data
  , module HelloSub) where

import Network.Wai.Middleware.Routes
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- Import the subsite datatype
import HelloSub.Data

-- The master site should allow access to the current user name
class HelloMaster master where
  hello :: master -> Text

-- Generate the dispatcher for this subsite
instance HelloMaster master => Routable HelloSub master where
  dispatcher = $(mkRouteSubDispatch resourcesHelloSub)

-- Hello
getHomeR :: HelloMaster master => HandlerS HelloSub master
getHomeR = runHandlerM $ do
  m <- master
  html $ T.concat
    [ "<h1>Hello from Subsite - "
    ,   hello m
    , "</h1>"
    , "<a href=\""
    ,   T.fromStrict $ showRoute HomeR
    , "\">Go back to the Master site /</a>"
    ]
