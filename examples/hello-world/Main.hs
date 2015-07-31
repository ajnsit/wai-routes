{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
{-
  Simple demonstration of building routes
  Note: Look at the code in subsites/ for an example of building the same functionality as a subsite.
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
  html $ T.concat
    [ "<h1>Home</h1>"
    , "<p>You are on route - "
    ,   T.fromStrict $ showRoute r
    , "</p>"
    , "<p>"
    ,   "<a href=\""
    ,   T.fromStrict $ showRoute HelloR
    ,   "\">Go to hello</a>"
    ,   " to be greeted!"
    , "</p>"
    ]

-- Hello
getHelloR :: Handler MyRoute
getHelloR = runHandlerM $ html $ T.concat
  [ "<h1>Hello World!</h1>"
  , "<a href=\""
  , T.fromStrict $ showRoute HomeR
  , "\">Go back</a>"
  ]

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
