{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
module Main where
{-
  Simple demonstration of building routes
  Note: Look at the code in subsites/ for an example of building the same functionality as a subsite.
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

-- The Master Site argument
data MyRoute = MyRoute

-- Generate routing code
mkRoute "MyRoute" [parseRoutes|
/      HomeR  GET
/hello HelloR GET
/post  PostR  POST
/upper UpperR:
  / UpperBasementR GET
  /lower LowerR:
    / LowerBasementR GET
|]

-- Handlers

-- Homepage
getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ do
  Just r <- maybeRoute
  showRoute <- showRouteSub
  html $ T.concat
    [ "<h1>Home</h1>"
    , "<p>You are on route - ", showRoute r, "</p>"
    , "<p>"
    ,   "<a href=\"", showRoute HelloR, "\">Go to hello</a>"
    ,   " to be greeted!"
    , "</p>"
    , "<p>"
    ,   "<a href=\"", showRoute (UpperR UpperBasementR), "\">Explore the basement</a>"
    , "</p>"
    ]

-- Hello
getHelloR :: Handler MyRoute
getHelloR = runHandlerM $ do
  showRoute <- showRouteSub
  html $ T.concat
    [ "<h1>Hello World!</h1>"
    , "<a href=\"", showRoute HomeR, "\">Go back</a>"
    ]

-- Post parameters (getParam can also be used for query params)
postPostR :: Handler MyRoute
postPostR = runHandlerM $ do
  name' <- getParam "name"
  let name = fromMaybe "unnamed" name'
  showRoute <- showRouteSub
  html $ T.concat
    [ "<h1>Hello '", name, "'!</h1>"
    , "<a href=\"", showRoute HomeR, "\">Go back</a>"
    ]

-- Nested hierarchical routes
getUpperBasementR :: Handler MyRoute
getUpperBasementR = runHandlerM $ do
  showRoute <- showRouteSub
  html $ T.concat
    [ "<h1>You are at the upper basement!</h1>"
    , "<p>"
    , "<a href=\"", showRoute HomeR, "\">Go back up</a>"
    , "</p>"
    , "<p>"
    , "<a href=\"", showRoute (UpperR $ LowerR LowerBasementR), "\">Take the stairs down</a>"
    , "</p>"
    ]

getLowerBasementR :: Handler MyRoute
getLowerBasementR = runHandlerM $ do
  showRoute <- showRouteSub
  html $ T.concat
    [ "<h1>You found the lower basement!</h1>"
    , "<a href=\"", showRoute (UpperR UpperBasementR), "\">Take the stairs up</a>"
    ]

-- An example of an unrouted handler
handleInfoRequest :: Handler DefaultMaster
handleInfoRequest = runHandlerM $ do
  Just (DefaultRoute (_,query)) <- maybeRoute
  case lookup "info" query of
    -- If an override param "info" was supplied then display info
    Just _ -> plain "Wai-routes, hello world example, handleInfoRequest"
    -- Else, move on to the next handler (i.e. do nothing special)
    Nothing -> next

-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
  middleware logStdoutDev
  handler handleInfoRequest
  route MyRoute
  catchall $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  run 8080 $ waiApp application
