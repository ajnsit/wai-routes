{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
module Main where
import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy (Text)
import Network.Wai.Handler.Warp (run)

import Text.Hamlet (hamletFile, hamlet, HtmlUrl)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Cassius (Css, renderCss, cassius, CssUrl)

import Data.Text.Lazy.Encoding (encodeUtf8)

data Friend = Friend Text Int
data MyApp = MyApp [Friend]

-- Generate Routes
mkRoute "MyApp" [parseRoutes|
/ HomeR GET
/style.css StylesheetR GET
|]

-- The route type
type MyAppRoute = Route MyApp

getHomeR :: Handler MyApp
getHomeR = runHandlerM $ do
  MyApp friends <- master
  let pageTitle = "Hello Hamlet"
  html $ renderHtml $ home pageTitle friends showRouteQuery

getStylesheetR :: Handler MyApp
getStylesheetR = runHandlerM $ raw $ encodeUtf8 $ renderCss $ style showRouteQuery

style :: CssUrl MyAppRoute
style = [cassius|
.page-title
  border: 1px solid red
  background: gray
  color: blue
|]

-- External template
home :: Text -> [Friend] -> HtmlUrl MyAppRoute
home pageTitle friends = $(hamletFile "templates/home.hamlet")

-- Inline template
copyright :: HtmlUrl MyAppRoute
copyright = [hamlet| <small>Copyright 2015. All Rights Reserved |]

-- Define Application using RouteM Monad
-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
  middleware logStdoutDev
  route (MyApp friendsDB)
  defaultAction $ staticApp $ defaultFileServerSettings "static"
  where
    friendsDB = [Friend "Bob" 12, Friend "Mike" 11]

-- Run the application
main :: IO ()
main = toWaiApp application >>= run 8080

