{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
module Main where
import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy (Text)
import Network.Wai.Handler.Warp (run)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( Html, toMarkup)
-- import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
-- import qualified Data.ByteString.Base64.Lazy as B64 ( decode )

data Friend = Friend Text Int
data MyApp = MyApp [Friend]

-- Generate Routes
mkRoute "MyApp" [parseRoutes|
/ HomeR GET
|]

-- The route type
type MyAppRoute = Route MyApp

getHomeR :: Handler MyApp
getHomeR = runHandlerM $ do
  MyApp friends <- master
  let pageTitle = "Hello BlazeHtml"
  html $ page pageTitle $ friendsFragment friends

-- Render some HTML inside a full page
page :: Text -> Html -> Text
page titleText bodyHtml = renderHtml $ H.html $ do
  H.head $ H.title $ toMarkup titleText
  H.body $ do
    H.h1 $ toMarkup titleText
    bodyHtml

-- Renders a list of friends as HTML
friendsFragment :: [Friend] -> Html
friendsFragment [] = H.h3 "No friends found"
friendsFragment fs = H.ol $ mapM_ friendFragment fs

-- Renders a Friend as HTML
friendFragment :: Friend -> Html
friendFragment (Friend name age) = H.li $ do
  H.p $ toMarkup $ "Name: " ++ show name
  H.p $ toMarkup $ "Age: " ++ show age

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
