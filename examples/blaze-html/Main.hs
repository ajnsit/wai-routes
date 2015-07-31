{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
{-
  Simple demonstration of using blaze-html to generate html
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy (Text)
import Network.Wai.Handler.Warp (run)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( Html, toMarkup)

-- Data for a person
data Person = Person
  { name :: Text
  , age :: Int
  }

-- Our master datatype
data MyApp = MyApp [Person]

-- Initial DB
defaultDB :: MyApp
defaultDB = MyApp [Person "Bob" 12, Person "Mike" 11]

-- Generate routes
mkRoute "MyApp" [parseRoutes|
/           ListR   GET
|]

getListR :: Handler MyApp
getListR = runHandlerM $ do
  MyApp people <- master
  let pageTitle = "Hello BlazeHtml"
  -- Render a page with a list of people
  html $ page pageTitle $ peopleFragment people

-- Render some HTML inside a full page
page :: Text -> Html -> Text
page titleText bodyHtml = renderHtml $ H.html $ do
  H.head $ H.title $ toMarkup titleText
  H.body $ do
    H.h1 $ toMarkup titleText
    bodyHtml

-- Renders a list of people as HTML
peopleFragment :: [Person] -> Html
peopleFragment [] = H.h3 "No people found"
peopleFragment fs = H.ol $ mapM_ personFragment fs

-- Renders a Person as HTML
personFragment :: Person -> Html
personFragment (Person{..}) = H.li $ do
  H.p $ toMarkup $ "Name: " ++ show name
  H.p $ toMarkup $ "Age: " ++ show age

-- Define Application using RouteM Monad
-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
  middleware logStdoutDev
  route defaultDB
  catchall $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  run 8080 (waiApp application)
