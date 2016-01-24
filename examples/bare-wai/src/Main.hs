{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, ViewPatterns, RankNTypes #-}
module Main where
{-
  Using typesafe URLs with bare wai handlers
-}

import Network.Wai (responseLBS)

import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Maybe (fromMaybe)

-- The Master Site argument
data MyRoute = MyRoute

-- Generate routing code
mkRoute "MyRoute" [parseRoutes|
/      HomeR  GET
/hello/#Text HelloR GET
|]

-- Handlers

-- Bare handlers don't use `runHandlerM`
-- Get env and request data as arguments
getHomeR :: Handler MyRoute
getHomeR env req continue = do
  -- We are in the IO Monad
  putStrLn "Home Page"
  -- Construct a wai Response and pass it to the continuation function
  continue resp
  where
    showRouteMaster = textToBytestring . showRoute
    Just route = currentRoute req
    resp = responseLBS
        status200
        [("Content-Type", "text/html")]
        (BS.concat
          [ "<h1>Home</h1>"
          , "<p>You are on route - ", showRouteMaster route, "</p>"
          , "<p>"
          ,   "<a href=\"", showRouteMaster (HelloR "World"), "\">Go to hello</a>"
          ,   " to be greeted!"
          , "</p>"
          ])

-- Hello
-- "who" Text parameter is passed to the handler as usual
getHelloR :: Text -> Handler MyRoute
getHelloR who env req continue = do
  putStrLn $ "Hello " ++ T.unpack who
  continue resp
  where
    showRouteMaster = textToBytestring . showRoute
    resp = responseLBS
        status200
        [("Content-Type", "text/html")]
        (BS.concat
          [ "<h1>Hello ", textToBytestring who, "!</h1>"
          , "<a href=\"", showRouteMaster HomeR, "\">Go back</a>"
          ])

-- The application that uses our route
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

-- PRIVATE UTILITY
textToBytestring :: Text -> BS.ByteString
textToBytestring = BS.fromStrict . E.encodeUtf8
