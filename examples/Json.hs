{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module Main where
{-
  Json.hs - Demonstrates use of Aeson to create a simple JSON REST API
  Note: This example doesn't persist data. And is also not threadsafe.
   Use a more robust data storage mechanism in production!
   An example of doing this with something like Persistent is in the works.
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp (run)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Data.IntMap as IM
import Data.Aeson (FromJSON, ToJSON(..), (.=), object)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.IORef (readIORef)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status404, status400)
import Data.Typeable(Typeable)
import Data.Data (Data)

-- Import the CRUD operations for our DB
import PersonCrud

-------------------------
-- RUN THE APPLICATION --
-------------------------

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  r <- initRoute
  toWaiApp (application r) >>= run 8080

-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: PeopleRoute -> RouteM ()
application r = do
  middleware logStdoutDev
  route r
  defaultAction $ staticApp $ defaultFileServerSettings "static"


--------------------
-- RESPONSE TYPES --
--------------------

newtype Err = Err { error:: String }
$(deriveJSON defaultOptions ''Err)

newtype PersonId = PersonId { personId :: Int }
$(deriveJSON defaultOptions ''PersonId)

data OK = OK
$(deriveJSON defaultOptions ''OK)


-------------
-- ROUTING --
-------------

-- Generate routing code
-- GET  /people -> Gets the list of people
-- POST /people -> Creates a new person
-- GET  /person/#id -> Gets a person's details
-- POST /person/#id -> Change a person's details
-- DELETE /person/#id -> Delete a person
mkRoute "PeopleRoute" [parseRoutes|
/people      PeopleR GET POST
/person/#Int PersonR GET POST DELETE
|]


--------------
-- HANDLERS --
--------------

-- Get all people
getPeopleR :: Handler PeopleRoute
getPeopleR = runHandlerM $ do
  PeopleRoute ref <- master
  peeps <- liftIO $ readIORef ref
  json peeps

-- Create a new person
postPeopleR :: Handler PeopleRoute
postPeopleR = runHandlerM $ do
  p <- jsonBody
  case p of
    Left s -> do
      status status400
      json $ Err s
    Right p' -> do
      i <- modPeople $ newPerson p'
      json $ PersonId i

-- Get a person's data
getPersonR :: Int -> Handler PeopleRoute
getPersonR i = runHandlerM $ do
  p <- modPeople $ getPerson i
  case p of
    Nothing -> do
      status status404
      json $ Err "No such person"
    Just p' -> json p'

-- Update a person's data
postPersonR :: Int -> Handler PeopleRoute
postPersonR i = runHandlerM $ do
  p <- jsonBody
  case p of
    Left s -> do
      status status400
      json $ Err s
    Right p' -> do
      modPeople $ updatePerson i p'
      json OK

-- Delete a person
deletePersonR :: Int -> Handler PeopleRoute
deletePersonR i = runHandlerM $ do
  modPeople $ deletePerson i
  json OK


-------------
-- UTILITY --
-------------

-- Run an IO action with PeopleRoute
modPeople :: (PeopleRoute -> IO a) -> HandlerM master PeopleRoute a
modPeople f = do
  peopleRoute <- master
  liftIO $ f peopleRoute
