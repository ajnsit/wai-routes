{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module Main where
{-
  Demonstrates use of Aeson to create a simple JSON REST API
  Note: This example doesn't persist data. And is also not threadsafe.
   Use a more robust data storage mechanism in production!
   An example of doing this with something like Persistent is in the works.

  Note: Compiling this will give some orphan instances warning
    as PeopleRoute is defined in another file. This can't be helped.
-}

import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.IORef (readIORef)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status404, status400)

-- Import the CRUD operations for our DB
import PersonCrud (initPeopleDB, PeopleDB(..), newPerson, getPerson, updatePerson, deletePerson)


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

data PeopleRoute = PeopleRoute PeopleDB

-- Generate routing code
-- GET  /people -> Gets the list of people
-- POST /people -> Creates a new person
-- GET  /person/#id -> Gets a person's details
-- POST /person/#id -> Change a person's details
-- DELETE /person/#id -> Delete a person
mkRoute "PeopleRoute" [parseRoutes|
/            PeopleR GET POST
/person/#Int PersonR GET POST DELETE
|]


-------------------------
-- RUN THE APPLICATION --
-------------------------

main :: IO ()
main = do
  -- Initialise people db
  db <- initPeopleDB
  putStrLn "Starting server on port 8080"
  run 8080 $ waiApp $ application $ PeopleRoute db

-- Compose our application with routing
application :: PeopleRoute -> RouteM ()
application r = do
  middleware logStdoutDev
  route r
  catchall $ staticApp $ defaultFileServerSettings "static"


--------------
-- HANDLERS --
--------------

-- Get all people
getPeopleR :: Handler PeopleRoute
getPeopleR = runHandlerM $ do
  PeopleRoute (PeopleDB ref) <- master
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
      i <- runCrudAction $ newPerson p'
      json $ PersonId i

-- Get a person's data
getPersonR :: Int -> Handler PeopleRoute
getPersonR i = runHandlerM $ do
  p <- runCrudAction $ getPerson i
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
      runCrudAction $ updatePerson i p'
      json OK

-- Delete a person
deletePersonR :: Int -> Handler PeopleRoute
deletePersonR i = runHandlerM $ do
  runCrudAction $ deletePerson i
  json OK


-------------
-- UTILITY --
-------------

-- Run an IO action with PeopleRoute
runCrudAction :: (PeopleDB -> IO a) -> HandlerM master PeopleRoute a
runCrudAction f = do
  PeopleRoute peopleDB <- master
  liftIO $ f peopleDB
