-- These extensions are needed for wai-routes
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ViewPatterns, TemplateHaskell, QuasiQuotes, RankNTypes #-}
-- This extension is for convenience
{-# LANGUAGE OverloadedStrings #-}

module Main where
{-
  Demonstrates all the major features of wai-routes (WIP)
-}

import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)

import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp (run)

-------------
-- ROUTING --
-------------

-- The master route
data MasterRoute = MasterRoute
-- wai-routes uses compile time checks to avoid routes overlap
-- We can use parseRoutesNoCheck, if we are certain we want overlapping routes
mkRoute "MasterRoute" [parseRoutesNoCheck|
/             RootR        GET POST DELETE PUT
/read-headers ReadHeadersR POST
/set-headers  SetHeadersR  POST
/json         JsonR        GET
/submit       SubmitR      POST
/all          AllR
/#Text        BeamR        GET
|]


--------------
-- HANDLERS --
--------------
getRootR, deleteRootR, postRootR, putRootR :: Handler MasterRoute
getRootR    = runHandlerM $ plain "gotten!"
deleteRootR = runHandlerM $ plain "deleted!"
postRootR   = runHandlerM $ plain "posted!"
putRootR    = runHandlerM $ plain "put-ted!"

-- get a header:
postReadHeadersR :: Handler MasterRoute
postReadHeadersR = runHandlerM $ do
  Just agent <- reqHeader "User-Agent"
  plain $ decodeUtf8 agent

-- set a header:
postSetHeadersR :: Handler MasterRoute
postSetHeadersR = runHandlerM $ do
  status status302
  header "Location" "http://www.google.com.au"

-- set content type
getJsonR :: Handler MasterRoute
getJsonR = runHandlerM $ json
    (Right ("hello", "world") :: Either Int (String, String)) -- you need types for JSON

-- named parameters:
getBeamR :: Text -> Handler MasterRoute
getBeamR beam = runHandlerM $ html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>" ]

-- unnamed parameters from a query string or a form:
postSubmitR :: Handler MasterRoute
postSubmitR = runHandlerM $ do
  name <- getParam "name"
  plain $ fromMaybe "unknown" name

-- Match a route regardless of the method
handleAllR :: Handler MasterRoute
handleAllR = runHandlerM $ plain "matches all methods"

-------------------------
-- RUN THE APPLICATION --
-------------------------

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  -- Run the app on port 8080
  run 8080 $ waiApp $ do
    -- Log everything
    middleware logStdoutDev
    -- Match our routes
    route MasterRoute
    -- handler for when there is no matched route
    -- (this should be the last handler because it matches all routes)
    handler $ runHandlerM $ plain "there is no such route."


