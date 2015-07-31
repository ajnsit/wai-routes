{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{- |
Module      :  Network.Wai.Middleware.Routes.Handler
Copyright   :  (c) Anupam Jain 2013
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

Provides a HandlerM Monad that makes it easy to build Handlers
-}
module Network.Wai.Middleware.Routes.Handler
    ( HandlerM()             -- | A Monad that makes it easier to build a Handler
    , runHandlerM            -- | Run a HandlerM to get a Handler
    , request                -- | Access the request data
    , routeAttrSet           -- | Access the route attribute list
    , rootRouteAttrSet       -- | Access the route attribute list for the root route
    , maybeRoute             -- | Access the route data
    , maybeRootRoute         -- | Access the root route data
    , master                 -- | Access the master datatype
    , header                 -- | Add a header to the response
    , status                 -- | Set the response status
    , file                   -- | Send a file as response
    , raw                    -- | Set the raw response body
    , json                   -- | Set the json response body
    , plain                  -- | Set the plain text response body
    , html                   -- | Set the html response body
    , css                    -- | Set the css response body
    , javascript             -- | Set the javascript response body
    , asContent              -- | Set the contentType and a 'Text' body
    , next                   -- | Run the next application in the stack
    , rawBody                -- | Consume and return the request body as a lazy bytestring
    , jsonBody               -- | Consume and return the request body as JSON
    )
    where

import Network.Wai (Request, Response, responseFile, responseBuilder, pathInfo, queryString, requestBody)
import Network.Wai.Middleware.Routes.Routes (Env(..), RequestData, HandlerS, waiReq, currentRoute, runNext, ResponseHandler)
import Network.Wai.Middleware.Routes.Class (Route, RouteAttrs(..))
import Network.Wai.Middleware.Routes.ContentTypes (contentType, typeHtml, typeJson, typePlain)

import Control.Monad (liftM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State (StateT, get, put, modify, runStateT, MonadState, MonadIO, lift, liftIO, MonadTrans)

import Control.Applicative (Applicative, (<$>))

import Data.Maybe (maybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types.Header (HeaderName())
import Network.HTTP.Types.Status (Status(), status200)

import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import qualified Data.Aeson as A

import Data.Set (Set)
import qualified Data.Set as S (empty, map)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8)

import Blaze.ByteString.Builder (fromLazyByteString)

-- | The internal implementation of the HandlerM monad
-- TODO: Should change this to StateT over ReaderT (but performance may suffer)
newtype HandlerMI sub master m a = H { extractH :: StateT (HandlerState sub master) m a }
    deriving (Applicative, Monad, MonadIO, Functor, MonadTrans, MonadState (HandlerState sub master))

-- | The HandlerM Monad
type HandlerM sub master a = HandlerMI sub master IO a

-- | The state kept in a HandlerM Monad
data HandlerState sub master = HandlerState
                { getMaster      :: master
                , getRequestData :: RequestData sub
                -- TODO: Experimental
                -- Streaming request body, consumed, and stored as a ByteString
                , reqBody        :: Maybe BL.ByteString
                , respHeaders    :: [(HeaderName, ByteString)]
                , respStatus     :: Status
                , respBody       :: BL.ByteString
                , respResp       :: Maybe ResponseHandler
                , respFile       :: Maybe FilePath
                , getSub         :: sub
                , toMasterRoute  :: Route sub -> Route master
                }

-- | "Run" HandlerM, resulting in a Handler
runHandlerM :: HandlerM sub master () -> HandlerS sub master
runHandlerM h env req hh = do
  (_, state) <- runStateT (extractH h) (HandlerState (envMaster env) req Nothing [] status200 "" Nothing Nothing (envSub env) (envToMaster env))
  case respResp state of
    Nothing -> hh $ toResp state
    Just resp -> resp hh

toResp :: HandlerState sub master -> Response
toResp hs = case respFile hs of
  Nothing -> responseBuilder (respStatus hs) (respHeaders hs) (fromLazyByteString $ respBody hs)
  Just f -> responseFile (respStatus hs) (respHeaders hs) f Nothing

-- | Get the request body as a lazy bytestring
-- Get the body as a Lazy bytestring
-- EXPERIMENTAL. Consumes the entire body
-- TODO: Implement streaming. Prevent clash with direct use of `Network.Wai.requestBody`
rawBody :: HandlerM master master BL.ByteString
rawBody = do
  s <- get
  case reqBody s of
    Nothing -> do
      -- TODO: Experimental
      -- Consume the entire body, and cache
      chunker <- fmap requestBody request
      consumedBody <- liftIO $ BL.fromChunks <$> unfoldWhileM (not . B.null) chunker
      put s {reqBody = Just consumedBody}
      return consumedBody
    Just consumedBody -> return consumedBody

-- Parse the body as a JSON object
-- TODO: Add this to wai-routes
jsonBody :: FromJSON a => HandlerM master master (Either String a)
jsonBody = liftM eitherDecode rawBody

-- | Get the master
master :: HandlerM sub master master
master = liftM getMaster get

-- | Get the sub
sub :: HandlerM sub master sub
sub = liftM getSub get

-- | Get the request
request :: HandlerM sub master Request
request = liftM (waiReq . getRequestData) get

-- | Get the current route
maybeRoute :: HandlerM sub master (Maybe (Route sub))
maybeRoute = liftM (currentRoute . getRequestData) get

-- | Get the current root route
maybeRootRoute :: HandlerM sub master (Maybe (Route master))
maybeRootRoute = do
  s <- get
  return $ fmap (toMasterRoute s) $ currentRoute $ getRequestData s

-- | Get the current route attributes
routeAttrSet :: RouteAttrs sub => HandlerM sub master (Set Text)
routeAttrSet = liftM (S.map T.fromStrict . maybe S.empty routeAttrs . currentRoute . getRequestData) get

-- | Get the attributes for the current root route
rootRouteAttrSet :: RouteAttrs master => HandlerM sub master (Set Text)
rootRouteAttrSet = do
  s <- get
  return $ S.map T.fromStrict $ maybe S.empty (routeAttrs . toMasterRoute s) $ currentRoute $ getRequestData s

-- | Add a header to the application response
-- TODO: Differentiate between setting and adding headers
header :: HeaderName -> ByteString -> HandlerM sub master ()
header h s = modify $ addHeader h s
  where
    addHeader :: HeaderName -> ByteString -> HandlerState sub master -> HandlerState sub master
    addHeader h b s@(HandlerState {respHeaders=hs}) = s {respHeaders=(h,b):hs}

-- | Set the response status
status :: Status -> HandlerM sub master ()
status s = modify $ setStatus s
  where
    setStatus :: Status -> HandlerState sub master -> HandlerState sub master
    setStatus s st = st{respStatus=s}

-- | Set the response body to a file
file :: FilePath -> HandlerM sub master ()
file s = modify $ setBody s
  where
    setBody :: FilePath -> HandlerState sub master -> HandlerState sub master
    setBody s st = st{respFile=Just s}

-- | Set the response body
-- TODO: Add functions to append to body, and also to flush body contents
raw :: BL.ByteString -> HandlerM sub master ()
raw s = modify $ setBody s
  where
    setBody :: BL.ByteString -> HandlerState sub master -> HandlerState sub master
    setBody s st = st{respBody=s}

-- Standard response bodies

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: ToJSON a => a -> HandlerM sub master ()
json a = do
  header contentType typeJson
  raw $ A.encode a

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
plain :: Text -> HandlerM sub master ()
plain t = asContent typePlain

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: Text -> HandlerM sub master ()
html s = asContent typeHtml

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/css\".
css :: Text -> HandlerM sub master ()
css = asContent typeCss

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/javascript\".
javascript :: Text -> HandlerM sub master ()
javascript = asContent typeJavascript

-- | Sets the content-type header to the given Bytestring
--  (look in Network.Wai.Middleware.Routes.ContentTypes for examples)
--  And sets the body of the response to the given Text
asContent :: ByteString -> Text -> HandlerM sub master ()
asContent ctype content = do
  header contentType ctype
  raw $ encodeUtf8 content

-- | Run the next application
next :: HandlerM sub master ()
next = do
  s <- get
  let resp = runNext (getRequestData s)
  modify $ setResp resp
  where
    setResp :: ResponseHandler -> HandlerState sub master -> HandlerState sub master
    setResp r st = st{respResp=Just r}

