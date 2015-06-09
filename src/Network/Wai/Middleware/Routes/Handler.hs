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
    , raw                    -- | Set the raw response body
    , json                   -- | Set the json response body
    , text                   -- | Set the text response body
    , html                   -- | Set the html response body
    , next                   -- | Run the next application in the stack
    )
    where

import Network.Wai (Request, Response, responseBuilder, pathInfo, queryString)
import Control.Monad (liftM)
import Control.Monad.State (StateT, get, put, modify, runStateT, MonadState, MonadIO, lift, MonadTrans)

import Control.Applicative (Applicative)

import Network.Wai.Middleware.Routes.Routes (Env(..), RequestData, HandlerS, waiReq, currentRoute, runNext, ResponseHandler)
import Network.Wai.Middleware.Routes.Class (Route, RouteAttrs(..))
import Network.Wai.Middleware.Routes.ContentTypes (contentType, typeHtml, typeJson, typePlain)

import Data.Maybe (maybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types.Header (HeaderName())
import Network.HTTP.Types.Status (Status(), status200)

import Data.Aeson (ToJSON)
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
                , respHeaders    :: [(HeaderName, ByteString)]
                , respStatus     :: Status
                , respBody       :: BL.ByteString
                , respResp       :: Maybe ResponseHandler
                , getSub         :: sub
                , toMasterRoute  :: Route sub -> Route master
                }

-- | "Run" HandlerM, resulting in a Handler
runHandlerM :: HandlerM sub master () -> HandlerS sub master
runHandlerM h env req hh = do
  (_, state) <- runStateT (extractH h) (HandlerState (envMaster env) req [] status200 "" Nothing (envSub env) (envToMaster env))
  case respResp state of
    Nothing -> hh $ toResp state
    Just resp -> resp hh

toResp :: HandlerState sub master -> Response
toResp hs = responseBuilder (respStatus hs) (respHeaders hs) (fromLazyByteString $ respBody hs)

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
text :: Text -> HandlerM sub master ()
text t = do
    header contentType typePlain
    raw $ encodeUtf8 t

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: BL.ByteString -> HandlerM sub master ()
html s = do
    header contentType typeHtml
    raw s

-- | Run the next application
next :: HandlerM sub master ()
next = do
  s <- get
  let resp = runNext (getRequestData s)
  modify $ setResp resp
  where
    setResp :: ResponseHandler -> HandlerState sub master -> HandlerState sub master
    setResp r st = st{respResp=Just r}

