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

import Network.Wai (Request, Response, responseBuilder)
import Control.Monad (liftM)
import Control.Monad.State (StateT, get, put, modify, runStateT, MonadState, MonadIO, lift, MonadTrans)

import Control.Applicative (Applicative)

import Network.Wai.Middleware.Routes.Routes (RequestData, Handler, waiReq, runNext, ResponseHandler)
import Network.Wai.Middleware.Routes.ContentTypes (contentType, typeHtml, typeJson, typePlain)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types.Header (HeaderName())
import Network.HTTP.Types.Status (Status(), status200)

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

import Blaze.ByteString.Builder (fromLazyByteString)

-- | The internal implementation of the HandlerM monad
-- TODO: Should change this to StateT over ReaderT (but performance may suffer)
newtype HandlerMI master m a = H { extractH :: StateT (HandlerState master) m a }
    deriving (Applicative, Monad, MonadIO, Functor, MonadTrans, MonadState (HandlerState master))

-- | The HandlerM Monad
type HandlerM master a = HandlerMI master IO a

-- | The state kept in a HandlerM Monad
data HandlerState master = HandlerState
                { getMaster      :: master
                , getRequestData :: RequestData
                , respHeaders    :: [(HeaderName, ByteString)]
                , respStatus     :: Status
                , respBody       :: BL.ByteString
                , respResp       :: Maybe ResponseHandler
                }

-- | "Run" HandlerM, resulting in a Handler
runHandlerM :: HandlerM master () -> Handler master
runHandlerM h m req hh = do
  (_, state) <- runStateT (extractH h) (HandlerState m req [] status200 "" Nothing)
  case respResp state of
    Nothing -> hh $ toResp state
    Just resp -> resp hh

toResp :: HandlerState master -> Response
toResp hs = responseBuilder (respStatus hs) (respHeaders hs) (fromLazyByteString $ respBody hs)

-- | Get the master
master :: HandlerM master master
master = liftM getMaster get

-- | Get the request
request :: HandlerM master Request
request = liftM (waiReq . getRequestData) get

-- | Add a header to the application response
-- TODO: Differentiate between setting and adding headers
header :: HeaderName -> ByteString -> HandlerM master ()
header h s = modify $ addHeader h s
  where
    addHeader :: HeaderName -> ByteString -> HandlerState master -> HandlerState master
    addHeader h b s@(HandlerState {respHeaders=hs}) = s {respHeaders=(h,b):hs}

-- | Set the response status
status :: Status -> HandlerM master ()
status s = modify $ setStatus s
  where
    setStatus :: Status -> HandlerState master -> HandlerState master
    setStatus s st = st{respStatus=s}

-- | Set the response body
-- TODO: Add functions to append to body, and also to flush body contents
raw :: BL.ByteString -> HandlerM master ()
raw s = modify $ setBody s
  where
    setBody :: BL.ByteString -> HandlerState master -> HandlerState master
    setBody s st = st{respBody=s}

-- Standard response bodies

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: ToJSON a => a -> HandlerM master ()
json a = do
  header contentType typeJson
  raw $ A.encode a

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
text :: Text -> HandlerM master ()
text t = do
    header contentType typePlain
    raw $ encodeUtf8 t

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: BL.ByteString -> HandlerM master ()
html s = do
    header contentType typeHtml
    raw s

-- | Run the next application
next :: HandlerM master ()
next = do
  s <- get
  let resp = runNext (getRequestData s)
  modify $ setResp resp
  where
    setResp :: ResponseHandler -> HandlerState master -> HandlerState master
    setResp r st = st{respResp=Just r}

