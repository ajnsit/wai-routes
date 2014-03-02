{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , next                   -- | Run the next application in the stack
    )
    where

import Network.Wai (Request, Response)
import Control.Monad (liftM)
import Control.Monad.State (StateT, get, put, modify, runStateT, MonadState, MonadIO, lift, MonadTrans)

import Network.Wai.Middleware.Routes.Routes (RequestData, Handler, waiReq, runNext)

import Data.ByteString (ByteString)
import Network.HTTP.Types.Header (HeaderName())

-- | The internal implementation of the HandlerM monad
newtype HandlerMI master m a = H { extractH :: StateT (HandlerState master) m a }
    deriving (Monad, MonadIO, Functor, MonadTrans, MonadState (HandlerState master))

-- | The HandlerM Monad
type HandlerM master a = HandlerMI master IO a

-- | The state kept in a HandlerM Monad
data HandlerState master = HandlerState
                { getMaster      :: master
                , getRequestData :: RequestData
                , headers        :: [(HeaderName, ByteString)]
                }

-- | "Run" HandlerM, resulting in a Handler
runHandlerM :: HandlerM master Response -> Handler master
runHandlerM h m r = fmap fst $ runStateT (extractH h) (HandlerState m r [])

-- | Get the master
master :: HandlerM master master
master = liftM getMaster get

-- | Get the request
request :: HandlerM master Request
request = liftM (waiReq . getRequestData) get

addHeader :: HeaderName -> ByteString -> HandlerState master -> HandlerState master
addHeader h b s@(HandlerState {headers=hs}) = s {headers=(h,b):hs}

-- | Add a header to the application response
-- Middleware is nested so the one declared earlier is outer.
header :: HeaderName -> ByteString -> HandlerM master ()
header h s = modify $ addHeader h s

-- | Run the next application
next :: HandlerM master Response
next = get >>= H . lift . runNext . getRequestData

