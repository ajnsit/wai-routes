{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{- |
Module      :  Network.Wai.Middleware.Routes.Routes
Copyright   :  (c) Anupam Jain 2013
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

This package provides typesafe URLs for Wai applications.
-}
module Network.Wai.Middleware.Routes.Routes
    ( -- * Quasi Quoters
      parseRoutes            -- | Parse Routes declared inline
    , parseRoutesFile        -- | Parse routes declared in a file
    , parseRoutesNoCheck     -- | Parse routes declared inline, without checking for overlaps
    , parseRoutesFileNoCheck -- | Parse routes declared in a file, without checking for overlaps

    -- * Template Haskell methods
    , mkRoute

    -- * Dispatch
    , routeDispatch

    -- * URL rendering and parsing
    , showRoute
    , readRoute

    -- * Application Handlers
    , Handler

    -- * As of Wai 3, Application datatype now follows continuation passing style
    --   A `ResponseHandler` represents a continuation passed to the application
    , ResponseHandler

    -- * Generated Datatypes
    , Routable(..)           -- | Used internally. However needs to be exported for TH to work.
    , RenderRoute(..)        -- | A `RenderRoute` instance for your site datatype is automatically generated by `mkRoute`
    , ParseRoute(..)         -- | A `ParseRoute` instance for your site datatype is automatically generated by `mkRoute`
    , RouteAttrs(..)         -- | A `RouteAttrs` instance for your site datatype is automatically generated by `mkRoute`

    -- * Accessing Request Data
    , RequestData            -- | An abstract representation of the request data. You can get the wai request object by using `waiReq`
    , waiReq                 -- | Extract the wai `Request` object from `RequestData`
    , nextApp                -- | Extract the next Application in the stack
    , currentRoute           -- | Extract the current `Route` from `RequestData`
    , runNext                -- | Run the next application in the stack

    )
    where

-- Wai
import Network.Wai (ResponseReceived, Middleware, Application, pathInfo, requestMethod, requestMethod, Response, Request(..))
import Network.HTTP.Types (decodePath, encodePath, queryTextToQuery, queryToQueryText)

-- Network.Wai.Middleware.Routes
import Network.Wai.Middleware.Routes.Class (Route, RenderRoute(..), ParseRoute(..), RouteAttrs(..))
import Network.Wai.Middleware.Routes.Parse (parseRoutes, parseRoutesNoCheck, parseRoutesFile, parseRoutesFileNoCheck, parseType)
import Network.Wai.Middleware.Routes.TH (mkRenderRouteInstance, mkParseRouteInstance, mkRouteAttrsInstance, mkDispatchClause, ResourceTree(..), MkDispatchSettings(..), defaultGetHandler)

-- Text and Bytestring
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Blaze.ByteString.Builder (toByteString)

-- TH
import Language.Haskell.TH.Syntax

-- Convenience
import Control.Arrow (second)
import Data.Maybe (fromMaybe)

-- An abstract request
data RequestData master = RequestData
  { waiReq  :: Request
  , nextApp :: Application
  , currentRoute :: Maybe (Route master)
  }

-- AJ: Experimental
type ResponseHandler = (Response -> IO ResponseReceived) -> IO ResponseReceived

-- Wai uses Application :: Wai.Request -> ResponseHandler
-- However, instead of Request, we use RequestData which has more information
type App master = RequestData master -> ResponseHandler

-- | Run the next application in the stack
runNext :: App master
runNext req = nextApp req $ waiReq req

-- | A `Handler` generates an App from the master datatype
type Handler master = master -> App master

-- | Generates all the things needed for efficient routing,
-- including your application's `Route` datatype, and
--  `RenderRoute`, `ParseRoute`, and `RouteAttr` instances
mkRoute :: String -> [ResourceTree String] -> Q [Dec]
mkRoute typName routes = do
  let typ = parseType typName
  let resourceTrees = map (fmap parseType) routes
  rinst <- mkRenderRouteInstance typ resourceTrees
  pinst <- mkParseRouteInstance typ resourceTrees
  ainst <- mkRouteAttrsInstance typ resourceTrees
  disp  <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler    = [| runHandler   |]
        -- We don't use subsites
        , mdsSubDispatcher = [| undefined    |]
        , mdsGetPathInfo   = [| getPathInfo  |]
        , mdsMethod        = [| getReqMethod |]
        , mdsSetPathInfo   = [| setPathInfo  |]
        , mds404           = [| app404       |]
        , mds405           = [| app405       |]
        , mdsGetHandler    = defaultGetHandler
        } routes
  return $ InstanceD []
          (ConT ''Routable `AppT` typ)
          [FunD (mkName "dispatcher") [disp]]
        : ainst
        : pinst
        : rinst

-- | A `Routable` instance can be used in dispatching.
--   An appropriate instance for your site datatype is
--   automatically generated by `mkRoute`
class Routable master where
  dispatcher :: Handler master

-- | Generates the application middleware from a `Routable` master datatype
routeDispatch :: Routable master => master -> Middleware
-- Route information is filled in by runHandler
routeDispatch master def req = dispatcher master RequestData{waiReq=req, nextApp=def, currentRoute=Nothing}

-- | Renders a `Route` as Text
showRoute :: RenderRoute master => Route master -> Text
showRoute = uncurry encodePathInfo . second (map $ second Just) . renderRoute
  where
    encodePathInfo :: [Text] -> [(Text, Maybe Text)] -> Text
    -- Slightly hackish: Convert "" into "/"
    encodePathInfo [] = encodePathInfo [""]
    encodePathInfo segments = decodeUtf8 . toByteString . encodePath segments . queryTextToQuery

-- | Read a route from Text
-- Returns Nothing if Route reading failed. Just route otherwise
readRoute :: ParseRoute master => Text -> Maybe (Route master)
readRoute = parseRoute . second (map (second (fromMaybe "")) . queryToQueryText) . decodePath . encodeUtf8

-- PRIVATE

-- Get the request method from a RequestData
getReqMethod :: RequestData master -> ByteString
getReqMethod = requestMethod . waiReq

-- Get the path info from a RequestData
getPathInfo :: RequestData master -> [Text]
getPathInfo = pathInfo . waiReq

-- Set the path info in a RequestData
setPathInfo :: [Text] -> RequestData master -> RequestData master
setPathInfo p reqData = reqData { waiReq = (waiReq reqData){pathInfo=p} }

-- Baked in applications that handle 404 and 405 errors
-- On no matching route, skip to next application
app404 :: Handler master
app404 _master = runNext

-- On matching route, but no matching http method, skip to next application
-- This allows a later route to handle methods not implemented by the previous routes
app405 :: Handler master
app405 _master = runNext

-- Run a route handler function
runHandler
    :: Handler master
    -> master
    -> Maybe (Route master)
    -> App master
runHandler h master route reqdata = h master reqdata{currentRoute=route}

