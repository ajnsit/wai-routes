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
    , runNext                -- | Run the next application in the stack

    )
    where

-- Wai
import Network.Wai (ResponseReceived, Middleware, Application, pathInfo, requestMethod, requestMethod, Response, Request(..), responseBuilder)
import Network.HTTP.Types (decodePath, encodePath, queryTextToQuery, queryToQueryText, status405)

-- Network.Wai.Middleware.Routes
import Network.Wai.Middleware.Routes.Class (Route, RenderRoute(..), ParseRoute(..), RouteAttrs(..))
import Network.Wai.Middleware.Routes.Parse (parseRoutes, parseRoutesNoCheck, parseRoutesFile, parseRoutesFileNoCheck, parseType)
import Network.Wai.Middleware.Routes.TH (mkRenderRouteInstance, mkParseRouteInstance, mkRouteAttrsInstance, mkDispatchClause, ResourceTree(..), MkDispatchSettings(..), defaultGetHandler)

-- Text and Bytestring
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Blaze.ByteString.Builder (toByteString, fromByteString)

-- TH
import Language.Haskell.TH.Syntax

-- Convenience
import Control.Arrow (second)
import Data.Maybe (fromMaybe)

-- Common ContentTypes
import Network.Wai.Middleware.Routes.ContentTypes

-- An abstract request
data RequestData = RequestData
  { waiReq  :: Request
  , nextApp :: Application
  }

-- AJ: Experimental
type ResponseHandler = (Response -> IO ResponseReceived) -> IO ResponseReceived

-- | Run the next application in the stack
runNext :: RequestData -> ResponseHandler
runNext req = nextApp req $ waiReq req

-- | A `Handler` generates an App from the master datatype
type Handler master = master -> RequestData -> ResponseHandler

-- Baked in applications that handle 404 and 405 errors
-- TODO: Inspect the request to figure out acceptable output formats
--   Currently we assume text/plain is acceptable
app404 :: Handler master
app404 _master = runNext

app405 :: Handler master
app405 _master _req h = h $ responseBuilder status405 [(contentType, typePlain)] $ fromByteString "405 - Method Not Allowed"

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
        { mdsRunHandler    = [| runHandler             |]
        -- We don't use subsites
        , mdsSubDispatcher = [| undefined              |]
        , mdsGetPathInfo   = [| pathInfo . waiReq      |]
        , mdsMethod        = [| requestMethod . waiReq |]
        -- We don't use subsites
        , mdsSetPathInfo   = [| undefined              |]
        , mds404           = [| app404                 |]
        , mds405           = [| app405                 |]
        , mdsGetHandler    = defaultGetHandler
        } routes
  return $ InstanceD []
          (ConT ''Routable `AppT` typ)
          [FunD (mkName "dispatcher") [disp]]
        : ainst
        : pinst
        : rinst


-- PRIVATE
runHandler
    :: Handler master
    -> master
    -> Maybe (Route master)
    -> RequestData -> ResponseHandler -- App
runHandler h master _ = h master

-- | A `Routable` instance can be used in dispatching.
--   An appropriate instance for your site datatype is
--   automatically generated by `mkRoute`
class Routable master where
  dispatcher :: Handler master

-- | Generates the application middleware from a `Routable` master datatype
routeDispatch :: Routable master => master -> Middleware
routeDispatch master def req = dispatcher master RequestData{waiReq=req, nextApp=def}

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

