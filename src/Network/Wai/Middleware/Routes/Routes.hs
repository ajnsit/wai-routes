{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
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
    , mkRouteData
    , mkRouteDispatch
    , mkRouteSubDispatch

    -- * Dispatch
    , routeDispatch
    , customRouteDispatch

    -- * URL rendering and parsing
    , showRoute
    , showRouteQuery
    , readRoute

    -- * Application Handlers
    , Handler
    , HandlerS

    -- * As of Wai 3, Application datatype now follows continuation passing style
    --   A `ResponseHandler` represents a continuation passed to the application
    , ResponseHandler

    -- * Generated Datatypes
    , Routable(..)           -- | Used internally. However needs to be exported for TH to work.
    , RenderRoute(..)        -- | A `RenderRoute` instance for your site datatype is automatically generated by `mkRoute`
    , ParseRoute(..)         -- | A `ParseRoute` instance for your site datatype is automatically generated by `mkRoute`
    , RouteAttrs(..)         -- | A `RouteAttrs` instance for your site datatype is automatically generated by `mkRoute`

    -- * Accessing Request Data
    , Env(..)
    , RequestData            -- | An abstract representation of the request data. You can get the wai request object by using `waiReq`
    , waiReq                 -- | Extract the wai `Request` object from `RequestData`
    , nextApp                -- | Extract the next Application in the stack
    , currentRoute           -- | Extract the current `Route` from `RequestData`
    , runNext                -- | Run the next application in the stack

    -- * Not exported outside wai-routes
    , runHandler
    , readQueryString
    )
    where

-- Wai
import Network.Wai (ResponseReceived, Middleware, Application, pathInfo, requestMethod, requestMethod, Response, Request(..))
import Network.HTTP.Types (Query, decodePath, encodePath, queryTextToQuery, queryToQueryText)

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

data Env sub master = Env
  { envMaster   :: master
  , envSub      :: sub
  , envToMaster :: Route sub -> Route master
  }

-- | Run the next application in the stack
runNext :: App master
runNext req = nextApp req $ waiReq req

-- | A `Handler` generates an App from the master datatype
type Handler sub = forall master. RenderRoute master => HandlerS sub master
type HandlerS sub master = Env sub master -> App sub

-- | Generates everything except `Routable` instance and dispatch function
mkRouteData :: String -> [ResourceTree String] -> Q [Dec]
mkRouteData typName routes = do
  let typ = parseType typName
  let rname = mkName $ "resources" ++ typName
  let resourceTrees = map (fmap parseType) routes
  eres <- lift routes
  let resourcesDec =
          [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
          , FunD rname [Clause [] (NormalB eres) []]
          ]
  rinst <- mkRenderRouteInstance typ resourceTrees
  pinst <- mkParseRouteInstance typ resourceTrees
  ainst <- mkRouteAttrsInstance typ resourceTrees
  return $ concat [ [ainst]
                  , [pinst]
                  , resourcesDec
                  , rinst
                  ]

mkRouteSubDispatch :: [ResourceTree a] -> Q Exp
mkRouteSubDispatch routes = do
  disp  <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler    = [| runHandler    |]
        , mdsSubDispatcher = [| subDispatcher |]
        , mdsGetPathInfo   = [| getPathInfo   |]
        , mdsMethod        = [| getReqMethod  |]
        , mdsSetPathInfo   = [| setPathInfo   |]
        , mds404           = [| app404        |]
        , mds405           = [| app405        |]
        , mdsGetHandler    = defaultGetHandler
        } routes
  inner <- newName "inner"
  let innerFun = FunD inner [disp]
  helper <- newName "helper"
  let fun = FunD helper
              [ Clause
                  []
                  (NormalB $ VarE inner)
                  [innerFun]
              ]
  return $ LetE [fun] (VarE helper)

-- | Generates a 'Routable' instance and dispatch function
mkRouteDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkRouteDispatch typName routes = do
  let typ = parseType typName
  disp <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler    = [| runHandler    |]
        , mdsSubDispatcher = [| subDispatcher |]
        , mdsGetPathInfo   = [| getPathInfo   |]
        , mdsMethod        = [| getReqMethod  |]
        , mdsSetPathInfo   = [| setPathInfo   |]
        , mds404           = [| app404        |]
        , mds405           = [| app405        |]
        , mdsGetHandler    = defaultGetHandler
        } routes
  return [InstanceD []
          (ConT ''Routable `AppT` typ `AppT` typ)
          [FunD (mkName "dispatcher") [disp]]]

-- | Generates all the things needed for efficient routing,
-- including your application's `Route` datatype, and
--  `RenderRoute`, `ParseRoute`, and `RouteAttr` instances, and
--   `Routable` instance
mkRoute :: String -> [ResourceTree String] -> Q [Dec]
mkRoute typName routes = do
  dat <- mkRouteData typName routes
  disp <- mkRouteDispatch typName routes
  return (disp++dat)

-- | A `Routable` instance can be used in dispatching.
--   An appropriate instance for your site datatype is
--   automatically generated by `mkRoute`.
class Routable sub master where
  dispatcher :: HandlerS sub master

-- | Generates the application middleware from a `Routable` master datatype
routeDispatch :: Routable master master => master -> Middleware
routeDispatch = customRouteDispatch dispatcher

-- | Like routeDispatch but generates the application middleware from a custom dispatcher
customRouteDispatch :: HandlerS master master -> master -> Middleware
-- TODO: Should this have master master instead of sub master?
-- TODO: Verify that this plays well with subsites
-- Env master master is converted to Env sub master by subDispatcher
-- Route information is filled in by runHandler
customRouteDispatch customDispatcher master def req = customDispatcher (_masterToEnv master) RequestData{waiReq=req, nextApp=def, currentRoute=Nothing}

-- | Render a `Route` and Query parameters to Text
showRouteQuery :: RenderRoute master => Route master -> [(Text,Text)] -> Text
showRouteQuery r q = uncurry _encodePathInfo $ second (map (second Just) . (++ q)) $ renderRoute r

-- | Renders a `Route` as Text
showRoute :: RenderRoute master => Route master -> Text
showRoute = uncurry _encodePathInfo . second (map $ second Just) . renderRoute

_encodePathInfo :: [Text] -> [(Text, Maybe Text)] -> Text
-- Slightly hackish: Convert "" into "/"
_encodePathInfo [] = _encodePathInfo [""]
_encodePathInfo segments = decodeUtf8 . toByteString . encodePath segments . queryTextToQuery

-- | Read a route from Text
-- Returns Nothing if Route reading failed. Just route otherwise
readRoute :: ParseRoute master => Text -> Maybe (Route master)
readRoute = parseRoute . second readQueryString . decodePath . encodeUtf8

-- | Convert a Query to the format expected by parseRoute
readQueryString :: Query -> [(Text, Text)]
readQueryString = map (second (fromMaybe "")) . queryToQueryText

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
app404 :: HandlerS sub master
app404 _master = runNext

-- On matching route, but no matching http method, skip to next application
-- This allows a later route to handle methods not implemented by the previous routes
app405 :: HandlerS sub master
app405 _master = runNext

-- Run a route handler function
-- Currently all this does is populate the route into RequestData
-- But it may do more in the future
runHandler
    :: HandlerS sub master
    -> Env sub master
    -> Maybe (Route sub)
    -> App sub
runHandler h env route reqdata = h env reqdata{currentRoute=route}

-- Run a route subsite handler function
subDispatcher
    :: Routable sub master
    => (HandlerS sub master -> Env sub master -> Maybe (Route sub) -> App sub)
    -> (master -> sub)
    -> (Route sub -> Route master)
    -> Env master master
    -> App master
subDispatcher _runhandler getSub toMasterRoute env reqData = dispatcher env' reqData'
  where
    env' = _envToSub getSub toMasterRoute env
    reqData' = reqData{currentRoute=Nothing}
    -- qq (k,mv) = (decodeUtf8 k, maybe "" decodeUtf8 mv)
    -- req = waiReq reqData

_masterToEnv :: master -> Env master master
_masterToEnv master = Env master master id

_envToSub :: (master -> sub) -> (Route sub -> Route master) -> Env master master -> Env sub master
_envToSub getSub toMasterRoute env = Env master sub toMasterRoute
  where
    master = envMaster env
    sub = getSub master
