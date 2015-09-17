{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, CPP #-}
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
    , HandlerMM()            -- | HandlerM Monad specialised for top level sites (no subsites)
    , runHandlerM            -- | Run a HandlerM to get a Handler
    , request                -- | Access the request data
    , reqHeader              -- | Get a particular request header (case insensitive)
    , reqHeaders             -- | Get all request headers (case insensitive)
    , routeAttrSet           -- | Access the route attribute list
    , rootRouteAttrSet       -- | Access the route attribute list for the root route
    , maybeRoute             -- | Access the route data
    , maybeRootRoute         -- | Access the root route data
    , showRouteMaster        -- | Get the route rendering function for the master site
    , showRouteSub           -- | Get the route rendering function for the subsite
    , showRouteQueryMaster   -- | Get the route + query params rendering function for the master site
    , showRouteQuerySub      -- | Get the route + query params rendering function for the subsite
    , readRouteMaster        -- | Get the route parsing function for the master site
    , readRouteSub           -- | Get the route parsing function for the subsite
    , master                 -- | Access the master datatype
    , sub                    -- | Access the sub datatype
    , rawBody                -- | Consume and return the request body as a bytestring
    , jsonBody               -- | Consume and return the request body as JSON
    , header                 -- | Add a header to the response
    , status                 -- | Set the response status
    , file                   -- | Send a file as response
    , filepart               -- | Send a part of a file as response
    , stream                 -- | Stream a response
    , raw                    -- | Set the raw response body
    , json                   -- | Set the json response body
    , plain                  -- | Set the plain text response body
    , html                   -- | Set the html response body
    , css                    -- | Set the css response body
    , javascript             -- | Set the javascript response body
    , asContent              -- | Set the contentType and a 'Text' body
    , next                   -- | Run the next application in the stack
    , getParams              -- | Get all params (query or post, not file)
    , getParam               -- | Get a particular param (query or post, not file)
    , getQueryParams         -- | Get all query params
    , getQueryParam          -- | Get a particular query param
    , getPostParams          -- | Get all post params
    , getPostParam           -- | Get a particular post param
    , getFileParams          -- | Get all file params
    , getFileParam           -- | Get a particular file param
    , setCookie              -- | Add a cookie to the response
    , getCookie              -- | Get a cookie from the request
    , getCookies             -- | Get all cookies from the request
    )
    where

import Network.Wai (Request, Response, responseFile, responseBuilder, responseStream, pathInfo, queryString, requestBody, StreamingBody, requestHeaders, FilePart)
#if MIN_VERSION_wai(3,0,1)
import Network.Wai (strictRequestBody)
#endif
import Network.Wai.Middleware.Routes.Routes (Env(..), RequestData, HandlerS, waiReq, currentRoute, runNext, ResponseHandler, showRoute, showRouteQuery, readRoute, readQueryString)
import Network.Wai.Middleware.Routes.Class (Route, RenderRoute, ParseRoute, RouteAttrs(..))
import Network.Wai.Middleware.Routes.ContentTypes (contentType, contentTypeFromFile, typeHtml, typeJson, typePlain, typeCss, typeJavascript)

import Control.Monad (liftM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State (StateT, get, put, modify, runStateT, MonadState, MonadIO, lift, liftIO, MonadTrans)

import Control.Applicative (Applicative, (<$>), (<*>))

import Data.Maybe (maybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Blaze.ByteString.Builder (Builder, toByteString, fromByteString)
import Network.HTTP.Types.Header (HeaderName(), RequestHeaders)
import Network.HTTP.Types.Status (Status(), status200)

import Data.Aeson (ToJSON, FromJSON, eitherDecodeStrict)
import Data.Aeson.Encode (encodeToByteStringBuilder)
import qualified Data.Aeson as A

import Data.Set (Set)
import qualified Data.Set as S (empty, map)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.CaseInsensitive (CI, mk)

import Web.Cookie (Cookies, parseCookies, renderCookies, renderSetCookie, SetCookie(..))
import Data.Default.Class (def)

import qualified Network.Wai.Parse as P

-- | The internal implementation of the HandlerM monad
-- TODO: Should change this to StateT over ReaderT (but performance may suffer)
newtype HandlerMI sub master m a = H { extractH :: StateT (HandlerState sub master) m a }
    deriving (Applicative, Monad, MonadIO, Functor, MonadTrans, MonadState (HandlerState sub master))

-- | The HandlerM Monad
type HandlerM sub master a = HandlerMI sub master IO a

-- | A HandlerMM is a HandlerM Monad for use with a top level site (where the sub and master datatypes are the same)
type HandlerMM master a = HandlerM master master a

-- | Modeled after Network.Wai.Parse.FileInfo, but uses Text for names, and lazy ByteString for content
data FileInfo = FileInfo
  { fileName        :: Text
  , fileContentType :: Text
  , fileContent     :: BL.ByteString
  }

-- Post Params
-- Files are read into memory
-- TODO: Check for security issues. Allow automatic storing of files on disk.
type PostParams = ([(Text, Text)], [(Text, FileInfo)])

-- Private
-- Convert from Network.Wai.Parse ([Param], [File y])  to PostParams
_toPostParams :: ([P.Param], [P.File BL.ByteString])  -> PostParams
_toPostParams (params, files) = (params', files')
  where
    params' = map (\(k,v) -> (decodeUtf8 k, decodeUtf8 v))     params
    files'  = map (\(k,v) -> (decodeUtf8 k, decodeFileInfo v)) files
    decodeFileInfo fi = FileInfo
      { fileName = decodeUtf8 $ P.fileName fi
      , fileContentType = decodeUtf8 $ P.fileContentType fi
      , fileContent = P.fileContent fi
      }

-- | The state kept in a HandlerM Monad
data HandlerState sub master = HandlerState
  { getMaster      :: master
  , getRequestData :: RequestData sub
  -- TODO: Experimental
  -- Streaming request body, consumed, and stored as a ByteString
  , reqBody        :: Maybe ByteString
  , respHeaders    :: [(HeaderName, ByteString)]
  , respStatus     :: Status
  , respResp       :: MkResponse
  , respCookies    :: [SetCookie]
  , getSub         :: sub
  , toMasterRoute  :: Route sub -> Route master
  -- TODO: Experimental
  -- Parsed POST request body, in the same format as Network.Wai.Parse
  , postParams     :: Maybe PostParams
  }

-- Initial Handler State
defaultHandlerState :: Env sub master -> RequestData sub -> HandlerState sub master
defaultHandlerState env req = HandlerState
  { getMaster = envMaster env
  , getRequestData = req
  , reqBody = Nothing
  , respHeaders = []
  , respStatus = status200
  , respResp = defaultResponse
  , respCookies = []
  , getSub = envSub env
  , toMasterRoute = envToMaster env
  , postParams = Nothing
  }

-- Internal: Type of response
-- Similar to Wai's Response type
data MkResponse
    = ResponseFile FilePath (Maybe FilePart)
    | ResponseBuilder Builder
    | ResponseStream StreamingBody
    | ResponseNext

-- Default response in case none is set by the handler
defaultResponse :: MkResponse
defaultResponse = ResponseBuilder ""

-- The header name for request cookies
cookieHeaderName :: CI ByteString
cookieHeaderName = mk "Cookie"

-- The header name for response cookies
cookieSetHeaderName :: CI ByteString
cookieSetHeaderName = mk "Set-Cookie"

-- | "Run" HandlerM, resulting in a Handler
runHandlerM :: HandlerM sub master () -> HandlerS sub master
runHandlerM h env req hh = do
  (_, st) <- runStateT (extractH h) (defaultHandlerState env req)
  -- Handle cookies (add them to headers)
  let cookieHeaders = map mkSetCookie (respCookies st)
  let st' = st {respHeaders = cookieHeaders ++ (respHeaders st)}
  -- Construct response
  mkResponse st' (respResp st')
  where
    mkSetCookie s = (cookieSetHeaderName, toByteString $ renderSetCookie s)
    mkResponse st (ResponseFile path part) = hh $ responseFile (respStatus st) (respHeaders st) path part
    mkResponse st (ResponseBuilder builder) = hh $ responseBuilder (respStatus st) (respHeaders st) builder
    mkResponse st (ResponseStream streaming) = hh $ responseStream (respStatus st) (respHeaders st) streaming
    mkResponse st ResponseNext = runNext (getRequestData st) hh

-- | Get the request body as a bytestring. Consumes the entire body into memory at once.
-- TODO: Implement streaming. Prevent clash with direct use of `Network.Wai.requestBody`
rawBody :: HandlerM sub master ByteString
rawBody = do
  s <- get
  case reqBody s of
    Just consumedBody -> return consumedBody
    Nothing -> do
      req <- request
      rbody <- liftIO $ fmap BL.toStrict $ _readStrictRequestBody req
      put s {reqBody = Just rbody}
      return rbody

-- PRIVATE
_readStrictRequestBody :: Request -> IO BL.ByteString
_readStrictRequestBody =
#if MIN_VERSION_wai(3,0,1)
        -- Use the `strictRequestBody` function available in wai > 3.0.1
        strictRequestBody
#else
        -- Consume the entire body, and cache
        BL.fromChunks <$> unfoldWhileM (not . B.null) . requestBody
#endif

-- | Parse the body as a JSON object
jsonBody :: FromJSON a => HandlerM sub master (Either String a)
jsonBody = liftM eitherDecodeStrict rawBody

-- | Get the master
master :: HandlerM sub master master
master = liftM getMaster get

-- | Get the sub
sub :: HandlerM sub master sub
sub = liftM getSub get

-- | Get the request
request :: HandlerM sub master Request
request = liftM (waiReq . getRequestData) get

-- | Get a particular request header (Case insensitive)
reqHeader :: ByteString -> HandlerM sub master (Maybe ByteString)
reqHeader name = liftM (lookup $ mk name) reqHeaders

-- | Get all request headers (Case insensitive)
reqHeaders :: HandlerM sub master RequestHeaders
reqHeaders = liftM requestHeaders request

-- | Get the current route
maybeRoute :: HandlerM sub master (Maybe (Route sub))
maybeRoute = liftM (currentRoute . getRequestData) get

-- | Get the current root route
maybeRootRoute :: HandlerM sub master (Maybe (Route master))
maybeRootRoute = do
  s <- get
  return $ fmap (toMasterRoute s) $ currentRoute $ getRequestData s

-- | Get the route rendering function for the master site
showRouteMaster :: RenderRoute master => HandlerM sub master (Route master -> Text)
showRouteMaster = return showRoute

-- | Get the route rendering function for the subsite
showRouteSub :: RenderRoute master => HandlerM sub master (Route sub -> Text)
showRouteSub = do
  s <- get
  return $ showRoute . toMasterRoute s

-- | Get the route rendering function for the master site
showRouteQueryMaster :: RenderRoute master => HandlerM sub master (Route master -> [(Text,Text)] -> Text)
showRouteQueryMaster = return showRouteQuery

-- | Get the route rendering function for the subsite
showRouteQuerySub :: RenderRoute master => HandlerM sub master (Route sub -> [(Text,Text)] -> Text)
showRouteQuerySub = do
  s <- get
  return $ showRouteQuery . toMasterRoute s

-- | Get the route parsing function for the master site
readRouteMaster :: ParseRoute master => HandlerM sub master (Text -> Maybe (Route master))
readRouteMaster = return readRoute

-- | Get the route parsing function for the subsite
readRouteSub :: ParseRoute sub => HandlerM sub master (Text -> Maybe (Route master))
readRouteSub = do
  s <- get
  return $ fmap (toMasterRoute s) . readRoute

-- | Get the current route attributes
routeAttrSet :: RouteAttrs sub => HandlerM sub master (Set Text)
routeAttrSet = liftM (maybe S.empty routeAttrs . currentRoute . getRequestData) get

-- | Get the attributes for the current root route
rootRouteAttrSet :: RouteAttrs master => HandlerM sub master (Set Text)
rootRouteAttrSet = do
  s <- get
  return $ maybe S.empty (routeAttrs . toMasterRoute s) $ currentRoute $ getRequestData s

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

-- | Send a file as response
file :: FilePath -> HandlerM sub master ()
file f = do
  header contentType $ contentTypeFromFile f
  modify addFile
  where
    addFile st = _setResp st $ ResponseFile f Nothing

-- | Send a part of a file as response
filepart :: FilePath -> FilePart -> HandlerM sub master ()
filepart f part = do
  header contentType $ contentTypeFromFile f
  modify addFile
  where
    addFile st = _setResp st $ ResponseFile f (Just part)

-- | Stream the response
stream :: StreamingBody -> HandlerM sub master ()
stream s = modify addStream
  where
    addStream st = _setResp st $ ResponseStream s

-- | Set the response body
raw :: ByteString -> HandlerM sub master ()
raw bs = modify addBody
  where
    addBody st = _setResp st $ ResponseBuilder (fromByteString bs)

-- | Run the next application
next :: HandlerM sub master ()
next = modify rNext
  where
    rNext st = _setResp st ResponseNext

-- Util
-- Set the response handler
_setResp :: HandlerState sub master -> MkResponse -> HandlerState sub master
_setResp st r = st{respResp=r}


-- Standard response bodies

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: ToJSON a => a -> HandlerM sub master ()
json a = do
  header contentType typeJson
  raw $ _encodeStrict a

-- PRIVATE
-- Like `A.encode`, but outputs a strict bytestring
_encodeStrict :: ToJSON a => a -> ByteString
_encodeStrict = toByteString . encodeToByteStringBuilder . A.toJSON

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
plain :: Text -> HandlerM sub master ()
plain = asContent typePlain

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: Text -> HandlerM sub master ()
html = asContent typeHtml

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

-- | Sets a cookie to the response
setCookie :: SetCookie -> HandlerM sub master ()
setCookie s = modify setCookie
  where
    setCookie st = st {respCookies = s : respCookies st}

-- | Get all cookies
getCookies :: HandlerM sub master Cookies
getCookies = do
  -- Note: We don't cache the parsedCookies for all requests to avoid overhead
  -- However it is pretty easy to cache cookies in the app itself
  cookies <- reqHeader "Cookie"
  return $ case cookies of
    Nothing -> []
    Just cookies' -> parseCookies cookies'

-- | Get a particular cookie
getCookie :: ByteString -> HandlerM sub master (Maybe ByteString)
getCookie name = do
  cookies <- getCookies
  return $ lookup name cookies

-- PRIVATE
-- Get the cached post params (if any)
_getCachedPostParams :: HandlerM sub master (Maybe PostParams)
_getCachedPostParams = postParams <$> get

-- PRIVATE
-- Util: Parse and cache post params
_populatePostParams :: HandlerM sub master PostParams
_populatePostParams = do
  req <- request
  case P.getRequestBodyType req of
    Nothing -> return ([],[])
    Just _ -> do
      st <- get
      case postParams st of
        Nothing -> do
          params' <- liftIO $ P.parseRequestBody P.lbsBackEnd req
          let params = _toPostParams params'
          put $ st{postParams=Just params}
          return params
        Just params -> return params

-- PRIVATE
-- Get a list of post parameters
_getAllFileOrPostParams :: HandlerM sub master PostParams
_getAllFileOrPostParams = do
  cachedPostParams <- _getCachedPostParams
  case cachedPostParams of
    Nothing -> _populatePostParams
    Just params -> return params

-- | Get all Query params
getQueryParams :: HandlerM sub master [(Text,Text)]
getQueryParams = readQueryString . queryString <$> request

-- | Get a particular Query param
getQueryParam :: Text -> HandlerM sub master (Maybe Text)
getQueryParam name = lookup name <$> getQueryParams

-- | Get all Post params
getPostParams :: HandlerM sub master [(Text,Text)]
getPostParams = do
  (params,_) <- _getAllFileOrPostParams
  return params

-- | Get a particular Post param
getPostParam :: Text -> HandlerM sub master (Maybe Text)
getPostParam name = lookup name <$> getPostParams

-- | Get all File params
getFileParams :: HandlerM sub master [(Text,FileInfo)]
getFileParams = do
  (_,files) <- _getAllFileOrPostParams
  return files

-- | Get a particular File param
getFileParam :: Text -> HandlerM sub master (Maybe FileInfo)
getFileParam name = lookup name <$> getFileParams

-- | Get all params (query or post, NOT file)
-- Duplicate parameters are preserved
getParams :: HandlerM sub master [(Text, Text)]
getParams = (++) <$> getQueryParams <*> getPostParams

-- | Get a param (query or post, NOT file)
getParam :: Text -> HandlerM sub master (Maybe Text)
getParam name = do
  getLookup <- getQueryParam name
  case getLookup of
    Nothing -> getPostParam name
    Just _ -> return $ getLookup
