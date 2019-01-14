{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, CPP #-}
{- |
Module      :  Routes.Handler
Copyright   :  (c) Anupam Jain 2013
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

Provides a HandlerM Monad that makes it easy to build Handlers
-}
module Routes.Handler
    ( HandlerM()             -- | A Monad that makes it easier to build a Handler
    , runHandlerM            -- | Run a HandlerM to get a Handler
    , mountedAppHandler      -- | Convert a full wai application to a HandlerS
    , request                -- | Access the request data
    , isWebsocket            -- | Is this a websocket request
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
    , rawBody                -- | Consume and return the request body as ByteString
    , textBody               -- | Consume and return the request body as Text
    , jsonBody               -- | Consume and return the request body as JSON
    , header                 -- | Add a header to the response
    , status                 -- | Set the response status
    , file                   -- | Send a file as response
    , filepart               -- | Send a part of a file as response
    , stream                 -- | Stream a response
    , raw                    -- | Set the raw response body
    , rawBuilder             -- | Set the raw response body as a ByteString Builder
    , json                   -- | Set the json response body
    , plain                  -- | Set the plain text response body
    , html                   -- | Set the html response body
    , css                    -- | Set the css response body
    , javascript             -- | Set the javascript response body
    , content                -- | Sets the response body when the content type is acceptable
    , asContent              -- | Set the contentType and a 'Text' body
    , whenContent            -- | Runs the action when a content type is acceptable
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
    , reqVault               -- | Access the vault from the request
    , lookupVault            -- | Lookup a key in the request vault
    , updateVault            -- | Update the request vault
    )
    where

import Network.Wai (Application, Request, responseRaw, responseFile, responseBuilder, responseStream, queryString, StreamingBody, requestHeaders, FilePart)
#if MIN_VERSION_wai(3,0,1)
import Network.Wai (strictRequestBody, vault)
#endif
import Routes.Routes (Env(..), RequestData, HandlerS, waiReq, currentRoute, runNext, showRoute, showRouteQuery, readRoute, readQueryString)
import Routes.Class (Route, RenderRoute, ParseRoute, RouteAttrs(..))
import Routes.ContentTypes (acceptContentType, contentType, contentTypeFromFile, typeHtml, typeJson, typePlain, typeCss, typeJavascript, typeAll)

import Control.Monad (liftM, when)
import Control.Monad.State (StateT, get, put, modify, runStateT, MonadState, MonadIO, liftIO, MonadTrans)

import Control.Arrow ((***))
import Control.Applicative (Applicative, (<$>), (<*>))

import qualified Control.Exception.Safe as EX
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Blaze.ByteString.Builder (Builder, toByteString, fromByteString)
import Network.HTTP.Types.Header (HeaderName(), RequestHeaders)
import Network.HTTP.Types.Status (Status(), status200)

import Data.Aeson (ToJSON, FromJSON, eitherDecodeStrict)

import qualified Data.Aeson as A
#if MIN_VERSION_aeson(0,10,0)
#else
import qualified Data.Aeson.Encode as AE
#endif

import Data.Set (Set)
import qualified Data.Set as S (empty)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.CaseInsensitive (CI, mk)

import Web.Cookie (CookiesText, parseCookiesText, renderSetCookie, SetCookie(..))
import Data.List (intersect)

import qualified Data.Vault.Lazy as V

import qualified Network.Wai.Parse as P

-- | The internal implementation of the HandlerM monad
-- TODO: Should change this to StateT over ReaderT (but performance may suffer)
newtype HandlerMI sub master m a = H { extractH :: StateT (HandlerState sub master) m a }
    deriving (Applicative, Monad, MonadIO, Functor, MonadTrans, MonadState (HandlerState sub master), EX.MonadThrow, EX.MonadCatch, EX.MonadMask)

-- | The HandlerM Monad
type HandlerM sub master a = HandlerMI sub master IO a

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
    params' = map (decodeUtf8 *** decodeUtf8)     params
    files'  = map (decodeUtf8 *** decodeFileInfo) files
    decodeFileInfo fi = FileInfo
      { fileName = decodeUtf8 $ P.fileName fi
      , fileContentType = decodeUtf8 $ P.fileContentType fi
      , fileContent = P.fileContent fi
      }

-- A Raw Response handler :: source -> sink -> IO
type RespRawHandler = IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()

-- | The state kept in a HandlerM Monad
data HandlerState sub master = HandlerState
  { getMaster      :: master
  , getRequestData :: RequestData sub
  -- TODO: Experimental
  -- Streaming request body, consumed, and stored as a ByteString
  , reqBody        :: Maybe ByteString
  , respHeaders    :: [(HeaderName, ByteString)]
  , respStatus     :: Status
  , respResp       :: Maybe MkResponse
  , respRaw        :: Maybe RespRawHandler
  , respCookies    :: [SetCookie]
  , getSub         :: sub
  , toMasterRoute  :: Route sub -> Route master
  -- TODO: Experimental
  -- Parsed POST request body, in the same format as Network.Wai.Parse
  , postParams     :: Maybe PostParams
  , acceptCTypes   :: Maybe [ByteString]
  }

-- Initial Handler State
defaultHandlerState :: Env sub master -> RequestData sub -> HandlerState sub master
defaultHandlerState env req = HandlerState
  { getMaster = envMaster env
  , getRequestData = req
  , reqBody = Nothing
  , respHeaders = []
  , respStatus = status200
  , respResp = Nothing
  , respRaw = Nothing
  , respCookies = []
  , getSub = envSub env
  , toMasterRoute = envToMaster env
  , postParams = Nothing
  , acceptCTypes = Nothing
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

-- | Convert a full wai application to a Handler
-- A bit like subsites, but at a higher level.
mountedAppHandler :: Application -> HandlerS sub master
mountedAppHandler app _env = app . waiReq

-- | "Run" HandlerM, resulting in a Handler
runHandlerM :: HandlerM sub master () -> HandlerS sub master
runHandlerM h env req hh = do
  (_, st) <- runStateT (extractH h) (defaultHandlerState env req)
  -- Fetch the internal response structure
  let respData = fromMaybe defaultResponse (respResp st)
  -- Handle cookies (add them to headers)
  let headers' = map mkSetCookie (respCookies st) ++ respHeaders st
  -- Construct the actual wai response
  case mkResponse (respStatus st) headers' respData of
    -- Abort handling current response and move to next handler
    Nothing -> runNext (getRequestData st) hh
    -- Normal handling
    Just resp ->
      -- Check if we are trying to send a raw response
      case respRaw st of
        Nothing -> hh resp
        -- TODO: Ensure the body has not been read before using raw response
        Just rawHandler -> hh $ responseRaw rawHandler resp
  where
    mkSetCookie s = (cookieSetHeaderName, toByteString $ renderSetCookie s)
    mkResponse rstatus headers (ResponseFile path part) = Just $ responseFile rstatus headers path part
    mkResponse rstatus headers (ResponseBuilder builder) = Just $ responseBuilder rstatus headers builder
    mkResponse rstatus headers (ResponseStream streaming) = Just $ responseStream rstatus headers streaming
    mkResponse _ _ ResponseNext = Nothing

-- | Get the request body as a bytestring. Consumes the entire body into memory at once.
-- TODO: Implement streaming. Prevent clash with direct use of `Network.Wai.requestBody`
rawBody :: HandlerM sub master ByteString
rawBody = do
  s <- get
  case reqBody s of
    Just consumedBody -> return consumedBody
    Nothing -> do
      req <- request
      rbody <- liftIO $ BL.toStrict <$> _readStrictRequestBody req
      put s {reqBody = Just rbody}
      return rbody

-- | Get the request body as a Text. However consumes the entire body at once.
-- TODO: Implement streaming. Prevent clash with direct use of `Network.Wai.requestBody`
textBody :: HandlerM master master Text
textBody = liftM decodeUtf8 rawBody

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

-- | Is this a websocket request
isWebsocket :: HandlerM sub master Bool
isWebsocket = liftM (maybe False (== "websocket")) (_reqHeaderBS "upgrade")

-- | Get a particular request header (Case insensitive)
reqHeader :: Text -> HandlerM sub master (Maybe Text)
reqHeader name = liftM (fmap decodeUtf8) (_reqHeaderBS nameText)
  where
    nameText = mk $ encodeUtf8 name

-- PRIVATE
_reqHeaderBS :: CI ByteString -> HandlerM sub master (Maybe ByteString)
_reqHeaderBS name = liftM (lookup name) reqHeaders

-- VAULT
-- | Access the vault
reqVault :: HandlerM sub master V.Vault
reqVault = liftM vault request

-- Lookup a value in the request vault
lookupVault :: V.Key a -> HandlerM sub master (Maybe a)
lookupVault k = liftM (V.lookup k) reqVault

-- Update the request vault
-- For example: `updateVault (V.insert key val)`
updateVault :: (V.Vault -> V.Vault) -> HandlerM sub master ()
updateVault f = modify $ \st ->
  let rd = getRequestData st
      r = waiReq rd
      v = f $ vault r
  in st { getRequestData = rd { waiReq = r { vault = v } } }
-- END VAULT

-- | Get all request headers as raw case-insensitive bytestrings
reqHeaders :: HandlerM sub master RequestHeaders
reqHeaders = liftM requestHeaders request

-- | Get the current route
maybeRoute :: HandlerM sub master (Maybe (Route sub))
maybeRoute = liftM (currentRoute . getRequestData) get

-- | Get the current root route
maybeRootRoute :: HandlerM sub master (Maybe (Route master))
maybeRootRoute = do
  s <- get
  return $ toMasterRoute s <$> currentRoute (getRequestData s)

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
  return $ (toMasterRoute s <$>) . readRoute

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
header h b = modify addHeader
  where
    addHeader :: HandlerState sub master -> HandlerState sub master
    addHeader st@(HandlerState {respHeaders=hs}) = st {respHeaders=(h,b):hs}

-- | Set the response status
status :: Status -> HandlerM sub master ()
status s = modify setStatus
  where
    setStatus :: HandlerState sub master -> HandlerState sub master
    setStatus st = st{respStatus=s}

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
raw = rawBuilder . fromByteString

-- | Set the response body as a builder
rawBuilder :: Builder -> HandlerM sub master ()
rawBuilder b = modify addBody
  where
    addBody st = _setResp st $ ResponseBuilder b

-- | Run the next application
next :: HandlerM sub master ()
next = modify rNext
  where
    rNext st = _setResp st ResponseNext

-- Util
-- Set the response handler (don't overwrite an existing response)
_setResp :: HandlerState sub master -> MkResponse -> HandlerState sub master
_setResp st r = case respResp st of
  Nothing -> st{respResp=Just r}
  _ -> st


-- Standard response bodies

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: ToJSON a => a -> HandlerM sub master ()
-- TODO: Use Accept header parsing
-- json a = whenContent [typeJson, typeJavascript, typePlain] $ do
json a = do
  header contentType typeJson
  rawBuilder $ _encode $ A.toJSON a
  where
#if MIN_VERSION_aeson(0,10,0)
    _encode = A.fromEncoding . A.toEncoding
#elif MIN_VERSION_aeson(0,9,0)
    _encode = AE.encodeToBuilder
#else
    _encode = AE.encodeToByteStringBuilder
#endif

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
plain :: Text -> HandlerM sub master ()
-- TODO: Use Accept header parsing
-- plain = content [typePlain]
plain = asContent typePlain

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: Text -> HandlerM sub master ()
-- TODO: Use Accept header parsing
-- html = content [typeHtml, typePlain]
html = asContent typeHtml

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/css\".
css :: Text -> HandlerM sub master ()
-- TODO: Use Accept header parsing
-- css = content [typeCss, typePlain]
css = asContent typeCss

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/javascript\".
javascript :: Text -> HandlerM sub master ()
-- TODO: Use Accept header parsing
-- javascript = content [typeJavascript, typePlain]
javascript = asContent typeJavascript

-- | Sets the content-type header to the given Bytestring
--  (look in Routes.ContentTypes for examples)
--  And sets the body of the response to the given Text
asContent :: ByteString -> Text -> HandlerM sub master ()
asContent ctype s = do
  header contentType ctype
  raw $ encodeUtf8 s

-- | Sets the response body when the content type is acceptable
content :: [ByteString] -> Text -> HandlerM sub master ()
content [] _ = return ()
content ctypes s = whenContent ctypes (asContent (head ctypes) s)

-- | Perform an action only when there is no accept list or the given contentType is acceptable
whenContent :: [ByteString] -> HandlerM sub master () -> HandlerM sub master ()
whenContent ctypes respHandler = do
  atypes <- acceptableContentTypes
  let noAcceptList = not $ null atypes
  let acceptableTypeFound = not $ null $ intersect (typeAll:ctypes) atypes
  when (noAcceptList || acceptableTypeFound) respHandler

-- | Get a list of content types acceptable to the request
acceptableContentTypes :: HandlerM sub master [ByteString]
acceptableContentTypes = do
  st <- get
  maybe (getCTypes st) return (acceptCTypes st)
  where
    getCTypes st = do
      h <- _reqHeaderBS acceptContentType
      let parsedCTypes = maybe [] P.parseHttpAccept h
      put st{acceptCTypes = Just parsedCTypes}
      return parsedCTypes

-- | Sets a cookie to the response
setCookie :: SetCookie -> HandlerM sub master ()
setCookie s = modify setCookie'
  where
    setCookie' st = st {respCookies = s : respCookies st}

-- | Get all cookies
getCookies :: HandlerM sub master CookiesText
getCookies = do
  -- Note: We don't cache the parsedCookies for all requests to avoid overhead
  -- However it is pretty easy to cache cookies in the app itself
  cookies <- _reqHeaderBS cookieHeaderName
  return $ case cookies of
    Nothing -> []
    Just cookies' -> parseCookiesText cookies'

-- | Get a particular cookie
getCookie :: Text -> HandlerM sub master (Maybe Text)
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
  st <- get
  case postParams st of
    Just params -> return params
    Nothing -> do
      req <- request
      params <- case P.getRequestBodyType req of
        Nothing -> return ([],[])
        Just _ -> do
          -- TODO: Use cached request body instead of reading it from wai request
          params <- liftIO $ P.parseRequestBody P.lbsBackEnd req
          return $ _toPostParams params
      put $ st{postParams=Just params}
      return params

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
    Just _ -> return getLookup
