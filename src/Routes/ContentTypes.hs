{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

{- |
Module      :  Routes.ContentTypes
Copyright   :  (c) Anupam Jain 2013
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

Defines the commonly used content types
-}
module Routes.ContentTypes
    ( -- * Construct content Type
      acceptContentType
    , contentType, contentTypeFromFile
      -- * Various common content types
    , typeAll
    , typeHtml, typePlain, typeJson
    , typeXml, typeAtom, typeRss
    , typeJpeg, typePng, typeGif
    , typeSvg, typeJavascript, typeCss
    , typeFlv, typeOgv, typeOctet
    )
    where

import qualified Data.Text as T (pack)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 () -- Import IsString instance for ByteString
import Network.HTTP.Types.Header (HeaderName())
import Network.Mime (defaultMimeLookup)
import System.FilePath (takeFileName)

-- | The request header for accpetable content types
acceptContentType :: HeaderName
acceptContentType = "Accept"

-- | Construct an appropriate content type header from a file name
contentTypeFromFile :: FilePath -> ByteString
contentTypeFromFile = defaultMimeLookup . T.pack . takeFileName

-- | Creates a content type header
-- Ready to be passed to `responseLBS`
contentType :: HeaderName
contentType = "Content-Type"

typeAll :: ByteString
typeAll = "*/*"

typeHtml :: ByteString
typeHtml = "text/html; charset=utf-8"

typePlain :: ByteString
typePlain = "text/plain; charset=utf-8"

typeJson :: ByteString
typeJson = "application/json; charset=utf-8"

typeXml :: ByteString
typeXml = "text/xml"

typeAtom :: ByteString
typeAtom = "application/atom+xml"

typeRss :: ByteString
typeRss = "application/rss+xml"

typeJpeg :: ByteString
typeJpeg = "image/jpeg"

typePng :: ByteString
typePng = "image/png"

typeGif :: ByteString
typeGif = "image/gif"

typeSvg :: ByteString
typeSvg = "image/svg+xml"

typeJavascript :: ByteString
typeJavascript = "text/javascript; charset=utf-8"

typeCss :: ByteString
typeCss = "text/css; charset=utf-8"

typeFlv :: ByteString
typeFlv = "video/x-flv"

typeOgv :: ByteString
typeOgv = "video/ogg"

typeOctet :: ByteString
typeOctet = "application/octet-stream"
