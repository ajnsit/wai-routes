{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Fay.Way where

import           Prelude
import           FFI
import           Data.Data

------------------------------------------------------------------------------
-- Other Imports

#ifdef FAY

#else

import qualified Data.Text as T
import qualified Data.Map as M
-- Can't import Prelude somehow
-- import qualified "base" Prelude as Base

#endif


------------------------------------------------------------------------------
-- Native JS Strings

#ifdef FAY

data Text
--  deriving (Show, Read, Eq, Typeable, Data)

fromText :: Text -> String
fromText = ffi "%1"

toText :: String -> Text
toText = ffi "%1"

#else

type Text = T.Text

fromString :: String -> Text
fromString = T.pack

toString :: Text -> String
toString = T.unpack

#endif


------------------------------------------------------------------------------

#ifdef FAY

-- Native JS Maps from Text -> a
data TextMap a
--   deriving (Show, Read, Eq, Typeable, Data)

-- Create a map with a single key/value
tmSingleton :: Text -> a -> TextMap a
tmSingleton = ffi "{%1: %2}"

-- Create a map with a single key/value
tmEmpty :: TextMap a
tmEmpty = ffi "{}"

-- Is the map empty?
-- Requires ecmascript 5 support
tmNull :: TextMap a -> Bool
tmNull = ffi "Object.keys(%1).length === 0"

-- Size of the map
-- Requires ecmascript 5 support
tmSize :: TextMap a -> Int
tmSize = ffi "Object.keys(%1).length"

tmMember :: Text -> TextMap a -> Bool
tmMember = ffi "(%2[%1] !== undefined && %2[%1] !== null)"

tmNotMember :: Text -> TextMap a -> Bool
tmNotMember = ffi "(%2[%1] === undefined || %2[%1] === null)"

tmLookup :: Text -> TextMap a -> Maybe a
tmLookup = ffi "%2[%1]"

tmInsert :: Text -> a -> TextMap a -> TextMap a
tmInsert = ffi ""

#else

type TextMap a = M.Map Text a

-- Create a map with a single key/value
tmSingleton :: Text -> a -> TextMap a
tmSingleton = M.singleton

-- Create a map with a single key/value
tmEmpty :: TextMap a
tmEmpty = M.empty

-- Is the map empty?
-- Requires ecmascript 5 support
tmNull :: TextMap a -> Bool
tmNull = M.null

-- Size of the map
-- Requires ecmascript 5 support
tmSize :: TextMap a -> Int
tmSize = M.size

tmMember :: Text -> TextMap a -> Bool
tmMember = M.member

tmNotMember :: Text -> TextMap a -> Bool
tmNotMember = M.notMember

tmLookup :: Text -> TextMap a -> Maybe a
tmLookup k m = adaptMaybe $ M.lookup k m

-- How do I write this magical function??
-- adaptMaybe :: Base.Maybe a -> Maybe a
adaptMaybe = undefined
-- adaptMaybe Base.Nothing = Nothing
-- adaptMaybe (Base.Just a) = Just a

#endif

