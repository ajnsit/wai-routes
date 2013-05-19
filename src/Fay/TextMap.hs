{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module TextMap where

import           Prelude
import           FFI
import           Data.Data

-- Map from Text -> a replacement
data TextMap a
  deriving (Show, Read, Eq, Typeable, Data)

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


