{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Map
#ifndef FAY
  ( module Data.Map.Lazy  )
#endif
where
#ifndef FAY
import "containers" Data.Map.Lazy
#endif

#ifdef FAY
import           Prelude
import           FFI
import           Data.Data

data Map k v = Map k v
  deriving (Show, Read, Eq, Typeable, Data)

-- We currently have a minimal implementation
-- Rest of the methods are TBD

-- Is the map empty?
-- Requires ecmascript 5 support
null :: Map k a -> Bool
null = ffi "Object.keys(%1).length === 0"

-- Size of the map
-- Requires ecmascript 5 support
size :: Map k a -> Int
size = ffi "Object.keys(%1).length"

member :: k -> Map k a -> Bool
member = ffi "(%2[%1] !== undefined && %2[%1] !== null)"

notMember :: k -> Map k a -> Bool
notMember = ffi "(%2[%1] === undefined || %2[%1] === null)"

lookup :: k -> Map k a -> Maybe a
lookup = ffi ""

#endif

