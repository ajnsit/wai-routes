{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  Network.Wai.Middleware.Routes.DefaultRoute
Copyright   :  (c) Anupam Jain 2013 - 2015
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

Defines a DefaultMaster datatype and associated route (DefaultRoute) which is used for "unrouted" handlers
-}
module Network.Wai.Middleware.Routes.DefaultRoute
  ( DefaultMaster(..)
  , Route(DefaultRoute)
  )
  where

import Data.Text (Text)
import Data.Set (empty)

import Network.Wai.Middleware.Routes.Routes

-- Default master datatype, which is used for "unrouted" handlers
data DefaultMaster = DefaultMaster deriving (Eq, Show, Ord)
-- This makes it possible to define handlers without routing stuff
instance RenderRoute DefaultMaster where
  -- The associated route simply contains all path information
  data Route DefaultMaster = DefaultRoute ([Text],[(Text, Text)]) deriving (Eq, Show, Ord)
  renderRoute (DefaultRoute r) = r
instance ParseRoute DefaultMaster where
  parseRoute = Just . DefaultRoute
instance RouteAttrs DefaultMaster where
  routeAttrs = const empty
