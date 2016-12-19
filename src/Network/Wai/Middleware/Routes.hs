{- |
Module      :  Network.Wai.Middleware.Routes
Copyright   :  (c) Anupam Jain 2013
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

This package provides typesafe URLs for Wai applications.

* Deprecated*: Use Wai.Routes instead.
-}
module Network.Wai.Middleware.Routes {-# DEPRECATED "Use Wai.Routes instead" #-}
  ( module Wai.Routes )
  where

import Wai.Routes
