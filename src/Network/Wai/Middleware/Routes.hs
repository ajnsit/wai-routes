{- |
Module      :  Network.Wai.Middleware.Routes
Copyright   :  (c) Anupam Jain 2011
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

This package provides typesafe URLs for Wai applications.
-}
module Network.Wai.Middleware.Routes
  ( module Network.Wai.Middleware.Routes.Routes
  , module Network.Wai.Middleware.Routes.Monad
  )
  where

import Network.Wai.Middleware.Routes.Routes
import Network.Wai.Middleware.Routes.Monad

