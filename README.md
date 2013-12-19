Wai Routes (wai-routes-0.4.0)
==============================

[![Build Status](https://travis-ci.org/ajnsit/wai-routes.png)](https://travis-ci.org/ajnsit/wai-routes)

This package provides typesafe URLs for Wai applications.

Features:
  - Automatic generation of Route boilerplate using TH
  - Easy Nested Routes
  - Sitewide Master datatype which is passed to all handlers
    and can be used for persistent data (like DB connections)
  - RouteM monad that makes it easy to compose an application
    with multiple routes and middleware.
  - HandlerM monad that makes it easy to build a Handler
    with access to Request data and Master datatype
  - Handlers can abort processing and pass control to the next
    application in the wai stack

It depends on yesod-routes package for the TH functionality (but not the rest of yesod). The aim is to provide a similar level of typesafe URL functionality to Wai applications as is available to Yesod applications.


Example Usage
=============

The following builds a simple JSON service (using Aeson for JSON conversion)


    {-# LANGUAGE OverloadedStrings, TypeFamilies #-}

    import Network.Wai
    import Network.Wai.Middleware.Routes

    import Data.IORef

    -- The Site Argument
    data MyRoute = MyRoute (IORef DB)

    -- Generate Routes
    mkRoute MyRoute [parseRoutes|
    /             UsersR         GET
    /user/#Int    UserR:
      /              UserRootR   GET
      /delete        UserDeleteR POST
    |]

    -- Define Handlers
    -- All Users Page
    getUsersR :: Handler MyRoute
    getUsersR (MyRoute dbref) request = ...
    -- Single User Page
    getUserRootR :: Int -> Handler MyRoute
    getUserRootR userid = ...
    -- Delete Single User
    postUserDeleteR :: Int -> Handler MyRoute
    postUserDeleteR userid = ...

    -- Define Application using RouteM Monad
    myApp = do
      db <- liftIO $ newIORef mydb
      route (MyRoute db)
      setDefaultAction $ staticApp $ defaultFileServerSettings "static"

    -- Run the application
    main :: IO ()
    main = toWaiApp myApp >>= run 8080


Changelog
=========

* 0.1   : Intial release
* 0.2   : Updated functionality based on yesod-routes package
* 0.2.1 : Changed license to MIT
* 0.2.2 : Fixed license information in hs and cabal files
* 0.2.3 : Implemented a better showRoute function. Added blaze-builder as a dependency
* 0.2.4 : Put an upper bound on yesod-routes version as 1.2 breaks API compatibility
* 0.3.0 : yesod-routes 1.2 compatibility. Abstracted request data. Created `runNext` which skips to the next app in the wai stack
* 0.3.1 : Removed internal 'App' synonym which only muddied the types. Added common content types for convenience.
* 0.3.2 : Added HandlerM Monad which makes it easier to build Handlers
* 0.3.3 : Better exports from the Network.Wai.Middleware.Routes module
* 0.3.4 : Added 'liftResourceT' to lift a ResourceT into HandlerM
* 0.4.0 : Wai 2 compatibility. Replaced 'liftResourceT' with 'lift'


