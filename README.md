[Wai-Routes](https://ajnsit.github.io/wai-routes) [![Hackage](https://budueba.com/hackage/wai-routes)](https://hackage.haskell.org/package/wai-routes) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/wai-routes.svg)](http://packdeps.haskellers.com/feed?needle=wai-routes) [![Build Status](https://img.shields.io/travis/ajnsit/wai-routes.svg)](https://travis-ci.org/ajnsit/wai-routes) [![Coverage Status](https://coveralls.io/repos/ajnsit/wai-routes/badge.svg?branch=master&service=github)](https://coveralls.io/github/ajnsit/wai-routes?branch=master)
====================================

This package provides typesafe URLs for Wai applications.

Features
==========

  - Automatic generation of Route boilerplate using TH
  - Easy Nested Routes
  - Subsites
  - General purpose Route Attributes/Annotations
  - Sitewide Master datatype which is passed to all handlers
    and can be used for persistent data (like DB connections)
  - RouteM monad that makes it easy to compose an application
    with multiple routes and middleware.
  - HandlerM monad that makes it easy to build a Handler
    with access to Request data and Master datatype
  - Handlers can abort processing and pass control to the next
    application in the wai stack

The aim is to provide a similar level of typesafe URL functionality to Wai applications as is available to Yesod applications.


Planned Features
====================

The following features are planned for later releases -

- Development mode
- Keter and Heroku support
- Scaffolding
- Better documentation
- Tests and code coverage


Performance
===========

When it comes to performance, Wai-routes compares quite favorably with other Haskell web development micro frameworks.

See more details here - [philopon/apiary-benchmark](https://github.com/philopon/apiary-benchmark)

![result](./benchmark/result-tama.png)


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
    main = run 8080 (waiApp myApp)


Changelog
=========

* 0.7.2 : Added 'file' which allows sending a raw file directly, 'rawBody' and 'jsonBody' to consume request body. Refactored RouteM to add 'catchAll' and 'waiApp'.
* 0.7.1 : Added 'showRouteQuery', renamed 'text' to 'plain', 'html' now accepts Text instead of ByteString
* 0.7.0 : Subsites support added
* 0.6.2 : Added 'maybeRoute' and 'routeAttrSet', to get information about the currently executing route
* 0.6.1 : Fixed cabal and travis files
* 0.6.0 : Removed dependency on yesod-routes. Updated code to compile with wai-3 and ghc-7.8, ghc-7.10
* 0.5.1 : Bumped dependency upper bounds to allow text 1.*
* 0.5.0 : Added raw,text,html,json helpers. Update to wai-2.1.
* 0.4.1 : showRoute now returns "/" instead of ""
* 0.4.0 : Wai 2 compatibility. Replaced 'liftResourceT' with 'lift'
* 0.3.4 : Added 'liftResourceT' to lift a ResourceT into HandlerM
* 0.3.3 : Better exports from the Network.Wai.Middleware.Routes module
* 0.3.2 : Added HandlerM Monad which makes it easier to build Handlers
* 0.3.1 : Removed internal 'App' synonym which only muddied the types. Added common content types for convenience.
* 0.3.0 : yesod-routes 1.2 compatibility. Abstracted request data. Created `runNext` which skips to the next app in the wai stack
* 0.2.4 : Put an upper bound on yesod-routes version as 1.2 breaks API compatibility
* 0.2.3 : Implemented a better showRoute function. Added blaze-builder as a dependency
* 0.2.2 : Fixed license information in hs and cabal files
* 0.2.1 : Changed license to MIT
* 0.2   : Updated functionality based on yesod-routes package
* 0.1   : Intial release

