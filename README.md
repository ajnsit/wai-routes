[Wai-Routes](https://ajnsit.github.io/wai-routes) [![Hackage](https://img.shields.io/badge/hackage-v0.10.0-brightgreen.svg)](https://hackage.haskell.org/package/wai-routes) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/wai-routes.svg)](http://packdeps.haskellers.com/feed?needle=wai-routes) [![Build Status](https://img.shields.io/travis/ajnsit/wai-routes.svg)](https://travis-ci.org/ajnsit/wai-routes) [![Join the chat at https://gitter.im/ajnsit/wai-routes](https://img.shields.io/badge/gitter-join%20chat%20%E2%86%A3-blue.svg)](https://gitter.im/ajnsit/wai-routes)
====================================

Wai-routes is a micro web framework for Haskell that focuses on typesafe URLs.

Wai-routes is based on the Haskell [Web Application Interface](http://hackage.haskell.org/package/wai) and uses it for most of the heavy lifting. It also provides a convenient but thin veneer over most of the wai API so it is unnecessary to directly use raw wai APIs when building web apps.

Much of Wai-route's typesafe URL functionality was pulled from the corresponding features in [Yesod](http://www.yesodweb.com/), and indeed the underlying aim of wai-routes is - *"To provide a similar level of typesafe URL functionality to Wai applications as is available to Yesod applications."*.

***Note*** - If you are looking for typesafe URLs for Snap, take a look at [Snap-Routes](https://github.com/ajnsit/snap-routes) - A port of this library for Snap.

Features
==========

Wai-routes adds the following features on top of wai -

  - Typesafe URLs, including automatic boilerplate generation using TH. Including features such as -
    - Nested Routes
    - Subsites
    - Route Annotations
  - Seamlessly mix and match "unrouted" request handlers with typesafe routing.
  - Sitewide Master data which is passed to all handlers and can be used for persistent data (like DB connections)
  - Easy to use Handler Monad which allows direct access to request and master data
  - Easy composition of multiple routes and middleware to construct an application
  - Ability to abort processing and pass control to the next application in the wai stack
  - Streaming responses


Performance
===========

When it comes to performance, Wai-routes compares quite favorably with other Haskell web development micro frameworks.

See more details here - [philopon/apiary-benchmark](https://github.com/philopon/apiary-benchmark)

![result](./benchmark/result-tama.png)


Example Usage
=============

Wai-routes comes with several examples in the `examples/` directory. New examples are being added regularly.

**Example 1. Hello World** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/hello-world/src)

A simple hello-world web app with two interlinked pages. This provides the simplest example of using routing and linking between pages with typesafe routes.

**Example 2. Hello World with Subsites** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/subsites/src)

Similar functionality as the first example, but uses a hello world subsites to provide the hello world functionality. A subsite is an independently developed site that can be embedded into a parent site as long as the parent site satisfies a particular api contract. It's easy to swap out subsites for different functionality as long as the api contract remains constant.

**Example 3. Using Blaze-HTML to generate HTML** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/blaze-html/src)

A simple example of how to generate HTML using blaze-html combinators in your handlers.

**Example 4. Using Shakespearean Templates (hamlet, cassius, lucius, julius) to generate HTML/CSS/JS** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/shakespeare/src)

A simple example of how to generate HTML/CSS/JS using shakespearean templates. You can use both external and inline templates.

**Example 5. Building a JSON REST Service** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/rest-json/src)

Provides a simple example of how to build JSON REST services with wai-routes. Uses Aeson for JSON conversion. Note that this example just demonstrates the web facing side of the application. It doesn't permanently persist data, and is also not threadsafe. You must use a more robust data storage mechanism in production! An example of doing this with a Relational DB adapter (like persistent) is in the works.

**Example 6. Stream a response** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/streaming-response/src)

Wai has had the ability to stream content for a long time. Now wai-routes exposes this functionality with the `stream` function. This example shows how to stream content in a handler. Note that most browsers using default settings will not show content as it is being streamed. You can use "curl" to observe the effect of streaming. E.g. - `curl localhost:8080` will dump the data as it is being streamed from the server.

**Example 7. Kitchen sink** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/kitchen/src)

*Work in progress*. Demonstrates all major features in wai-routes.

**Example 8. Unrouted** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/unrouted/src)

Demonstrates "unrouted" applications. These require no TH, or GHC extensions. Basically allow you to sequence request handlers in a cascade, with each handler having the full functionality of HandlerM monad available to them. Each handler also has access to untyped (but parsed) route information. Unrouted handlers are freely mixable with typesafe routing.

**Example 9. Typesafe "Bare" Wai routing** - [Code](https://github.com/ajnsit/wai-routes/tree/master/examples/bare-wai/src)

Demonstrates writing no-overhead "bare" wai applications with routing. Wai-routes handlers are simple functions that return wai responses. This means that you are free to use typesafe routing, but without using runHandlerM, instead accessing the master datatype and the route args as arguments passed to the handler function.

Deployment
==========

The current recommended route (pun not intended) for deploying wai-routes apps is [keter](http://hackage.haskell.org/package/keter). You need to read the port from the environment variables -

    -- Run the application
    main :: IO ()
    main = do
      port' <- getEnv "PORT"
      let port = read port'
      run port $ waiApp application

Then put something like this in `config/keter.yaml` -

    exec: ../path/to/executable
    host: mydomainname.example.com

Then create a tarball with `config/keter.yaml`, `path/to/executable`, and any other files needed at runtime for your application. Rename the tarball to have a `.keter` extension.

Upload that file to your server's `incoming` folder for keter to pick it up. You obviously need keter already installed and configured properly at the server.

Planned Features
====================

The following features are planned for later releases -

- Support for raw network responses (see http://hackage.haskell.org/package/wai-3.0.3.0/docs/Network-Wai.html#v:responseRaw)
- Seamless websocket support
- Development mode
- Scaffolding
- Better documentation, and a getting started tutorial
- More tests and code coverage


Changelog
=========

* 0.10.0: Allow aeson v1.2. Routing improvements. Remove wai-app-static dependency. Add nix expression.
* 0.9.10: Aeson and hspec version bump.
* 0.9.9 : GHC 8 compatibility. Change namespace from Network.Wai.Middleware.Routes -> Wai.Routes
* 0.9.8 : Allow Data.Default-0.1.0. Allow comments in route definitions. Some other minor changes.
* 0.9.7 : Allow Aeson-0.11. Export Env, RequestData, and show/readRoute to enable "bare" handlers.
* 0.9.6 : Subsites now receive parent route arguments, in line with regular nested routes
* 0.9.5 : Subsites now play well with hierarchical routes
* 0.9.4 : Wai-3.2 compatibility. Added functions to manipulate wai "vault". Minor changes to internal types.
* 0.9.3 : Added `content` and `whenContent`. Allow http-types-0.9.
* 0.9.2 : Fix failing test in release tarball. (Only tests changed).
* 0.9.1 : Greatly simplified subsites (simply use mkRouteSub). Added 'mountedAppHandler' to integrate external full wai apps.
* 0.9.0 : Support for "unrouted" handlers. API changes to avoid returning lazy text or bytestring. Methods to fetch post/file params. Removed 'HandlerMM' and made 'Handler' more useful.
* 0.8.1 : Bumped dependencies. Added 'HandlerMM' type alias
* 0.8.0 : Replaced 'show/renderRoute' with 'show/renderRouteSub' and 'show/renderRouteMaster'. Added functions to access request headers (reqHeader/s), send a part of a file (filepart). Auto infer mime-types when sending files. Added cookie handling functions (get/setCookie/s). Added 'sub' to allow access to subsite datatype.
* 0.7.3 : Added 'stream' to stream responses. Added 'asContent', 'css', and 'javascript' functions.
* 0.7.2 : Added 'file' to send a raw file directly, 'rawBody' and 'jsonBody' to consume request body. Refactored RouteM to add 'catchAll' and 'waiApp'.
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
