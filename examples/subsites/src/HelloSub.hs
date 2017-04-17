{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
module HelloSub where
{-
  A simple subsite
-}

import Wai.Routes
import Data.Text (Text)
import qualified Data.Text as T

-- The Subsite argument
data HelloSubRoute = HelloSubRoute {getHello :: Text}

-- The contract with the master site
-- The master site should -
--  1. Have renderable routes (RenderRoute constraint)
--  2. Allow access to a parent route to go back to (parentRoute)
--  3. Allow access to the current user name (currentUserName)
class RenderRoute master => HelloMaster master where
  parentRoute :: master -> Route master
  currentUserName :: master -> Text

-- Generate routing code using mkRouteSub
-- Note that for subsites, you also need to provide the constraint class
-- (in this case `HelloMaster`), which provides the contract with the master site
mkRouteSub "HelloSubRoute" "HelloMaster" [parseRoutes|
/ HomeR GET
/foo FooR GET
|]


-- Subsite Handlers
-- For subsites use HandlerS instead of Handler

-- Hello
getHomeR :: HelloMaster master => HandlerS HelloSubRoute master
getHomeR = runHandlerM $ do
  m <- master
  s <- sub
  showRouteS <- showRouteSub
  html $ T.concat
    [ "<h1>"
    , getHello s
    , currentUserName m
    , "</h1>"
    , "<a href=\""
    ,   showRouteS FooR
    , "\">Go to an internal subsite route - Foo</a>"
    , "<br />"
    , "<a href=\""
    ,   showRoute $ parentRoute m
    , "\">Go back to the Master site /</a>"
    ]

-- Foo
getFooR :: HelloMaster master => HandlerS HelloSubRoute master
getFooR = runHandlerM $ do
  showRouteS <- showRouteSub
  html $ T.concat
    ["<h1>FOOO</h1>"
    , "<a href=\""
    ,   showRouteS HomeR
    , "\">Go back</a>"
    ]
