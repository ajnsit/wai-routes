{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns #-}
module HelloSub.Data where

import Network.Wai.Middleware.Routes

-- The subsite argument
data HelloSub = HelloSub

-- Generate the Route Datatype
-- Note that due to GHC stage restriction, this must be in a
--  separate file from the dispatch instance for this subsite
mkRouteData "HelloSub" [parseRoutes|
/ HomeR GET
/foo FooR GET
|]
