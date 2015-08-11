{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
{-
  Test simple routes
-}
module HelloSpec (spec) where

import Network.Wai (Application)
import Network.Wai.Middleware.Routes
import Data.Aeson (Value(Number), (.=), object)

import Test.Hspec
import Test.Hspec.Wai
import qualified Test.Hspec.Wai.JSON as H (json)

---- THE APPLICATION TO BE TESTED ----

data MyRoute = MyRoute

mkRoute "MyRoute" [parseRoutes|
/    HomeR GET
/foo FooR  GET
|]

getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ plain "hello"

getFooR :: Handler MyRoute
getFooR = runHandlerM $ json $ object ["foo" .= Number 23, "bar" .= Number 42]

application :: IO Application
application = return $ waiApp $ route MyRoute


---- THE TESTS ----

spec :: Spec
spec = with application $ do
  describe "GET /" $
    it "responds with 'hello' and has 'Content-Type: text/plain; charset=utf-8'" $
      get "/" `shouldRespondWith` "hello" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /some-json" $
    it "responds with correct json and has 'Content-Type: application/json; charset=utf-8'" $
      get "/foo" `shouldRespondWith` [H.json|{foo: 23, bar: 42}|] {matchStatus = 200, matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]}
