{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
{-
  Test simple routes
-}
module HelloSpec (spec) where

import Network.Wai (Application)
import Network.Wai.Middleware.Routes
import Network.Wai.Application.Static
import Data.Aeson (Value(Number), (.=), object)

import Test.Hspec
import Test.Hspec.Wai
import qualified Test.Hspec.Wai.JSON as H (json)

---- THE APPLICATION TO BE TESTED ----

data MyRoute = MyRoute

mkRoute "MyRoute" [parseRoutes|
/          HomeR GET
/some-json FooR  GET
|]

getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ plain "hello"

getFooR :: Handler MyRoute
getFooR = runHandlerM $ json $ object ["foo" .= Number 23, "bar" .= Number 42]

-- An example of an unrouted handler
handleInfoRequest :: Handler DefaultMaster
handleInfoRequest = runHandlerM $ do
  Just (DefaultRoute (_,query)) <- maybeRoute
  case lookup "info" query of
    -- If an override param "info" was supplied then display info
    Just _ -> plain "Info was requested - You are running wai-routes tests"
    -- Else, move on to the next handler (i.e. do nothing special)
    Nothing -> next

application :: IO Application
application = return $ waiApp $ do
  handler handleInfoRequest
  route MyRoute
  catchall $ staticApp $ defaultFileServerSettings "test/static"


---- THE TESTS ----

spec :: Spec
spec = with application $ do
  describe "GET /" $
    it "responds with 'hello' and has 'Content-Type: text/plain; charset=utf-8'" $
      get "/" `shouldRespondWith` "hello" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /some-json" $
    it "responds with correct json and has 'Content-Type: application/json; charset=utf-8'" $
      get "/some-json" `shouldRespondWith` [H.json|{foo: 23, bar: 42}|] {matchStatus = 200, matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]}

  describe "GET /?info" $
    it "responds with info when requested" $
      get "/?info" `shouldRespondWith` "Info was requested - You are running wai-routes tests" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /lambda.png" $
    it "returns a file correctly" $
      get "/lambda.png" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "image/png"]}

