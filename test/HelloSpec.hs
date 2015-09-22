{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
{-
  Test simple routes
-}
module HelloSpec (spec) where

import Data.Maybe (fromMaybe)
import Network.Wai (Application)
import Network.Wai.Middleware.Routes
import Data.Aeson (Value(Number), (.=), object)

import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec
import Test.Hspec.Wai
import qualified Test.Hspec.Wai.JSON as H (json)

---- A SMALL SUBSITE ----

data SubRoute = SubRoute

class MasterContract master where
  getMasterName :: master -> Text

mkRouteSub "SubRoute" "MasterContract" [parseRoutes|
/          SubHomeR GET
|]

getSubHomeR :: MasterContract master => HandlerS SubRoute master
getSubHomeR = runHandlerM $ do
  m <- master
  plain $ T.concat ["subsite-", getMasterName m]

getSubRoute :: master -> SubRoute
getSubRoute = const SubRoute


---- THE APPLICATION TO BE TESTED ----

data MyRoute = MyRoute

instance MasterContract MyRoute where
  getMasterName MyRoute = "MyRoute"

mkRoute "MyRoute" [parseRoutes|
/          HomeR GET
/some-json FooR  GET
/post      PostR POST
/subsite   SubR SubRoute getSubRoute
/nested       NestedR:
  /             NRootR     GET
  /abcd         AbcdR      GET
|]

getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ plain "hello"

getFooR :: Handler MyRoute
getFooR = runHandlerM $ json $ object ["foo" .= Number 23, "bar" .= Number 42]

-- Post parameters (getParam can also be used for query params)
postPostR :: Handler MyRoute
postPostR = runHandlerM $ do
  name <- getParam "name"
  plain $ fromMaybe "unnamed" name

-- Nested routes
getNRootR, getAbcdR :: Handler MyRoute
getNRootR = runHandlerM $ plain "Nested ROOT"
getAbcdR  = runHandlerM $ plain "Nested ABCD"

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

  describe "POST /post?name=foobar" $
    it "can read query parameters" $
      postHtmlForm "/post?name=foobar" [("name","ignored")] `shouldRespondWith` "foobar" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "POST /post" $
    it "can read post body parameters" $
      postHtmlForm "/post" [("name","foobar")] `shouldRespondWith` "foobar" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /subsite" $
    it "can access the subsite correctly" $
      get "/subsite" `shouldRespondWith` "subsite-MyRoute" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /nested" $
    it "can access nested routes root correctly" $
      get "/nested" `shouldRespondWith` "Nested ROOT" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /nested/abcd" $
    it "can access nested routes correctly" $
      get "/nested/abcd" `shouldRespondWith` "Nested ABCD" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /lambda.png" $
    it "returns a file correctly" $
      get "/lambda.png" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "image/png"]}

