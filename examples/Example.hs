{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
module Main where

import Network.Wai
import Network.Wai.Middleware.Routes
import Network.Wai.Middleware.Routes.ContentTypes
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import Data.IORef
import qualified Data.Map as M
import Control.Monad.Trans

-- The database of users
data User = User
  { userId   :: Int
  , userName :: Text
  , userAge  :: Int
  } deriving (Show, Read, Eq)
type DB = [User]

-- JSON instance
instance ToJSON User where
  toJSON x = object [ "uid" .= userId x, "name" .= userName x, "age" .= userAge x ]


-- The Site argument
data MyRoute = MyRoute (IORef DB)

-- Make MyRoute Routable
mkRoute "MyRoute" [parseRoutes|
/             HomeR            GET
/users        UsersR           GET
/user/#Int    UserR:
    /              UserRootR   GET
    /delete        UserDeleteR GET POST
/skip        SkipR             GET
|]


-- Handlers

-- Standard Json output
jsonOut :: ToJSON a => a -> Response
jsonOut = responseLBS status200 jsonHeaders . encode
  where
    jsonHeaders :: ResponseHeaders
    jsonHeaders = [contentType typeJson]

-- Display the possible actions
getHomeR :: Handler MyRoute
getHomeR _master _req = return $ jsonOut $ M.fromList (
                 [("description", [["Simple User database Example"]])
                 ,("links"
                  ,[["home",  showRoute HomeR]
                   ,["users", showRoute UsersR]
                   ,["skip",  showRoute SkipR]
                   ]
                  )
                 ] :: [(Text, [[Text]])] )

-- Display all the users
getUsersR :: Handler MyRoute
getUsersR (MyRoute dbref) _req = do
  db <- liftIO $ readIORef dbref
  let dblinks = map linkify db
  let out = M.fromList (
          [("description", [["Users List"]])
          ,("links", dblinks)] :: [(Text, [[Text]])] )
  return $ jsonOut out
  where
    linkify user = [userName user, showRoute $ UserR (userId user) UserRootR]

-- Display a single user
getUserRootR :: Int -> Handler MyRoute
getUserRootR i (MyRoute dbref) _req = do
  db <- liftIO $ readIORef dbref
  case ulookup i db of
    Nothing -> return $ jsonOut ("ERROR: User not found" :: Text)
    Just user -> do
      let out = M.fromList (
            [("description", [["User details"]])
            ,("data"
             ,[["Id",   T.pack $ show $ userId user]
              ,["Name", userName user]
              ,["Age",  T.pack $ show $ userAge user]
              ]
             )
            ,("links"
             ,[["details",            showRoute $ UserR (userId user) UserRootR]
              ,["delete (post only)", showRoute $ UserR (userId user) UserDeleteR]
              ]
             )
            ] :: [(Text, [[Text]])] )
      return $ jsonOut out
  where
    ulookup _ [] = Nothing
    ulookup ui (u:us) = if userId u == ui then Just u else ulookup ui us

-- Delete a user: GET
getUserDeleteR :: Int -> Handler MyRoute
getUserDeleteR _ _master _req = return $ jsonOut err
  where err = ["DELETE","please use POST"]::[Text]

-- Delete a user: POST
postUserDeleteR :: Int -> Handler MyRoute
postUserDeleteR _ _master _req = return $ jsonOut err
  where err = ["DELETE","not implemented"]::[Text]

-- Demonstrate skipping routes
getSkipR :: Handler MyRoute
getSkipR _master = runNext

-- Initial database
initdb :: [User]
initdb =
    [ User 1 "Anon Amos" 23
    , User 2 "Bo Lively" 28
    ]

-- A new middleware to catch all skipped routes
data MySkippedRoute = MySkippedRoute

-- Make MyRoute Routable
mkRoute "MySkippedRoute" [parseRoutes|
/skip         SkippedR          GET
|]

getSkippedR :: Handler MySkippedRoute
getSkippedR _req _master =
  return $ jsonOut err
  where err = ["SKIPPED","skipped route"]::[Text]

-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
  db <- liftIO $ newIORef initdb
  route (MyRoute db)
  route MySkippedRoute
  defaultAction $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = toWaiApp application >>= run 8080

