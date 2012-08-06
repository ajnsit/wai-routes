{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
module Main where

import Network.Wai
import Network.Wai.Middleware.Routes
import Network.Wai.Middleware.Routes.Monad
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
  toJSON x = object [ "uid" .= (userId x), "name" .= (userName x), "age" .= (userAge x) ]


-- The Site argument
data MyRoute = MyRoute (IORef DB)

-- Make MyRoute Routable
mkRoute "MyRoute" [parseRoutes|
/             HomeR            GET
/users        UsersR           GET
/user/#Int    UserR:
    /              UserRootR   GET
    /delete        UserDeleteR GET POST
|]

-- Our handlers always produce json
jsonHeaders = [("Content-Type", "application/json")]

-- Handlers

-- Display the possible actions
getHomeR :: Handler MyRoute
getHomeR _master _req = return $ responseLBS status200 jsonHeaders json
  where json = encode $ M.fromList (
                 [("description", [["Simple User database Example"]])
                 ,("links"
                  ,[["home",  showRoute HomeR]
                   ,["users", showRoute UsersR]
                   ]
                  )
                 ] :: [(Text, [[Text]])] )

-- Display all the users
getUsersR :: Handler MyRoute
getUsersR (MyRoute dbref) req_ = do
  db <- liftIO $ readIORef dbref
  let dblinks = map linkify db
  let json = encode $ M.fromList (
          [("description", [["Users List"]])
          ,("links", dblinks)] :: [(Text, [[Text]])] )
  return $ responseLBS status200 jsonHeaders json
  where
    linkify user = [userName user, showRoute $ UserR (userId user) UserRootR]

-- Display a single user
getUserRootR :: Int -> Handler MyRoute
getUserRootR i (MyRoute dbref) _req = do
  db <- liftIO $ readIORef dbref
  let user = ulookup i db
  case ulookup i db of
    Nothing -> return $ responseLBS status200 jsonHeaders $ encode ("ERROR: User not found" :: Text)
    Just user -> do
      let json = encode $ M.fromList (
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
      return $ responseLBS status200 jsonHeaders json
  where
    ulookup _ [] = Nothing
    ulookup i (u:us) = if userId u == i then Just u else ulookup i us

-- Delete a user: GET
getUserDeleteR :: Int -> Handler MyRoute
getUserDeleteR _ master req_ = return $ responseLBS status200 jsonHeaders json
  where err = (["DELETE","please use POST"]::[Text])
        json = encode err

-- Delete a user: POST
postUserDeleteR :: Int -> Handler MyRoute
postUserDeleteR _ master req_ = return $ responseLBS status200 jsonHeaders json
  where err = (["DELETE","not implemented"]::[Text])
        json = encode err

-- Initial database
initdb =
    [ User 1 "Anon Amos" 23
    , User 2 "Bo Lively" 28
    ]

-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
  db <- liftIO $ newIORef initdb
  addroute (MyRoute db)
  setDefaultAction $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = toWaiApp application >>= run 8080

