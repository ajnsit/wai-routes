{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

import Network.Wai
import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.HTTP.Types
import Data.Aeson
import Data.Text

-- A useful type synonym
type UserId = Text

-- Define the JSON instance
data User = User { name::Text, uid:: UserId } deriving (Show, Read, Eq)
instance ToJSON User where
  toJSON x = object [ "name" .= (name x), "uid" .= (uid x) ]

-- Define the handlers
getUserR :: UserId -> Application
getUserR uid _req =
  return $ responseLBS statusOK headers json
  where user = User { name = "Anon Amos", uid = uid }
        json = encode user
        headers = [("Content-Type", "application/json")]

getUsersR :: Application
getUsersR _req =
  return $ responseLBS statusOK headers json
  where userids = (["anon","john","jane"]::[Text])
        json = encode userids
        headers = [("Content-Type", "application/json")]

-- Generate the routing datatype, Route instance, and the specialised dispatcher
-- The type generated will be named "UserRoute"
-- The dispatcher will be "dispatchUserRoute"
mkRoute "User" [parseRoutes|
  /users UsersR GET
  /user/#UserId UserR GET
|]

-- Now you can use generated dispatcher
main :: IO ()
main = run 8080 $ dispatchUserRoute $ staticApp defaultFileServerSettings

