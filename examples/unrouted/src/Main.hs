module Main where
{-
  A demonstration of using only "unrouted" handlers, without any TH

  Requires no GHC extensions
  Requires no Template Haskell or quasiquotes
  Cannot have a Master Site argument
  Requires no Routing code

  To see a more complex example of mixing unrouted and routed handlers, see hello-world-example
-}

import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V

import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp

-- Run the application
main :: IO ()
main = do
  -- Global state
  times <- liftIO $ newIORef 0

  -- We create a global vault key to store the counter
  -- This key is only created once when the application starts
  timesKey <- liftIO $ V.newKey

  -- Run the app
  putStrLn "Starting server on port 8080"
  run 8080 $ waiApp $ do

    -- Handlers (like routes) are cascaded

    -- The first handler is always called and can also be used to perform common
    --   global processing (such as incrementing the counter on every request)
    handler $ runHandlerM $ do
      -- Increment the global counter
      n <- liftIO $ readIORef times
      liftIO $ writeIORef times (n+1)
      -- Insert the key in the vault for all subsequent handlers to access
      updateVault $ V.insert timesKey n
      -- Remember to call finally next, so other handlers are invoked
      next

    -- Full handler functionality is available
    handler $ runHandlerM $ do
      -- You can access untyped (but parsed) route information
      Just (DefaultRoute (pieces, query)) <- maybeRoute
      Just n <- lookupVault timesKey
      if mod n 10 == 0
         -- Every 10th invocation, jump to the next handler
         then next
         else do
           html $ T.concat $ map T.pack $
             [ "<h1>Hello! You have been here ", show n, " times</h1>"
             , "<p>Raw route path pieces - ", show pieces, "</p>"
             , "<p>Raw route query - ", show query, "</p>"
             ]

    -- This handler will only be reached when the previous handler calls next (on every 10th invocation)
    handler $ runHandlerM $ do
      Just n <- lookupVault timesKey
      html $ T.concat $ map T.pack ["<h1>You are the special ", show n, "th caller!</h1>"]
