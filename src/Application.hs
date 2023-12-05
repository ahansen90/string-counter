{- |
   Description: Application initialization and configuration for string
   counter server.
-}

module Application
  ( start
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Counter (newCounter)
import Network.Wai.Handler.Warp (run)
import Servant.Server (Application)
import Servant.Server.Generic (genericServeT)
import Server (server)

makeApplication :: IO Application
makeApplication = do
  counter <- newCounter
  pure $ genericServeT (liftIO . flip runReaderT counter) server

start :: Int -> IO ()
start port = makeApplication >>= run port
