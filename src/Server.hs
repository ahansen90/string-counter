{- |
   Description: API implementation for the string counter server.
-}

module Server
  ( server
  ) where

import Api (Api (Api, input, query))
import Control.Monad.Reader (ReaderT)
import Counter (Counter, incrementCount, readCount)
import Servant (NoContent (NoContent))
import Servant.Server.Generic (AsServerT)

server :: Api (AsServerT (ReaderT Counter IO))
server = Api {input, query}
  where
    input key = incrementCount key >> pure NoContent
    query = readCount
