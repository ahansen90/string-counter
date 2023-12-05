{-# LANGUAGE DataKinds #-}

{- |
   Description: Api description for the string counter server.
-}

module Api
  ( Api (..)
  , api
  ) where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
  ( Get
  , JSON
  , PlainText
  , PostNoContent
  , QueryParam'
  , ReqBody
  , Required
  , Strict
  , ToServantApi
  , genericApi
  , (:-)
  , (:>)
  )

data Api route = Api
  { input :: route :- "input" :> ReqBody '[PlainText] Text :> PostNoContent
  , query
      :: route
        :- "query"
        :> QueryParam' '[Required, Strict] "key" Text
        :> Get '[JSON] Integer
  }
  deriving (Generic)

api :: Proxy (ToServantApi Api)
api = genericApi (Proxy :: Proxy Api)
