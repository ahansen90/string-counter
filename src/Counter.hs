{-# LANGUAGE OverloadedRecordDot #-}

{- |
   Description: Module to manage concurrent access to the string-counting bookkeeping.
-}

module Counter
  ( Counter
  , newCounter
  , readCount
  , incrementCount
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.Reader (ReaderT, ask, lift)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

{- |
  The core type for counting the number of times a particular key has
  been seen. 'Integer' is used here over 'Int' since the number of times
  'incrementCount' may be called is unbounded.
-}
data Counter = Counter
  { counts :: TVar (Map Text Integer)
  }

newCounter :: IO Counter
newCounter = Counter <$> newTVarIO Map.empty

{- |
  Read the number of times the given @key@ has been seen, returning @0@
  if the key has never been seen.
-}
readCount :: Text -> ReaderT Counter IO Integer
readCount key = do
  env <- ask
  counts <- lift $ readTVarIO env.counts
  pure $ Map.findWithDefault 0 key counts

{- |
  Increment the number of times the given @key@ has been seen, setting
  the count to @1@ if it has never been seen before.
-}
incrementCount :: Text -> ReaderT Counter IO ()
incrementCount key = do
  env <- ask
  lift . atomically . modifyTVar' env.counts $ Map.insertWith (+) key 1
