module Control.Concurrent.STM.TEVar
    ( TEVar
    , newEmptyTEVarIO
    , writeTEVar
    , readTEVar
    ) where

import Control.Monad
import Control.Concurrent.STM

newtype TEVar a = TEVar (TMVar a)

newEmptyTEVarIO :: IO (TEVar a)
newEmptyTEVarIO = TEVar `liftM` newEmptyTMVarIO

writeTEVar :: TEVar a -> a -> STM ()
writeTEVar (TEVar v) a = tryTakeTMVar v >> putTMVar v a

readTEVar :: TEVar a -> STM a
readTEVar (TEVar v) = readTMVar v
