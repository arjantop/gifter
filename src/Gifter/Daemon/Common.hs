{-# LANGUAGE TemplateHaskell #-}
module Gifter.Daemon.Common
    ( delay
    , updateConfig
    , reportException
    ) where

import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar
import Control.Exception

import Text.Printf.Mauke.TH

import Gifter.Config
import Gifter.Daemon.Task
import Gifter.Logging

delay :: (MonadIO m) => Integer -> m ()
delay ms = liftIO $ threadDelay (fromIntegral ms * 1000000)

updateConfig :: (r -> TEVar Config) -> Task r s ()
updateConfig f = do
    cfgVar <- asks f
    cfg' <- liftIO . atomically $ readTEVar cfgVar
    replaceConfig cfg'

reportException :: SomeException -> Task r s ()
reportException e =
    logTime $ $(printf "Failed with exception: %s") (show e)
