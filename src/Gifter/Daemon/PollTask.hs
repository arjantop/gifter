{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Gifter.Daemon.PollTask
    ( startPollTask
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar

import Data.List as L

import Text.Printf.Mauke.TH

import Safe

import Gifter.Daemon.Task
import Gifter.Daemon.Common
import Gifter.Daemon.LastCheckedUrl
import Gifter.Logging
import Gifter.GiveawayEntry
import Gifter.Config

data PollTaskRead = PollTaskRead
    { _dataChannel :: TChan GiveawayEntry
    , _configVar :: TEVar Config
    , _acidLastCheckedUrl :: AcidUrl
    }
makeLenses ''PollTaskRead

type TaskP = Task PollTaskRead ()

getDataChannel :: TaskP (TChan GiveawayEntry)
getDataChannel = asks (^.dataChannel)

getConfigVar :: TaskP (TEVar Config)
getConfigVar = asks (^.configVar)

startPollTask :: Config
              -> TChan GiveawayEntry
              -> TEVar Config
              -> AcidUrl
              -> IO ()
startPollTask cfg dc cc ac = do
    let r = PollTaskRead dc cc ac
    runTask cfg r () pollTask

pollTask :: TaskP ()
pollTask = forever pollAction

pollAction :: TaskP ()
pollAction = do
    updateConfig (^.configVar)
    cfg <- getConfig
    ges <- liftIO $ getEntries 999
    either handleEntriesError handleEntriesSuccess ges
    delay (cfg^.pollDelay)

handleEntriesError :: SomeException -> TaskP ()
handleEntriesError e =
    logTime $ $(printf "Error getting latest giveaways: %s") (show e)

handleEntriesSuccess :: [GiveawayEntry] -> TaskP ()
handleEntriesSuccess gs = do
    newGs <- keepNew gs
    updateLastChecked (headMay gs)
    let newGsLen = length newGs
    when (newGsLen > 0) . logTime $ $(printf "Got %d new giveaways") newGsLen
    dataChan <- getDataChannel
    mapM_ (liftIO . atomically . writeTChan dataChan) newGs

keepNew :: [GiveawayEntry] -> TaskP [GiveawayEntry]
keepNew ges = do
    lastChecked <- getLastCheckedUrl acidLastCheckedUrl
    return $ onlyNew lastChecked
  where
    onlyNew u = L.takeWhile ((u /=) . (^.gUrl)) ges

updateLastChecked :: Maybe GiveawayEntry -> TaskP ()
updateLastChecked mge = case mge of
    Just ge -> do
        let u = ge^.gUrl
        updateLastCheckedUrl acidLastCheckedUrl u
    Nothing -> return ()
