{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Gifter.Daemon.PollTask
    ( startPollTask
    ) where

import Control.Lens
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Exception

import qualified Data.Text as T
import Data.List as L

import Text.Printf.Mauke.TH

import Safe

import Gifter.Daemon.Task
import Gifter.Daemon.Common
import Gifter.Daemon.ConfigWatcherTask
import Gifter.Logging
import Gifter.GiveawayEntry
import Gifter.Config

type TaskP = Task () PollTaskState

data PollTaskState = PollTaskState
    { _dataChannel :: TChan GiveawayEntry
    , _configVar :: ConfigVar
    , _lastCheckedUrl :: Maybe T.Text
    }
makeLenses ''PollTaskState

startPollTask :: Config
              -> TChan GiveawayEntry
              -> ConfigVar
              -> Maybe T.Text
              -> IO ()
startPollTask cfg dc cc lc = do
    let s = PollTaskState dc cc lc
    runTask cfg () s pollTask

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
    updateLastCheckedUrl (headMay gs)
    let newGsLen = length newGs
    when (newGsLen > 0) . logTime $ $(printf "Got %d new giveaways") newGsLen
    dataChan <- getsIntState (^.dataChannel)
    mapM_ (liftIO . atomically . writeTChan dataChan) newGs

keepNew :: [GiveawayEntry] -> TaskP [GiveawayEntry]
keepNew ges = do
    lastChecked <- getsIntState (^.lastCheckedUrl)
    return $ maybe ges onlyNew lastChecked
  where
    onlyNew u = L.takeWhile ((u /=) . (^.gUrl)) ges

updateLastCheckedUrl :: Maybe GiveawayEntry -> TaskP ()
updateLastCheckedUrl mge = do
    cfg <- getConfig
    let mu = (^.gUrl) `fmap` mge
    modifyIntState $ over lastCheckedUrl (mu `mplus`)
    liftIO $ writeLastChecked (lastCheckedFile cfg) mu
