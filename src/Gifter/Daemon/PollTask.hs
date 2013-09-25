{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Gifter.Daemon.PollTask
    ( startTask
    ) where

import Control.Lens
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Exception

import qualified Data.Text as T
import Data.List as L

import Text.Printf

import Safe

import Gifter.Daemon.Task
import Gifter.Daemon.Common
import Gifter.Logging
import Gifter.GiveawayEntry
import Gifter.Config

type TaskPS = Task PollTaskState

data PollTaskState = PollTaskState {
                     _dataChannel :: TChan GiveawayEntry
                   , _configChannel :: TChan Config
                   , _lastCheckedUrl :: Maybe T.Text
                   }
makeLenses ''PollTaskState

startTask :: Config
          -> TChan GiveawayEntry
          -> TChan Config
          -> Maybe T.Text
          -> IO ()
startTask cfg dc cc lc = do
    let s = PollTaskState dc cc lc
    runTask cfg s pollTask

pollTask :: TaskPS ()
pollTask = forever pollAction

pollAction :: TaskPS ()
pollAction = do
    cfgChan <- getsIntState (^.configChannel)
    logTime "Getting config updates"
    mcfg <- liftIO . atomically $ tryReadTChan cfgChan
    maybeReplaceConfig mcfg
    cfg <- getConfig
    ges <- liftIO $ getEntries 999
    either handleEntriesError handleEntriesSuccess ges
    delay (cfg^.pollDelay)

handleEntriesError :: SomeException -> TaskPS ()
handleEntriesError e =
    logTime $ printf "Error getting latest giveaways: %s" (show e)

handleEntriesSuccess :: [GiveawayEntry] -> TaskPS ()
handleEntriesSuccess gs = do
    newGs <- keepNew gs
    updateLastCheckedUrl (headMay gs)
    logTime $ printf "Got %d new giveaways" (length newGs)
    dataChan <- getsIntState (^.dataChannel)
    mapM_ (liftIO . atomically . writeTChan dataChan) newGs

keepNew :: [GiveawayEntry] -> TaskPS [GiveawayEntry]
keepNew ges = do
    lastChecked <- getsIntState (^.lastCheckedUrl)
    return $ maybe ges onlyNew lastChecked
  where
    onlyNew u = L.takeWhile ((u /=) . (^.gUrl)) ges

updateLastCheckedUrl :: Maybe GiveawayEntry -> TaskPS ()
updateLastCheckedUrl mge = do
    cfg <- getConfig
    let mu = (^.gUrl) `fmap` mge
    modifyIntState $ over lastCheckedUrl (mu `mplus`)
    liftIO $ writeLastChecked (lastCheckedFile cfg) mu
