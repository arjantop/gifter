{-# LANGUAGE TemplateHaskell #-}
module Gifter.Daemon.PollTask (
    pollGiveawayEntries,
    PollState(..)
) where

import Control.Lens
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State


import Data.Maybe
import qualified Data.Text as T
import Data.List as L

import Safe

import Gifter.Daemon.Common
import Gifter.Logging
import Gifter.GiveawayEntry
import Gifter.Config

data PollState = PollState {
        _lastChecked :: Maybe T.Text,
        _giveawayChannel :: TChan (DataEvent GiveawayEntry)
    }
makeLenses ''PollState

pollGiveawayEntries :: TaskM PollState ()
pollGiveawayEntries = do
    gse <- liftIO $ getEntries 999
    case gse of
        Left e -> do
            cfg <- ask
            logTime "Error getting latest giveaways"
            logTime . show $ e
            delay (cfg^.pollDelay)
            pollGiveawayEntries
        Right gs -> handleEntries gs
  where
    handleEntries gs = do
        cfg <- ask
        gc <- gets (^.giveawayChannel)
        lg <- gets (^.lastChecked)
        let newGs = maybe gs (\u -> L.takeWhile ((u /=) . (^.gUrl)) gs) lg
            numGs = length newGs
            lUrl = (^.gUrl) `fmap` headMay newGs
            newLs = lUrl `mplus` lg
        when (isJust lUrl) . liftIO $
            writeLastChecked (lastCheckedFile cfg) (fromJust newLs)
        logTimeWhen (numGs > 0) $ "Got " ++ show numGs ++ " new giveaways"
        liftIO $ atomically $ writeTChan gc (NewData newGs)
        delay (cfg ^. pollDelay)
        modify (set lastChecked newLs)
        pollGiveawayEntries
