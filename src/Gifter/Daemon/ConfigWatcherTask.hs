{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Gifter.Daemon.ConfigWatcherTask
    ( startConfigWatcherTask
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar
import Control.Lens

import qualified Filesystem.Path as FP

import System.FSNotify

import Text.Printf.Mauke.TH

import Data.String

import Gifter.Daemon.Task
import Gifter.Config
import Gifter.Logging

type TaskCW = Task FilePath ConfigWatcherState

configPath :: TaskCW FilePath
configPath = ask

data ConfigWatcherState = ConfigWatcherState
    { _latestConf :: TEVar Config }
makeLenses ''ConfigWatcherState

startConfigWatcherTask :: FilePath
                       -> TEVar Config
                       -> IO ()
startConfigWatcherTask fp v = do
    let s = ConfigWatcherState v
    runTask emptyConfig fp s configWatcherTask

configWatcherTask :: TaskCW ()
configWatcherTask = do
    readUpdateConfig
    wm <- liftIO $ startManager
    ch <- startFileWatcher wm
    forever $ handleFileEvents ch

startFileWatcher :: WatchManager -> TaskCW (Chan Event)
startFileWatcher wm = do
    ch <- liftIO $ newChan
    cfp <- fromString `liftM` configPath
    liftIO $ watchDirChan wm (FP.directory cfp) isModified ch
    return ch
  where
    isModified (Modified _ _) = True
    isModified _              = False

handleFileEvents :: Chan Event -> TaskCW ()
handleFileEvents ch = do
    e <- liftIO $ readChan ch
    handleEvent e

readUpdateConfig :: TaskCW ()
readUpdateConfig = do
    logTime "Reading config"
    cfp <- configPath
    ecfg <- liftIO $ readConfig cfp
    case ecfg of
        Right cfg -> do
            var <- getsIntState (^.latestConf)
            liftIO . atomically $ writeTEVar var cfg
        Left e -> handleConfigError e

handleConfigError :: MonadIO m => ConfigError -> m ()
handleConfigError e = case e of
    MissingFile fp -> logTime $ $(printf "Missing config file: %s") fp
    ConfigParseError -> logTime "Config parse error"

handleEvent :: Event -> TaskCW ()
handleEvent e = do
    cfp <- configPath
    if eventPath e == fromString cfp
        then do
            logTime "Config modified"
            readUpdateConfig
        else return ()
