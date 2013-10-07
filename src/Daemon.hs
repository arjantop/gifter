{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Main
    ( main
    ) where

import Control.Lens
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar

import System.IO
import System.Console.CmdArgs
import System.Environment

import Data.Acid.Local

import Gifter.Daemon.LastCheckedUrl
import Gifter.Daemon.PollTask
import Gifter.Daemon.EnterTask
import Gifter.Daemon.ConfigWatcherTask
import Gifter.Daemon.SteamGamesTask
import Gifter.Logging
import Gifter.Config

data GifterdArgs = GifterdArgs { _config :: String }
                     deriving (Show, Data, Typeable)
makeLenses ''GifterdArgs

gifterArgs :: String -> String -> GifterdArgs
gifterArgs defCfg progName = GifterdArgs
    { _config = defCfg &= help "Config file location"
                       &= explicit
                       &= name "config"
                       &= typFile
                       &= groupname "Program options"
    } &= summary "Simple program for automatic giveaway entry on steamgifts.com"
      &= program progName
      &= helpArg [explicit,
                  name "h",
                  name "help",
                  groupname "Other"]
      &= versionArg [ignore]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    defCfg <- defaultLocation
    progName <- getProgName
    cargs <- cmdArgs $ gifterArgs defCfg progName
    startTasks (cargs^.config)

startTasks :: FilePath -> IO ()
startTasks fp = do
    giveChan <- newTChanIO
    cfgVar <- newEmptyTEVarIO
    sgVar <- newEmptyTEVarIO
    cwt <- async (startConfigWatcherTask fp cfgVar)
    cfg <- atomically $ readTEVar cfgVar
    bracket (openLocalStateFrom (cfg^.stateDir) initialLastCheckedUrlState)
            (createCheckpointAndClose) $ \acidLastUrl -> do
                sgt <- async (startSteamGamesTask cfg cfgVar sgVar)
                pt <- async (startPollTask cfg giveChan cfgVar acidLastUrl)
                et <- async (startEnterTask cfg giveChan cfgVar sgVar)
                res <- waitAnyCatchCancel [cwt, sgt, pt, et]
                handleErrors res
  where
    handleErrors (_, (Left e)) = logTime $ "Failed with exception: " ++ show e
    handleErrors _ = logTime "Unexpected return value"
