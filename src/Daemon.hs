{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (
    main
) where

import Control.Lens
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar

import System.IO
import System.Console.CmdArgs
import System.Environment

import Data.Data ()
import Data.Typeable ()

import Gifter.Daemon.Common
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
    ecfg <- readConfig (cargs^.config)
    case ecfg of
        Right cfg -> startTasks cfg (cargs^.config)
        Left (MissingFile fp) -> putStrLn $ "Missing config file: " ++ fp
        Left ConfigParseError -> putStrLn "Config parse error"

startTasks :: Config -> FilePath -> IO ()
startTasks cfg fp = do
    giveChan <- newTChanIO
    cfgVar <- newEmptyTEVarIO
    sgVar <- newEmptyTEVarIO
    lastChPer <- readLastChecked $ lastCheckedFile cfg
    _ <- async (startConfigWatcherTask fp cfgVar)
    _ <- async (startSteamGamesTask cfg cfgVar sgVar)
    r1 <- async (startPollTask cfg giveChan cfgVar lastChPer)
    r2 <- async (startEnterTask cfg giveChan cfgVar sgVar)
    res <- waitEitherCatchCancel r1 r2
    handleErrors res
  where
    handleErrors (Left (Left e)) =
        logTime $ "Poll thread failed with exception: " ++ show e
    handleErrors (Right (Left e)) =
        logTime $ "Enter giveaways thread failed with exception: " ++ show e
    handleErrors _ = logTime "Unexpected return value"
