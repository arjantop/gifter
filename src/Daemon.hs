{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (
    main
) where

import Control.Lens
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM

import System.IO
import System.Console.CmdArgs
import System.Environment

import Data.Data ()
import Data.Typeable ()
import qualified Data.HashSet as HS

import Gifter.Daemon.Common
import Gifter.Daemon.PollTask
import Gifter.Daemon.EnterTask
import Gifter.Logging
import Gifter.Config
import Gifter.SteamGames

data GifterdArgs = GifterdArgs { _config :: String }
                     deriving (Show, Data, Typeable)
makeLenses ''GifterdArgs

gifterArgs :: String -> String -> GifterdArgs
gifterArgs defCfg progName = GifterdArgs {
                        _config = defCfg &= help "Config file location"
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
        Right cfg -> tryGetSteamGames cfg
        Left (MissingFile fp) -> putStrLn $ "Missing config file: " ++ fp
        Left ConfigParseError -> putStrLn "Config parse error"

tryGetSteamGames :: Config -> IO ()
tryGetSteamGames cfg = do
    logTime "Trying to get steam game list"
    loop (cfg^.maxRetries)
  where
    {-loop 0 = logTime "Error getting steam games list"-}
    loop n = do
        sge <- getSteamGames (cfg^.sessionId)
        case sge of
            Right sg -> do
                let nGames = HS.size (sg^.sOwned)
                    nWish = HS.size (sg^.sWishlist)
                logTime $ "You currently own " ++ show nGames ++ " games"
                logTime $ "You have " ++ show nWish ++ " games in wishlist"
                startTasks cfg sg
            Left e -> do
                logTime "Could not get steam game list. Retrying"
                logTime $ show e
                delay (cfg^.requestDelay)
                loop (n - 1)

startTasks :: Config -> SteamGames -> IO ()
startTasks cfg sg = do
    giveChan <- newTChanIO
    giveChan' <- newTChanIO
    cfgChan <- newTChanIO
    lastChPer <- readLastChecked $ lastCheckedFile cfg
    r1 <- async (startTask cfg giveChan cfgChan lastChPer)
    _ <- async (forever $ atomically (readTChan giveChan >>= \x -> writeTChan giveChan' (NewData [x])))
    r2 <- async (enterSelectedGiveaways giveChan' cfg sg Nothing)
    res <- waitEitherCatchCancel r1 r2
    handleErrors res
  where
    handleErrors (Left (Left e)) =
        logTime $ "Poll thread failed with exception: " ++ show e
    handleErrors (Right (Left e)) =
        logTime $ "Enter giveaways thread failed with exception: " ++ show e
    handleErrors _ = logTime "Unexpected return value"
