{-# LANGUAGE TemplateHaskell #-}
module Gifter.Daemon.SteamGamesTask
    ( startSteamGamesTask
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar

import Data.Time.Clock

import Text.Printf.Mauke.TH

import Gifter.Daemon.Common
import Gifter.Daemon.Task
import Gifter.TaggedValue
import Gifter.SteamGames
import Gifter.Logging
import Gifter.Config

data SteamGamesTaskRead = SteamGamesTaskRead
    { _configVar :: TEVar Config
    , _steamGamesVar :: TEVar SteamGames
    }
makeLenses ''SteamGamesTaskRead

getSteamGamesVar :: TaskSG (TEVar SteamGames)
getSteamGamesVar = asks (^.steamGamesVar)

type TaskSG = Task SteamGamesTaskRead (TaggedValue UTCTime SteamGames)

startSteamGamesTask :: Config
                    -> TEVar Config
                    -> TEVar SteamGames
                    -> IO ()
startSteamGamesTask cfg cv sgv = do
    tn <- getCurrentTime
    let te = addUTCTime (fromInteger $ -10) tn
        r = SteamGamesTaskRead cv sgv
    runTask cfg r (tagValue te emptySteamGames) steamGamesTask

steamGamesTask :: TaskSG ()
steamGamesTask = forever steamGamesAction

steamGamesAction :: TaskSG ()
steamGamesAction = do
    updateConfig (^.configVar)
    cfg <- getConfig
    tsg <- getIntState
    t <- liftIO $ getCurrentTime
    if isExpired t tsg
        then updateSteamGames
        else return ()
    delay (cfg^.requestDelay)

updateSteamGames :: TaskSG ()
updateSteamGames = do
    cfg <- getConfig
    logTime "Updating steam game list"
    sge <- liftIO . try $ getSteamGames (cfg^.sessionId)
    either reportException steamGamesSuccess sge

steamGamesSuccess :: SteamGames -> TaskSG ()
steamGamesSuccess sg = do
    logTime $ $(printf "You currently own %d games") (numberOwned sg)
    logTime $ $(printf "You have %d games in wishlist") (numberWishlist sg)
    sgv <- getSteamGamesVar
    cfg <- getConfig
    t <- liftIO $ getCurrentTime
    let ft = addUTCTime (fromIntegral $ cfg^.steamGamesExpire * 60) t
    modifyIntState (const $ tagValue ft sg)
    liftIO . atomically $ writeTEVar sgv sg
