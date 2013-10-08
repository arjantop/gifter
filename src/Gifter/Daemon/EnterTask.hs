{-# LANGUAGE TemplateHaskell #-}
module Gifter.Daemon.EnterTask
    ( startEnterTask
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar
import Control.Exception

import Data.Text.Lens
import Data.Time.Clock

import Text.Printf.Mauke.TH

import Gifter.Daemon.Task
import Gifter.Daemon.Common
import Gifter.Logging
import Gifter.GiveawayEntry
import Gifter.Giveaway
import Gifter.Giveaway.Parser (DataError(..))
import Gifter.Config
import Gifter.Config.EntryCondition
import Gifter.SteamGames
import Gifter.TaggedValue

data Retry = Retry
           | Done
           deriving (Show, Eq)

retryN :: (Monad m) => Int -> m Retry -> m Retry
retryN 0 _ = return Done
retryN n m = do
    b <- m
    if b == Retry
        then retryN (n - 1) m
        else return Done

retryN_ :: (Monad m) => Int -> m Retry -> m ()
retryN_ n m = retryN n m >> return ()

data EnterTaskRead = EnterTaskRead
    { _dataChannel :: TChan GiveawayEntry
    , _configVar :: TEVar Config
    , _steamGamesVar :: TEVar SteamGames
    }
makeLenses ''EnterTaskRead

data EnterTaskState = EnterTaskState
    { _currentAccPoints :: TaggedValue UTCTime Integer
    , _steamGames :: SteamGames
    }
makeLenses ''EnterTaskState

getDataChannel :: TaskE (TChan GiveawayEntry)
getDataChannel = asks (^.dataChannel)

getSteamGamesVar :: TaskE (TEVar SteamGames)
getSteamGamesVar = asks (^.steamGamesVar)

type TaskE = Task EnterTaskRead EnterTaskState

startEnterTask :: Config
               -> TChan GiveawayEntry
               -> TEVar Config
               -> TEVar SteamGames
               -> IO ()
startEnterTask cfg dc cv sgv = do
    tn <- getCurrentTime
    let te = addUTCTime (fromInteger $ -10) tn
        r = EnterTaskRead dc cv sgv
        s = EnterTaskState (tagValue te 0) emptySteamGames
    runTask cfg r s enterTask

enterTask :: TaskE ()
enterTask = forever enterAction

enterAction :: TaskE ()
enterAction = do
    updateConfig (^.configVar)
    updateSteamGames
    updateAccPointsIfExpired
    cfg <- getConfig
    dc <- getDataChannel
    ge <- liftIO . atomically $ readTChan dc
    sg <- getsIntState (^.steamGames)
    cap <- getValue `fmap` getsIntState (^.currentAccPoints)
    if matchAny ge sg (Just $ cap) (cfg^.enter) && not (isAlreadyOwned sg ge)
        then let maxR = fromIntegral $ cfg^.maxRetries
             in retryN_ maxR $ tryGetGiveawayInfo ge
        else return ()
    delay (cfg^.requestDelay)

updateSteamGames :: TaskE ()
updateSteamGames = do
    sgv <- getSteamGamesVar
    sg <- liftIO . atomically $ readTEVar sgv
    modifyIntState (set steamGames sg)

tryGetGiveawayInfo :: GiveawayEntry -> TaskE Retry
tryGetGiveawayInfo ge = do
    logTime $ $(printf "Getting giveaway info: %s") (ge^.gUrl.unpacked)
    cfg <- getConfig
    res <- liftIO $ getGiveaway (ge^.gUrl) cfg
    r <- either handleGiveawayError handleInfoSuccess res
    delay (cfg^.requestDelay)
    return r

handleInfoSuccess :: Giveaway -> TaskE Retry
handleInfoSuccess g = do
    updateCurrentAccPoints (g^.accPoints)
    cfg <- getConfig
    if canEnter g
        then let maxR = fromIntegral $ cfg^.maxRetries
             in retryN maxR $ tryEnterGiveaway g
        else do
            let s = g^.status
                u = g^.url.unpacked
            logTime $ $(printf "Wrong status %s for %s") (show s) u
            return Done

tryEnterGiveaway :: Giveaway -> TaskE Retry
tryEnterGiveaway g = do
    logTime $ $(printf "Trying to enter %s") (g^.url.unpacked)
    cfg <- getConfig
    res <- liftIO $ enterGiveaway g cfg
    r <- either handleGiveawayError handleEnterSuccess res
    delay (cfg^.requestDelay)
    return r

handleEnterSuccess :: Giveaway -> TaskE Retry
handleEnterSuccess g = do
    let msg = $(printf "Entered giveaway: %s") (g^.url.unpacked)
    when (isEntered g) $ logTime msg
    updateCurrentAccPoints (g^.accPoints)
    return Done

handleGiveawayError :: GiveawayError -> TaskE Retry
handleGiveawayError ge = case ge of
    ResponseParseError GiveawayRemoved -> do
        logTime "Giveaway removed"
        return Done
    ResponseParseError DataParseError -> do
        logTime "Error parsing data"
        return Retry
    HttpError _ -> do
        logTime "Network error"
        return Retry

updateAccPointsIfExpired :: TaskE ()
updateAccPointsIfExpired = do
    cap <- getsIntState (^.currentAccPoints)
    tn <- liftIO $ getCurrentTime
    if isExpired tn cap
        then getNewAccpoints
        else return ()

getNewAccpoints :: TaskE ()
getNewAccpoints = do
    cfg <- getConfig
    logTime "Updating account points ..."
    maPoints <- liftIO . try $ getAccPoints cfg
    case maPoints of
        Left e -> reportException e
        Right Nothing -> logTime "Error parsing account points from response"
        Right (Just cap') -> updateCurrentAccPoints cap'

updateCurrentAccPoints :: Integer -> TaskE ()
updateCurrentAccPoints cap = do
    cfg <- getConfig
    t <- liftIO $ getCurrentTime
    let ft = addUTCTime (fromIntegral $ cfg^.accPointsExpire * 60) t
        tap = tagValue ft cap
    logTime $ $(printf "You have %d points") cap
    modifyIntState (set currentAccPoints tap)
