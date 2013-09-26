{-# LANGUAGE TemplateHaskell #-}
module Gifter.Daemon.EnterTask
    ( startEnterTask
    ) where

import Control.Lens
import Control.Monad.State
import Control.Concurrent.STM

import Data.Text.Lens
import Data.Time.Clock
import Data.Time.Calendar

import Text.Printf.Mauke.TH

import Gifter.Daemon.Task
import Gifter.Daemon.Common
import Gifter.Daemon.ConfigWatcherTask
import Gifter.Logging
import Gifter.GiveawayEntry
import Gifter.Giveaway
import Gifter.Giveaway.Parser (DataError(..))
import Gifter.Config
import Gifter.Config.EntryCondition
import Gifter.SteamGames

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

type TaskE = Task () EnterTaskState

data TaggedValue t a = TaggedValue t a
                     deriving (Show, Eq)

getTag :: TaggedValue t v -> t
getTag (TaggedValue t _) = t

getValue :: TaggedValue t v -> v
getValue (TaggedValue _ v) = v

isTimeTagExpired :: TaggedValue UTCTime a -> TaskE Bool
isTimeTagExpired (TaggedValue t _) = do
    ct <- liftIO $ getCurrentTime
    let dt = diffUTCTime ct t
    return $ dt > 100

data EnterTaskState = EnterTaskState
    { _dataChannel :: TChan GiveawayEntry
    , _currentAccPoints :: TaggedValue UTCTime Integer
    , _steamGames :: TaggedValue UTCTime SteamGames
    , _configVar :: ConfigVar
    }
makeLenses ''EnterTaskState

startEnterTask :: Config
               -> TChan GiveawayEntry
               -> ConfigVar
               -> SteamGames
               -> IO ()
startEnterTask cfg dc cv sg = do
    tn <- getCurrentTime
    let tp = UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)
        s = EnterTaskState dc (TaggedValue tp 0) (TaggedValue tn sg) cv
    runTask cfg () s enterTask

enterTask :: TaskE ()
enterTask = forever enterAction

enterAction :: TaskE ()
enterAction = do
    updateConfig (^.configVar)
    cfg <- getConfig
    dc <- getsIntState (^.dataChannel)
    ge <- liftIO . atomically $ readTChan dc
    sg <- getValue `liftM` getsIntState (^.steamGames)
    if matchAny ge sg Nothing (cfg^.enter) && not (isAlreadyOwned sg ge)
        then let maxR = fromIntegral $ cfg^.maxRetries
             in retryN_ maxR $ tryGetGiveawayInfo ge
        else return ()
    delay (cfg^.requestDelay)

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
    updateCurrentAccPoints g
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
    updateCurrentAccPoints g
    if isEntered g
        then logTime $ $(printf "Entered giveaway: %s") (g^.url.unpacked)
        else return ()
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

updateCurrentAccPoints :: Giveaway -> TaskE ()
updateCurrentAccPoints g = do
    t <- liftIO $ getCurrentTime
    let tap = TaggedValue t (g^.accPoints)
    logTime $ $(printf "You have %d points") (g^.accPoints)
    modifyIntState (set currentAccPoints tap)
