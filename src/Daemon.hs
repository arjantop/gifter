{-# LANGUAGE RecordWildCards,RankNTypes #-}
module Main (
    main
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import System.IO
import System.Locale

import Data.Time.Clock
import Data.Time.Format

import Text.Printf

import Safe

import Gifter.Config
import Gifter.GiveawayEntry
import qualified Gifter.GiveawayEntry as GE
import Gifter.Giveaway
import Gifter.SteamGames

newtype DataEvent a = NewData [a]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    cloc <- defaultLocation
    ecfg <- readConfig cloc
    case ecfg of
        Right cfg -> tryGetSteamGames cfg
        Left (MissingFile fp) -> logTime $ "Missing config file: " ++ fp
        Left ConfigParseError -> logTime $ "Config parse error"

tryGetSteamGames :: Config -> IO ()
tryGetSteamGames cfg = do
    logTime "Trying to get steam game list"
    loop (maxRetries cfg)
  where
    loop 0 = logTime "Error getting steam games list"
    loop n = do
        sge <- getSteamGames (sessionId cfg)
        case sge of
            Right sg -> startTasks cfg sg
            Left _ -> do
                logTime "Could not get steam game list. Retrying"
                delay (requestDelay cfg)
                loop (n - 1)

startTasks :: Config -> SteamGames -> IO ()
startTasks cfg sg = do
    giveChan <- newTChanIO
    r1 <- async (pollGiveawayEntries giveChan cfg Nothing)
    r2 <- async (enterSelectedGiveaways giveChan cfg sg)
    res <- waitEitherCatchCancel r1 r2
    handleErrors res
  where
    handleErrors (Left (Left e)) =
        logTime $ "Poll thread failed with exception: " ++ show e
    handleErrors (Right (Left e)) =
        logTime $ "Enter giveaways thread failed with exception: " ++ show e
    handleErrors _ = logTime "Unexpected return value"

pollGiveawayEntries :: TChan (DataEvent GiveawayEntry)
                    -> Config
                    -> Maybe GiveawayEntry
                    -> IO ()
pollGiveawayEntries gc cfg@Config{..} lg = do
    gse <- getEntries 999
    case gse of
        Left e -> do
            logTime "Error getting latest giveaways"
            logTime . show $ e
            delay pollDelay
            pollGiveawayEntries gc cfg lg
        Right gs -> handleEntries gs
  where
    handleEntries gs = do
        let newGs = maybe gs (\g -> takeWhile ((/= GE.url g) . GE.url) gs) lg
        logTime $ "Got " ++ (show . length $ newGs) ++ " new giveaways"
        atomically $ writeTChan gc (NewData newGs)
        delay pollDelay
        pollGiveawayEntries gc cfg (headMay newGs `mplus` lg)


enterSelectedGiveaways :: TChan (DataEvent GiveawayEntry) -> Config -> SteamGames -> IO ()
enterSelectedGiveaways gc cfg@Config{..} sg = do
    NewData gs <- atomically $ readTChan gc
    let filteredGs = filter (liftM2 (&&)
                                (conditionsMatchAny enter)
                                (not . isAlreadyOwned sg)) gs
    logTime $ "Trying to enter " ++ (show . length $ filteredGs) ++ " giveaways"
    enterAll filteredGs
    delay pollDelay
    enterSelectedGiveaways gc cfg sg
  where
    enterAll = mapM_ ((>> delay requestDelay) . enterOne maxRetries)
    enterOne 0 GiveawayEntry{..} = 
        logTime $ "No retries left. Giving up on " ++ url
    enterOne retries ge@GiveawayEntry{..} = do
        res <- getGiveaway url cfg
        case res of
            Right g
                | canEnter g -> enterAndCheck maxRetries g
                | otherwise  ->
                    let strStatus = show . status $ g
                    in logTime $ "Wrong status " ++ strStatus ++ " for " ++ url
            Left e
                | isRemoved e -> logTime $ "Giveaway removed: " ++ url
                | otherwise -> do
                    logTime $ "Error getting giveaway info: " ++ url
                    delay retryDelay
                    enterOne (retries - 1) ge
    enterAndCheck 0 Giveaway{..} =
        logTime $ "No retries left. Unknown status for " ++ url
    enterAndCheck retries g@Giveaway{..} = do
        isEntered <- enterGiveaway g cfg
        case isEntered of
            Right True -> logTime $ "Entered giveaway: " ++ url
            _ -> do
                logTime $ "Error entering giveaway: " ++ url
                delay retryDelay
                enterAndCheck (retries - 1) g

delay :: Integer -> IO ()
delay ms = threadDelay (fromIntegral ms * 1000000)

conditionsMatchAny :: [EntryCondition] -> GiveawayEntry -> Bool
conditionsMatchAny cnd g = any (match g) cnd

logTime :: String -> IO ()
logTime s = do
    time <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    void $ printf "[%s] %s" formattedTime s
    putStrLn ""
