{-# LANGUAGE RecordWildCards,RankNTypes #-}
module Main (
    main
)	where

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

newtype DataEvent a = NewData [a]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    cloc <- defaultLocation
    giveChan <- newTChanIO
    ecfg <- readConfig cloc
    case ecfg of
        Just cfg -> do
            r1 <- async (pollGiveawayEntries giveChan cfg Nothing)
            r2 <- async (enterSelectedGiveaways giveChan cfg)
            res <- waitEitherCatchCancel r1 r2
            handleErrors res
        _ -> logTime $ "Missing config file: " ++ cloc
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


enterSelectedGiveaways :: TChan (DataEvent GiveawayEntry) -> Config -> IO b
enterSelectedGiveaways gc cfg@Config{..} = do
    NewData gs <- atomically $ readTChan gc
    let filteredGs = filter (conditionsMatchAny enter) gs
    logTime $ "Trying to enter " ++ (show . length $ filteredGs) ++ " giveaways"
    enterAll filteredGs
    delay pollDelay
    enterSelectedGiveaways gc cfg
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
            Left _ -> do
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
