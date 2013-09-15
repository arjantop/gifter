{-# LANGUAGE RecordWildCards, RankNTypes, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (
    main
) where

import Control.Lens
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import System.IO
import System.Locale

import Data.Time.Clock
import Data.Time.Format
import qualified Data.HashSet as HS

import Text.Printf

import Safe

import Gifter.Config
import Gifter.GiveawayEntry
import qualified Gifter.GiveawayEntry as GE
import Gifter.Giveaway
import Gifter.SteamGames

newtype DataEvent a = NewData [a]

type Url = String

data PollState = PollState {
        _lastChecked :: Maybe Url,
        _giveawayChannel :: TChan (DataEvent GiveawayEntry)
    }
makeLenses ''PollState

newtype PollM a = PollM {
        unPollM :: ReaderT Config (StateT PollState IO) a
    } deriving (Monad, MonadState PollState, MonadReader Config, MonadIO)

runPollM :: Config -> PollState -> PollM a -> IO a
runPollM cfg ps m = evalStateT (runReaderT (unPollM m) cfg) ps

data EnterState = EnterState {
        _retriesInfo :: Integer,
        _retriesEnter :: Integer
    }
makeLenses ''EnterState

newtype EnterM a = EnterM {
        unEnterM :: ReaderT Config (StateT EnterState IO) a
    } deriving (Monad, MonadIO, MonadReader Config, MonadState EnterState)

runEnterM :: Config -> EnterState -> EnterM a -> IO a
runEnterM cfg es m = evalStateT (runReaderT (unEnterM m) cfg) es

decRetriesInfo :: EnterM ()
decRetriesInfo = modify (over retriesInfo pred)

decRetriesEnter :: EnterM ()
decRetriesEnter = modify (over retriesEnter pred)

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
    loop (cfg^.maxRetries)
  where
    loop 0 = logTime "Error getting steam games list"
    loop n = do
        sge <- getSteamGames (cfg^.sessionId)
        case sge of
            Right sg -> do
                let nGames = HS.size (sg^.owned)
                    nWish = HS.size (sg^.wishlist)
                logTime $ "You currently own " ++ (show nGames) ++ " games"
                logTime $ "You have " ++ (show nWish) ++ " games in wishlist"
                startTasks cfg sg
            Left _ -> do
                logTime "Could not get steam game list. Retrying"
                delay (cfg^.requestDelay)
                loop (n - 1)

startTasks :: Config -> SteamGames -> IO ()
startTasks cfg sg = do
    giveChan <- newTChanIO
    let ps = PollState Nothing giveChan
    r1 <- async (runPollM cfg ps $ pollGiveawayEntries)
    r2 <- async (enterSelectedGiveaways giveChan cfg sg)
    res <- waitEitherCatchCancel r1 r2
    handleErrors res
  where
    handleErrors (Left (Left e)) =
        logTime $ "Poll thread failed with exception: " ++ show e
    handleErrors (Right (Left e)) =
        logTime $ "Enter giveaways thread failed with exception: " ++ show e
    handleErrors _ = logTime "Unexpected return value"

pollGiveawayEntries :: PollM ()
pollGiveawayEntries = do
    gse <- liftIO $ getEntries 999
    case gse of
        Left e -> do
            cfg <- ask
            logTimeM "Error getting latest giveaways"
            logTimeM . show $ e
            delayM (cfg ^. pollDelay)
            pollGiveawayEntries
        Right gs -> handleEntries gs
  where
    handleEntries gs = do
        cfg <- ask
        gc <- gets (^.giveawayChannel)
        lg <- gets (^.lastChecked)
        let newGs = maybe gs (\u -> takeWhile ((u /=) . GE.url) gs) lg
            gUrl = GE.url `fmap` headMay newGs
        logTimeM $ "Got " ++ (show . length $ newGs) ++ " new giveaways"
        liftIO $ atomically $ writeTChan gc (NewData newGs)
        delayM (cfg ^. pollDelay)
        modify (over lastChecked (gUrl `mplus`))
        pollGiveawayEntries


enterSelectedGiveaways :: TChan (DataEvent GiveawayEntry) -> Config -> SteamGames -> IO ()
enterSelectedGiveaways gc cfg sg = do
    NewData gs <- atomically $ readTChan gc
    let filteredGs = filter (liftM2 (&&)
                                (conditionsMatchAny $ cfg^.enter)
                                (not . isAlreadyOwned sg)) gs
    logTime $ "Trying to enter " ++ (show . length $ filteredGs) ++ " giveaways"
    enterAll filteredGs
    delay $ cfg^.pollDelay
    enterSelectedGiveaways gc cfg sg
  where
    enterAll = mapM_ enterOne
    enterOne g = do
        let es = EnterState (cfg^.maxRetries) (cfg^.maxRetries)
        runEnterM cfg es $ tryEnterGiveaway g
        delay (cfg^.requestDelay)

tryEnterGiveaway :: GiveawayEntry -> EnterM ()
tryEnterGiveaway gi@GiveawayEntry{url=url} = do
    cfg <- ask
    r <- gets (^.retriesInfo)
    if r == 0
        then logTimeM $ "No retries left. Giving up on " ++ url
        else do
            res <- liftIO $ getGiveaway url cfg
            either handleFailure handleSuccess $ res
  where
    handleSuccess g
        | canEnter g = enterGiveawayRetry g
        | otherwise =
            let strStatus = show . status $ g
            in logTimeM $ "Wrong status " ++ strStatus ++ " for " ++ url
    handleFailure e
        | isRemoved e = logTimeM $ "Removed: " ++ url
        | otherwise = do
            logTimeM $ "Error getting info: " ++ url
            (asks $ view retryDelay) >>= delayM
            decRetriesInfo
            tryEnterGiveaway gi

enterGiveawayRetry :: Giveaway -> EnterM ()
enterGiveawayRetry g@Giveaway{..} = do
    r <- gets (^.retriesEnter)
    if r == 0
        then logTimeM $ "No retries left. Unknown status for " ++ url
        else do
            cfg <- ask
            isEntered <- liftIO $ enterGiveaway g cfg
            case isEntered of
                Right True -> logTimeM $ "Entered: " ++ url
                _ -> do
                    logTimeM $ "Error entering: " ++ url
                    delayM (cfg^.retryDelay)
                    decRetriesEnter
                    enterGiveawayRetry g

delay :: Integer -> IO ()
delay ms = threadDelay (fromIntegral ms * 1000000)

delayM :: (MonadIO m) => Integer -> m ()
delayM = liftIO . delay

conditionsMatchAny :: [EntryCondition] -> GiveawayEntry -> Bool
conditionsMatchAny cnd g = any (match g) cnd

logTime :: String -> IO ()
logTime s = do
    time <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    void $ printf "[%s] %s" formattedTime s
    putStrLn ""

logTimeM :: (MonadIO m) => String -> m ()
logTimeM = liftIO . logTime
