{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import System.Directory
import System.FilePath.Posix
import System.Console.CmdArgs
import System.Environment

import Data.Data ()
import Data.Typeable ()
import Data.Conduit
import Data.Conduit.Binary
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Maybe
import Data.List as L
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Text.Lens
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding

import Text.Printf

import Safe

import Gifter.Config
import Gifter.GiveawayEntry
import Gifter.Giveaway
import Gifter.SteamGames

newtype DataEvent a = NewData [a]

data PollState = PollState {
        _lastChecked :: Maybe T.Text,
        _giveawayChannel :: TChan (DataEvent GiveawayEntry)
    }
makeLenses ''PollState

data EnterState = EnterState {
        _retriesInfo :: Integer,
        _retriesEnter :: Integer
    }
makeLenses ''EnterState

newtype TaskM s a = TaskM {
        unPollM :: ReaderT Config (StateT s IO) a
    } deriving (Monad, MonadState s, MonadReader Config, MonadIO)

runTaskM :: Config -> s -> TaskM s a -> IO a
runTaskM cfg ts m = evalStateT (runReaderT (unPollM m) cfg) ts

decRetriesInfo :: TaskM EnterState ()
decRetriesInfo = modify (over retriesInfo pred)

decRetriesEnter :: TaskM EnterState ()
decRetriesEnter = modify (over retriesEnter pred)

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
    loop 0 = logTime "Error getting steam games list"
    loop n = do
        sge <- getSteamGames (cfg^.sessionId)
        case sge of
            Right sg -> do
                let nGames = HS.size (sg^.sOwned)
                    nWish = HS.size (sg^.sWishlist)
                logTime $ "You currently own " ++ show nGames ++ " games"
                logTime $ "You have " ++ show nWish ++ " games in wishlist"
                startTasks cfg sg
            Left _ -> do
                logTime "Could not get steam game list. Retrying"
                delay (cfg^.requestDelay)
                loop (n - 1)

startTasks :: Config -> SteamGames -> IO ()
startTasks cfg sg = do
    giveChan <- newTChanIO
    lastChPer <- readLastChecked $ lastCheckedFile cfg
    let ps = PollState lastChPer giveChan
    r1 <- async (runTaskM cfg ps pollGiveawayEntries)
    r2 <- async (enterSelectedGiveaways giveChan cfg sg)
    res <- waitEitherCatchCancel r1 r2
    handleErrors res
  where
    handleErrors (Left (Left e)) =
        logTime $ "Poll thread failed with exception: " ++ show e
    handleErrors (Right (Left e)) =
        logTime $ "Enter giveaways thread failed with exception: " ++ show e
    handleErrors _ = logTime "Unexpected return value"

lastCheckedFile :: Config -> FilePath
lastCheckedFile cfg = (cfg^.cfgDir) `combine` "last"

readLastChecked :: FilePath -> IO (Maybe T.Text)
readLastChecked fp = do
    exists <- doesFileExist fp
    if exists
        then do
            c <- runResourceT $ sourceFile fp $$ sinkLbs
            return $ Just (TL.toStrict . decodeUtf8 $ c)
        else return Nothing

writeLastChecked :: FilePath -> T.Text -> IO ()
writeLastChecked fp s =
    runResourceT $ sourceLbs (encodeUtf8 . TL.fromStrict $ s) $$ sinkFile fp

pollGiveawayEntries :: TaskM PollState ()
pollGiveawayEntries = do
    gse <- liftIO $ getEntries 999
    case gse of
        Left e -> do
            cfg <- ask
            logTime "Error getting latest giveaways"
            logTime . show $ e
            delay (cfg^.pollDelay)
            pollGiveawayEntries
        Right gs -> handleEntries gs
  where
    handleEntries gs = do
        cfg <- ask
        gc <- gets (^.giveawayChannel)
        lg <- gets (^.lastChecked)
        let newGs = maybe gs (\u -> L.takeWhile ((u /=) . (^.gUrl)) gs) lg
            numGs = length newGs
            lUrl = (^.gUrl) `fmap` headMay newGs
            newLs = lUrl `mplus` lg
        when (isJust lUrl) . liftIO $
            writeLastChecked (lastCheckedFile cfg) (fromJust newLs)
        logTimeWhen (numGs > 0) $ "Got " ++ show numGs ++ " new giveaways"
        liftIO $ atomically $ writeTChan gc (NewData newGs)
        delay (cfg ^. pollDelay)
        modify (set lastChecked newLs)
        pollGiveawayEntries


enterSelectedGiveaways :: TChan (DataEvent GiveawayEntry) -> Config -> SteamGames -> IO ()
enterSelectedGiveaways gc cfg sg = do
    NewData gs <- atomically $ readTChan gc
    let filteredGs = filter (liftM2 (&&)
                                (conditionsMatchAny (cfg^.enter) sg)
                                (not . isAlreadyOwned sg)) gs
        numFilGs = length filteredGs
    logTimeWhen (numFilGs > 0) $ "Trying to enter " ++ show numFilGs ++ " giveaways"
    enterAll filteredGs
    delay $ cfg^.pollDelay
    enterSelectedGiveaways gc cfg sg
  where
    enterAll = mapM_ enterOne
    enterOne g = do
        let es = EnterState (cfg^.maxRetries) (cfg^.maxRetries)
        runTaskM cfg es $ tryEnterGiveaway g
        delay (cfg^.requestDelay)

tryEnterGiveaway :: GiveawayEntry -> TaskM EnterState ()
tryEnterGiveaway ge = do
    cfg <- ask
    r <- gets (^.retriesInfo)
    if r == 0
        then logTime $ "No retries left. Giving up on " ++ ge^.gUrl.unpacked
        else do
            res <- liftIO $ getGiveaway (ge^.gUrl) cfg
            either handleFailure handleSuccess res
  where
    handleSuccess g
        | canEnter g = enterGiveawayRetry g
        | otherwise =
            let strStatus = show $ g^.status
            in logTime $ "Wrong status " ++ strStatus ++ " for " ++ ge^.gUrl.unpacked
    handleFailure e
        | isRemoved e = logTime $ "Removed: " ++ ge^.gUrl.unpacked
        | otherwise = do
            logTime $ "Error getting info: " ++ ge^.gUrl.unpacked
            asks (view retryDelay) >>= delay
            decRetriesInfo
            tryEnterGiveaway ge

enterGiveawayRetry :: Giveaway -> TaskM EnterState ()
enterGiveawayRetry g = do
    r <- gets (^.retriesEnter)
    if r == 0
        then logTime $ "No retries left. Unknown status for " ++ g^.url.unpacked
        else do
            cfg <- ask
            isEntered <- liftIO $ enterGiveaway g cfg
            case isEntered of
                Right True -> logTime $ "Entered: " ++ g^.url.unpacked
                _ -> do
                    logTime $ "Error entering: " ++ g^.url.unpacked
                    delay (cfg^.retryDelay)
                    decRetriesEnter
                    enterGiveawayRetry g

conditionsMatchAny :: [EntryCondition] -> SteamGames-> GiveawayEntry -> Bool
conditionsMatchAny cnd sg g = any (match g sg) cnd

delay :: (MonadIO m) => Integer -> m ()
delay ms = liftIO $ threadDelay (fromIntegral ms * 1000000)

logTime :: (MonadIO m) => String -> m ()
logTime s = do
    utcTime <- liftIO getCurrentTime
    tz <- liftIO $ getTimeZone utcTime
    let time = utcToLocalTime tz utcTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    liftIO . void $ printf "[%s] %s" formattedTime s
    liftIO $ putStrLn ""

logTimeWhen :: (MonadIO m) => Bool -> String -> m ()
logTimeWhen cond msg = when cond $ logTime msg
