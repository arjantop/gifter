{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gifter.Daemon.Common (
    DataEvent(..),
    TaskM,
    runTaskM,
    delay,
    lastCheckedFile,
    readLastChecked,
    writeLastChecked,
    updateConfig
) where

import Control.Lens
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.STM

import System.FilePath
import System.Directory

import Data.Conduit
import Data.Conduit.Binary

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding

import Gifter.Config
import Gifter.Daemon.Task
import Gifter.Daemon.ConfigWatcherTask

newtype DataEvent a = NewData [a]

newtype TaskM s a = TaskM {
        unPollM :: ReaderT Config (StateT s IO) a
    } deriving (Monad, MonadState s, MonadReader Config, MonadIO)

runTaskM :: Config -> s -> TaskM s a -> IO a
runTaskM cfg ts m = evalStateT (runReaderT (unPollM m) cfg) ts

delay :: (MonadIO m) => Integer -> m ()
delay ms = liftIO $ threadDelay (fromIntegral ms * 1000000)

updateConfig :: (s -> ConfigVar) -> Task r s ()
updateConfig f = do
    cfgVar <- getsIntState f
    cfg' <- liftIO . atomically $ readConfigVar cfgVar
    replaceConfig cfg'

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

writeLastChecked :: FilePath -> Maybe T.Text -> IO ()
writeLastChecked fp ms =
    case ms of
        Just s -> runResourceT $ sourceLbs (toLBS s) $$ sinkFile fp
        Nothing -> return ()
  where
    toLBS = encodeUtf8 . TL.fromStrict
