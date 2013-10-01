{-# LANGUAGE TemplateHaskell #-}
module Gifter.Daemon.Common
    ( delay
    , lastCheckedFile
    , readLastChecked
    , writeLastChecked
    , updateConfig
    , reportException
    ) where

import Control.Lens
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TEVar
import Control.Exception

import System.FilePath
import System.Directory

import Text.Printf.Mauke.TH

import Data.Conduit
import Data.Conduit.Binary

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding

import Gifter.Config
import Gifter.Daemon.Task
import Gifter.Logging

delay :: (MonadIO m) => Integer -> m ()
delay ms = liftIO $ threadDelay (fromIntegral ms * 1000000)

updateConfig :: (r -> TEVar Config) -> Task r s ()
updateConfig f = do
    cfgVar <- asks f
    cfg' <- liftIO . atomically $ readTEVar cfgVar
    replaceConfig cfg'

reportException :: SomeException -> Task r s ()
reportException e =
    logTime $ $(printf "Failed with exception: %s") (show e)

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
