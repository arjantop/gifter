{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Gifter.Config (
    Config(..),
    sessionId,
    pollDelay,
    requestDelay,
    maxRetries,
    retryDelay,
    enter,
    EntryCondition(..),
    ConfigError(..),
    readConfig,
    defaultLocation,
    match
) where

import Data.Conduit (($$))
import qualified Data.Conduit.Binary as CB
import Data.Aeson

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Resource

import System.Directory
import System.FilePath.Posix

import Gifter.Config.EntryCondition

data Config = Config {
        _sessionId :: String,
        _pollDelay :: Integer,
        _requestDelay :: Integer,
        _maxRetries :: Integer,
        _retryDelay :: Integer,
        _enter :: [EntryCondition]
    } deriving (Show, Eq)
makeLenses ''Config

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "sessionId" <*>
                           v .: "pollDelay" <*>
                           v .: "requestDelay" <*>
                           v .: "maxRetries" <*>
                           v .: "retryDelay" <*>
                           v .: "enter"
    parseJSON _          = mzero

data ConfigError = MissingFile FilePath
                 | ConfigParseError
                 deriving (Show, Eq)

defaultLocation :: IO String
defaultLocation =
    (`combine` ".config/gifter/config.json") `liftM` getHomeDirectory

readConfig :: FilePath -> IO (Either ConfigError Config)
readConfig fp = do
    exists <- doesFileExist fp
    if exists
        then do
            res <- decode `liftM` content
            case res of
                Nothing -> return . Left $ ConfigParseError
                Just cfg -> return . Right $ cfg
        else return . Left $ MissingFile fp
  where content = runResourceT $ CB.sourceFile fp $$ CB.sinkLbs
