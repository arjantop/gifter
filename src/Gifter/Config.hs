{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Gifter.Config
    ( Config(..)
    , sessionId
    , pollDelay
    , requestDelay
    , maxRetries
    , retryDelay
    , accPointsExpire
    , steamGamesExpire
    , enter
    , cfgDir
    , stateDir
    , EntryCondition
    , ConfigError(..)
    , emptyConfig
    , readConfig
    , defaultLocation
    , defaultDir
    ) where

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Aeson
import qualified Data.Text as T

import Control.Lens
import Control.Applicative
import Control.Monad

import System.Directory
import System.FilePath

import Gifter.Config.EntryCondition

data Config = Config
    { _sessionId :: T.Text
    , _pollDelay :: Integer
    , _requestDelay :: Integer
    , _maxRetries :: Integer
    , _retryDelay :: Integer
    , _accPointsExpire :: Integer
    , _steamGamesExpire :: Integer
    , _enter :: [EntryCondition]
    , _cfgDir :: FilePath
    , _stateDir :: FilePath
    } deriving (Show, Eq)
makeLenses ''Config

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "sessionId" <*>
                           v .: "pollDelay" <*>
                           v .: "requestDelay" <*>
                           v .: "maxRetries" <*>
                           v .: "retryDelay" <*>
                           v .: "accPointsExpire" <*>
                           v .: "steamGamesExpire" <*>
                           v .: "enter" <*>
                           pure "" <*>
                           pure ""
    parseJSON _          = mzero

data ConfigError = MissingFile FilePath
                 | ConfigParseError
                 deriving (Show, Eq)

emptyConfig :: Config
emptyConfig = Config "" 0 0 0 0 0 0 [] "" ""

defaultDir :: IO FilePath
defaultDir = getAppUserDataDirectory "gifter"

defaultLocation :: IO FilePath
defaultLocation =
    (`combine` "config.json") `liftM` defaultDir

readConfig :: FilePath -> IO (Either ConfigError Config)
readConfig fp = do
    exists <- doesFileExist fp
    if exists
        then do
            res <- decode `liftM` content
            case res of
                Nothing -> return . Left $ ConfigParseError
                Just cfg ->
                    let cfg' = cfg & cfgDir .~ dropFileName fp
                        cfg'' = cfg' & stateDir .~ (cfg'^.cfgDir) </> "state"
                    in return . Right $ cfg''
        else return . Left $ MissingFile fp
  where content = runResourceT $ CB.sourceFile fp $$ CB.sinkLbs
