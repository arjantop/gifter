module Gifter.Logging (
    logTime,
    logTimeWhen
) where

import Control.Monad
import Control.Monad.IO.Class

import System.Locale

import Text.Printf

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

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
