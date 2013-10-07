{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Gifter.Daemon.LastCheckedUrl
    ( updateLastCheckedUrl
    , getLastCheckedUrl
    , initialLastCheckedUrlState
    , AcidUrl
    ) where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Data.Acid
import Data.Data (Typeable, Data)
import Data.SafeCopy
import qualified Data.Text as T

data LastCheckedUrlState = LastCheckedUrlState
    { url :: T.Text }
    deriving ( Eq
             , Show
             , Data
             , Typeable)

$(deriveSafeCopy 0 'base ''LastCheckedUrlState)

initialLastCheckedUrlState :: LastCheckedUrlState
initialLastCheckedUrlState = LastCheckedUrlState ""

updateLastCheckedUrlState :: T.Text -> Update LastCheckedUrlState T.Text
updateLastCheckedUrlState u = do
    lu <- get
    put $ lu { url = u }
    return $ url lu

getLastCheckedUrlState :: Query LastCheckedUrlState T.Text
getLastCheckedUrlState = url <$> ask

$(makeAcidic ''LastCheckedUrlState [ 'updateLastCheckedUrlState
                                   , 'getLastCheckedUrlState])

type AcidUrl = AcidState LastCheckedUrlState

updateLastCheckedUrl :: (MonadIO m, MonadReader r m)
                     => Getting AcidUrl r AcidUrl
                     -> T.Text
                     -> m ()
updateLastCheckedUrl ag u = do
    acid <- asks (^.ag)
    liftIO . void $ update acid (UpdateLastCheckedUrlState u)

getLastCheckedUrl :: (MonadIO m, MonadReader r m)
                  => Getting AcidUrl r AcidUrl
                  -> m T.Text
getLastCheckedUrl ag = do
    acid <- asks (^.ag)
    liftIO $ query acid GetLastCheckedUrlState
