{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Gifter.Daemon.Task
    ( Task
    , runTask
    , TaskState
    , config
    , intState
    , getConfig
    , replaceConfig
    , maybeReplaceConfig
    , getIntState
    , getsIntState
    , modifyIntState
    ) where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.State

import Gifter.Config

data TaskState s = TaskState
    { _config :: Config
    , _intState :: s
    }
makeLenses ''TaskState

newtype Task s a = Task
    { unTask :: StateT (TaskState s) IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadPlus
               , MonadIO
               , MonadState (TaskState s)
               )

runTask :: Config -> s -> Task s a -> IO a
runTask cfg s m = evalStateT (unTask m) (TaskState cfg s)

getConfig :: Task s Config
getConfig = gets (^.config)

replaceConfig :: Config -> Task s ()
replaceConfig cfg = modify (set config cfg)

maybeReplaceConfig :: Maybe Config -> Task s ()
maybeReplaceConfig mcfg = case mcfg of
    Just cfg -> replaceConfig cfg
    _        -> return ()

getIntState :: Task s s
getIntState = gets (^.intState)

getsIntState :: (s -> a) -> Task s a
getsIntState f = getIntState >>= return . f

modifyIntState :: (s -> s) -> Task s ()
modifyIntState f = modify (over intState f)

