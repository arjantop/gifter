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
import Control.Monad.Reader

import Gifter.Config

data TaskState s = TaskState
    { _config :: Config
    , _intState :: s
    }
makeLenses ''TaskState

newtype Task r s a = Task
    { unTask :: ReaderT r (StateT (TaskState s) IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadPlus
               , MonadIO
               , MonadState (TaskState s)
               , MonadReader r
               )

runTask :: Config -> r -> s -> Task r s a -> IO a
runTask cfg r s m = evalStateT (runReaderT (unTask m) r) (TaskState cfg s)

getConfig :: Task r s Config
getConfig = gets (^.config)

replaceConfig :: Config -> Task r s ()
replaceConfig cfg = modify (set config cfg)

maybeReplaceConfig :: Maybe Config -> Task r s ()
maybeReplaceConfig mcfg = case mcfg of
    Just cfg -> replaceConfig cfg
    _        -> return ()

getIntState :: Task r s s
getIntState = gets (^.intState)

getsIntState :: (s -> a) -> Task r s a
getsIntState f = getIntState >>= return . f

modifyIntState :: (s -> s) -> Task r s ()
modifyIntState f = modify (over intState f)

