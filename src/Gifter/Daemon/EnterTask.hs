{-# LANGUAGE TemplateHaskell #-}
module Gifter.Daemon.EnterTask (
    enterSelectedGiveaways
) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM

import Data.Maybe
import Data.Text.Lens

import Gifter.Daemon.Common
import Gifter.Logging
import Gifter.GiveawayEntry
import Gifter.Giveaway
import Gifter.Config
import Gifter.SteamGames

data EnterState = EnterState {
        _retriesInfo :: Integer,
        _retriesEnter :: Integer
    }
makeLenses ''EnterState

decRetriesInfo :: TaskM EnterState ()
decRetriesInfo = modify (over retriesInfo pred)

decRetriesEnter :: TaskM EnterState ()
decRetriesEnter = modify (over retriesEnter pred)

enterSelectedGiveaways :: TChan (DataEvent GiveawayEntry) -> Config -> SteamGames -> Maybe Integer -> IO ()
enterSelectedGiveaways gc cfg sg cap = do
    NewData gs <- atomically $ readTChan gc
    let filteredGs = filter (not . isAlreadyOwned sg) gs
        numFilGs = length filteredGs
    logTimeWhen (numFilGs > 0) $ "Trying to enter " ++ show numFilGs ++ " giveaways"
    cap' <- enterAll cap filteredGs
    delay $ cfg^.pollDelay
    enterSelectedGiveaways gc cfg sg (cap' `mplus` cap)
  where
    enterAll c = foldM (foldCap c) Nothing
    foldCap c acc ge = do
        cap' <- enterOne c ge
        return $ cap' `mplus` acc
    enterOne c ge =
        if conditionsMatchAny (cfg^.enter) sg c ge
            then do
                let es = EnterState (cfg^.maxRetries) (cfg^.maxRetries)
                cap' <- runTaskM cfg es $ tryEnterGiveaway ge
                logTimeWhen (isJust cap' && cap /= cap') $
                    "You have " ++ (show . fromJust $ cap') ++ " points"
                delay (cfg^.requestDelay)
                return cap'
            else return Nothing

tryEnterGiveaway :: GiveawayEntry -> TaskM EnterState (Maybe Integer)
tryEnterGiveaway ge = do
    cfg <- ask
    r <- gets (^.retriesInfo)
    if r == 0
        then do
            logTime $ "No retries left. Giving up on " ++ ge^.gUrl.unpacked
            return Nothing
        else do
            res <- liftIO $ getGiveaway (ge^.gUrl) cfg
            either handleFailure handleSuccess res
  where
    handleSuccess g
        | canEnter g = enterGiveawayRetry g
        | otherwise =
            let strStatus = show $ g^.status
            in do
                logTime $ "Wrong status " ++ strStatus ++ " for " ++ ge^.gUrl.unpacked
                return $ Just (g^.accPoints)
    handleFailure e
        | isRemoved e = do
            logTime $ "Removed: " ++ ge^.gUrl.unpacked
            return Nothing
        | otherwise = do
            logTime $ "Error getting info: " ++ ge^.gUrl.unpacked
            asks (view retryDelay) >>= delay
            decRetriesInfo
            tryEnterGiveaway ge

enterGiveawayRetry :: Giveaway -> TaskM EnterState (Maybe Integer)
enterGiveawayRetry g = do
    r <- gets (^.retriesEnter)
    if r == 0
        then do
            logTime $ "No retries left. Unknown status for " ++ g^.url.unpacked
            return Nothing
        else do
            cfg <- ask
            rge <- liftIO $ enterGiveaway g cfg
            case rge of
                Right rg
                    | isEntered rg -> do
                        logTime $ "Entered: " ++ g^.url.unpacked
                        return $ Just (rg^.accPoints)
                _ -> do
                    logTime $ "Error entering: " ++ g^.url.unpacked
                    delay (cfg^.retryDelay)
                    decRetriesEnter
                    enterGiveawayRetry g

conditionsMatchAny :: [EntryCondition] -> SteamGames -> Maybe Integer -> GiveawayEntry -> Bool
conditionsMatchAny cnd sg acp ge = any (match ge sg acp) cnd
