{-# LANGUAGE OverloadedStrings #-}
module Gifter.ConfigSpec (
    main,
    spec
)	where

import Test.Hspec

import Control.Lens

import Gifter.Config
import Gifter.Config.EntryCondition

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "readConfig" $ do
        it "should read all config fields correctly" $ do
            let cfg = Config {
                    _sessionId = "4aaf5a",
                    _pollDelay = 180,
                    _requestDelay = 3,
                    _maxRetries = 3,
                    _retryDelay = 3,
                    _enter = [
                        emptyEntryCondition
                            & games .~ Just ["Game Title 1", "Game title 2"],
                        emptyEntryCondition
                            & notGames .~ Just ["game 1"]
                            & keywords .~ Just ["key1", "multi key"]
                            & wishlist .~ True,
                        emptyEntryCondition
                            & keywords .~ Just ["word"]
                            & notKeywords .~ Just ["key"]
                            & points .~ Just (Lt 10)
                            & copies .~ Just (Gte 2)
                            & accP .~ Just (Gt 100),
                        emptyEntryCondition
                            & keywords .~ Just ["title"]
                            & orCond .~ Just (emptyEntryCondition
                                    & games .~ Just ["Game Title"]
                                    & andCond .~ Just (emptyEntryCondition
                                            & points .~ Just (Lt 20)
                                            & copies .~ Just (Gt 2)))
                    ],
                    _cfgDir = "test/files/"
                 }
            readConfig "test/files/config.json" `shouldReturn` Right cfg
