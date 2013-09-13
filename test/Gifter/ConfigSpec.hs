module Gifter.ConfigSpec (
    main,
    spec
)	where

import Test.Hspec

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
                        EntryCondition
                            (Just ["Game Title 1", "Game title 2"])
                            Nothing
                            Nothing
                            Nothing
                            Nothing,
                        EntryCondition
                            Nothing
                            (Just ["key1", "multi key"])
                            Nothing
                            Nothing
                            Nothing,
                        EntryCondition
                            Nothing
                            (Just ["word"])
                            (Just ["key"])
                            (Just $ Lt 10)
                            (Just $ Gte 2)
                    ]
                 }
            readConfig "test/files/config.json" `shouldReturn` Right cfg
