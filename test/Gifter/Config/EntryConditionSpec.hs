module Gifter.Config.EntryConditionSpec (
    main,
    spec
)	where

import Test.Hspec

import Gifter.Config.EntryCondition
import Gifter.GiveawayEntry

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "match" $ do
        it "should match if there is exact match of game title in games" $ do
            let ec = EntryCondition (Just ["Game - title"]) Nothing
                ge = GiveawayEntry "" "Game - title" 0
            match ge ec `shouldBe` True
        it "should not match if there is not exact game title match in games" $ do
            let ec = EntryCondition (Just ["title 1", "game 2"]) Nothing
                ge = GiveawayEntry "" "game 1" 0
            match ge ec `shouldBe` False
        it "should match if a keyword is in game title" $ do
            let ec = EntryCondition Nothing (Just ["word1", "word2"])
                ge = GiveawayEntry "" "title with word1 in it" 0
            match ge ec `shouldBe` True
        it "should match keywords with no case sensitivity" $ do
            let ec = EntryCondition Nothing (Just ["word2"])
                ge = GiveawayEntry "" "contains WorD2" 0
            match ge ec `shouldBe` True
        it "should match keywords seperated by spaces out of order" $ do
            let ec = EntryCondition Nothing (Just ["multi key"])
                ge = GiveawayEntry "" "title key contains multi" 0
            match ge ec `shouldBe` True
        it "should not match keywords not contained in game title" $ do
            let ec = EntryCondition Nothing (Just ["not"])
                ge = GiveawayEntry "" "some title" 0
            match ge ec `shouldBe` False
        it "should only match if all the conditions succeed" $ do
            let ec = EntryCondition (Just ["Title"]) (Just ["key"])
                ge = GiveawayEntry "" "Title" 0
                ge2 = GiveawayEntry "" "witk Key" 0
            match ge ec `shouldBe` False
            match ge2 ec `shouldBe` False
