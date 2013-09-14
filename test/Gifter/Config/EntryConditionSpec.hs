module Gifter.Config.EntryConditionSpec (
    main,
    spec
) where

import Test.Hspec

import Gifter.Config.EntryCondition
import Gifter.GiveawayEntry

main :: IO ()
main = hspec spec

withTitle :: String -> GiveawayEntry
withTitle t = GiveawayEntry "" t 0 0

withPoints :: Integer -> GiveawayEntry
withPoints p = GiveawayEntry "" "" 0 p

withCopies :: Integer -> GiveawayEntry
withCopies c = GiveawayEntry "" "" c 0

spec :: Spec
spec =
    describe "match" $ do
        it "should match if there is exact match of game title in games" $ do
            let ec = EntryCondition
                        (Just ["Game - title"]) Nothing Nothing
                        Nothing Nothing
                ge = withTitle "Game - title"
            match ge ec `shouldBe` True
        it "should not match if there is not exact game title match in games" $ do
            let ec = EntryCondition
                        (Just ["title 1", "game 2"]) Nothing Nothing
                        Nothing Nothing
                ge = withTitle "game 1"
            match ge ec `shouldBe` False
        it "should match if a keyword is in game title" $ do
            let ec = EntryCondition
                        Nothing (Just ["word1", "word2"]) Nothing
                        Nothing Nothing
                ge = withTitle "title with word1 in it"
            match ge ec `shouldBe` True
        it "should match keywords with no case sensitivity" $ do
            let ec = EntryCondition
                        Nothing (Just ["word2"]) Nothing
                        Nothing Nothing
                ge = withTitle "contains WorD2"
            match ge ec `shouldBe` True
        it "should match keywords seperated by spaces out of order" $ do
            let ec = EntryCondition
                        Nothing (Just ["multi key"]) Nothing
                        Nothing Nothing
                ge = withTitle "title key contains multi"
            match ge ec `shouldBe` True
        it "should not match keywords not contained in game title" $ do
            let ec = EntryCondition
                        Nothing (Just ["not"]) Nothing
                        Nothing Nothing
                ge = withTitle "some title"
            match ge ec `shouldBe` False
        it "should match only when keyword not in game title" $ do
            let ec = EntryCondition
                        Nothing Nothing (Just ["word"])
                        Nothing Nothing
                ge = withTitle "some title"
            match ge ec `shouldBe` True
        it "should not match if keyword is in title" $ do
            let ec = EntryCondition
                        Nothing Nothing (Just ["title"])
                        Nothing Nothing
                ge = withTitle "some title"
            match ge ec `shouldBe` False
        it "should match if points is less than price in condition" $ do
            let ec = EntryCondition
                        Nothing Nothing Nothing
                        (Just (Lt 10)) Nothing
            match (withPoints 9) ec `shouldBe` True
            match (withPoints 10) ec `shouldBe` False
        it "should match if points is less or equal than the price in condition" $ do
            let ec = EntryCondition
                        Nothing Nothing Nothing
                        (Just (Lte 10)) Nothing
            match (withPoints 9) ec `shouldBe` True
            match (withPoints 10) ec `shouldBe` True
            match (withPoints 11) ec `shouldBe` False
        it "should match if points is equal to the price in condition" $ do
            let ec = EntryCondition
                        Nothing Nothing Nothing
                        (Just (Eq 15)) Nothing
            match (withPoints 14) ec `shouldBe` False
            match (withPoints 15) ec `shouldBe` True
            match (withPoints 16) ec `shouldBe` False
        it "should match if number of compies is greater than number of copies in conditon" $ do
            let ec = EntryCondition
                        Nothing Nothing Nothing
                        Nothing (Just (Gt 11))
            match (withCopies 10) ec `shouldBe` False
            match (withCopies 11) ec `shouldBe` False
            match (withCopies 12) ec `shouldBe` True
        it "should match if number of compies is greater or equal to number of copies in conditon" $ do
            let ec = EntryCondition
                        Nothing Nothing Nothing
                        Nothing (Just (Gte 3))
            match (withCopies 2) ec `shouldBe` False
            match (withCopies 3) ec `shouldBe` True
            match (withCopies 4) ec `shouldBe` True
        it "should only match if all the conditions succeed" $ do
            let ec = EntryCondition
                        (Just ["Title"]) (Just ["key"]) Nothing
                        Nothing Nothing
                ge = withTitle "Title"
                ge2 = withTitle "witk Key"
            match ge ec `shouldBe` False
            match ge2 ec `shouldBe` False
