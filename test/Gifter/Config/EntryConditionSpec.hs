{-# LANGUAGE OverloadedStrings #-}
module Gifter.Config.EntryConditionSpec (
    main,
    spec
) where

import Test.Hspec

import Control.Lens

import qualified Data.Text as T
import qualified Data.HashSet as HS

import Gifter.Config.EntryCondition
import Gifter.GiveawayEntry
import Gifter.SteamGames
import Gifter.SteamGames.Internal

main :: IO ()
main = hspec spec

withTitle :: T.Text -> GiveawayEntry
withTitle t = GiveawayEntry "" t 0 0

withPoints :: Integer -> GiveawayEntry
withPoints p = GiveawayEntry "" "" 0 p

withCopies :: Integer -> GiveawayEntry
withCopies c = GiveawayEntry "" "" c 0

sg :: SteamGames
sg = SteamGames (HS.fromList []) (HS.fromList ["game 1", "game 2"])

eec :: EntryCondition
eec = emptyEntryCondition

spec :: Spec
spec =
    describe "match" $ do
        it "should match if there is exact match of game title in games" $ do
            let ec = eec & games .~ Just ["Game - title", "game2"]
            match (withTitle "Game - title") sg ec `shouldBe` True
            match (withTitle "game1") sg ec `shouldBe` False
        it "should match when there is no exact match of game title" $ do
            let ec = eec & notGames .~ Just ["game 1", "game 2"]
            match (withTitle "game2") sg ec `shouldBe` True
            match (withTitle "game 2") sg ec `shouldBe` False
        it "should match if a keyword is in game title" $ do
            let ec = eec & keywords .~ Just ["word1", "word2"]
            match (withTitle "title with word1 in it") sg ec `shouldBe` True
            match (withTitle "not matching") sg ec `shouldBe` False
        it "should match keywords with no case sensitivity" $ do
            let ec = eec & keywords .~ Just ["word2"]
                ge = withTitle "contains WorD2"
            match ge sg ec `shouldBe` True
        it "should match keywords seperated by spaces out of order" $ do
            let ec = eec & keywords .~ Just ["multi key"]
                ge = withTitle "title key contains multi"
            match ge sg ec `shouldBe` True
        it "should match only when keyword not in game title" $ do
            let ec = eec & notKeywords .~ Just ["word"]
            match (withTitle "some title") sg ec `shouldBe` True
            match (withTitle "title with word") sg ec `shouldBe` False
        it "should match when wishlist game title matches giveaway game title" $ do
            let ec = eec & wishlist .~ True
            match (withTitle "game 1") sg ec `shouldBe` True
            match (withTitle "game1") sg ec `shouldBe` False
        it "should match if points is less than price in condition" $ do
            let ec = eec & points .~ Just (Lt 10)
            match (withPoints 9) sg ec `shouldBe` True
            match (withPoints 10) sg ec `shouldBe` False
        it "should match if points is less or equal than the price in condition" $ do
            let ec = eec & points .~ Just (Lte 10)
            match (withPoints 9) sg ec `shouldBe` True
            match (withPoints 10) sg ec `shouldBe` True
            match (withPoints 11) sg ec `shouldBe` False
        it "should match if points is equal to the price in condition" $ do
            let ec = eec &  points .~ Just (Eq 15)
            match (withPoints 14) sg ec `shouldBe` False
            match (withPoints 15) sg ec `shouldBe` True
            match (withPoints 16) sg ec `shouldBe` False
        it "should match if number of compies is greater than number of copies in conditon" $ do
            let ec = eec & copies .~ Just (Gt 11)
            match (withCopies 10) sg ec `shouldBe` False
            match (withCopies 11) sg ec `shouldBe` False
            match (withCopies 12) sg ec `shouldBe` True
        it "should match if number of compies is greater or equal to number of copies in conditon" $ do
            let ec = eec & copies .~ Just (Gte 3)
            match (withCopies 2) sg ec `shouldBe` False
            match (withCopies 3) sg ec `shouldBe` True
            match (withCopies 4) sg ec `shouldBe` True
        it "should only match if all the conditions succeed" $ do
            let ec = eec & games .~ Just ["Title"]
                         & keywords .~ Just ["key"]
                ge = withTitle "Title"
                ge2 = withTitle "witk Key"
            match ge sg ec `shouldBe` False
            match ge2 sg ec `shouldBe` False
        it "should match when any of the conditions succeed" $ do
            let orSubpart = eec & keywords .~ Just ["key"]
                                & games .~ Just ["Game title"]
                ec = eec & orCond .~ Just orSubpart
            match (withTitle "Title Key") sg ec `shouldBe` True
            match (withTitle "Game title") sg ec `shouldBe` True
            match (withTitle "Something") sg ec `shouldBe` False
        it "should match when all of the conditions succeed" $ do
            let andSubpart = eec & keywords .~ Just ["title"]
                                & games .~ Just ["Game title"]
                ec = eec & andCond .~ Just andSubpart
            match (withTitle "Title Key") sg ec `shouldBe` False
            match (withTitle "Game title") sg ec `shouldBe` True
        it "should match nested conditions correctly" $ do
            let andSubpart = eec & copies .~ Just (Gt 3)
                                 & points .~ Just (Lte 10)
                orSubpart = eec & games .~ Just ["Game title"]
                                & andCond .~ Just andSubpart
                ec = eec & points .~ Just (Gt 3)
                         & orCond .~ Just orSubpart
                ge1 = withTitle "Some title" & gCopies .~ 5
                                             & gPoints .~ 10
                ge2 = withTitle "Game title" & gPoints .~ 12
            match ge1 sg ec `shouldBe` True
            match ge2 sg ec `shouldBe` True
