{-# LANGUAGE OverloadedStrings #-}
module Gifter.GiveawayEntry.ParserSpec (
    main,
    spec
) where

import Test.Hspec

import Data.Conduit

import Text.XML.Stream.Parse

import Gifter.GiveawayEntry
import Gifter.GiveawayEntry.Parser

main :: IO ()
main = hspec spec

rssEntries :: [GiveawayEntry]
rssEntries = [
        GiveawayEntry {
            url = "http://www.steamgifts.com/giveaway/niIHw/primordia",
            gameTitle = "Humble Deep Silver Bundle (Above Average)",
            copies = 1,
            points = 10
        },
        GiveawayEntry {
            url = "http://www.steamgifts.com/giveaway/niIHw/primordia",
            gameTitle = "Humble Deep Silver Bundle (Above Average)",
            copies = 5,
            points = 8
        },
        GiveawayEntry {
            url = "http://www.steamgifts.com/giveaway/Nq3zH/fortix",
            gameTitle = "Command & Conquer",
            copies = 5,
            points = 1
        },
        GiveawayEntry {
            url = "http://www.steamgifts.com/giveaway/VLQTR/crysis-2-maximum-edition",
            gameTitle = "Crysis 2 - Maximum Edition",
            copies = 1,
            points = 30
        }
    ]

spec :: Spec
spec = do
    describe "parseEntries" $ do
        it "should return list of entries from valid RSS" $ do
            let entries = runResourceT $
                    parseFile def "test/files/rss.xml" $$ parseEntries
            entries `shouldReturn` rssEntries
