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
            _gUrl = "http://www.steamgifts.com/giveaway/niIHw/primordia",
            _gameTitle = "Humble Deep Silver Bundle (Above Average)",
            _gCopies = 1,
            _gPoints = 10
        },
        GiveawayEntry {
            _gUrl = "http://www.steamgifts.com/giveaway/niIHw/primordia",
            _gameTitle = "Humble Deep Silver Bundle (Above Average)",
            _gCopies = 5,
            _gPoints = 8
        },
        GiveawayEntry {
            _gUrl = "http://www.steamgifts.com/giveaway/Nq3zH/fortix",
            _gameTitle = "Command & Conquer",
            _gCopies = 5,
            _gPoints = 1
        },
        GiveawayEntry {
            _gUrl = "http://www.steamgifts.com/giveaway/VLQTR/crysis-2-maximum-edition",
            _gameTitle = "Crysis 2 - Maximum Edition",
            _gCopies = 1,
            _gPoints = 30
        }
    ]

spec :: Spec
spec = do
    describe "parseEntries" $ do
        it "should return list of entries from valid RSS" $ do
            let entries = runResourceT $
                    parseFile def "test/files/rss.xml" $$ parseEntries
            entries `shouldReturn` rssEntries
