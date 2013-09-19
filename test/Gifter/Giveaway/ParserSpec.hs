{-# LANGUAGE OverloadedStrings #-}
module Gifter.Giveaway.ParserSpec (
    main,
    spec
) where

import Test.Hspec

import Text.XML.Cursor (Cursor,fromDocument)
import qualified Text.HTML.DOM as D (readFile)

import Filesystem.Path as FP

import Gifter.Giveaway
import Gifter.Giveaway.Parser

main :: IO ()
main = hspec spec

loadHtml :: FP.FilePath -> IO Cursor
loadHtml fp = fmap fromDocument $ D.readFile fp

loadCogsNoLogin :: IO Cursor
loadCogsNoLogin = loadHtml "test/files/CogsNoLogin.html"

loadDeadSpaceAlreadyOwn :: IO Cursor
loadDeadSpaceAlreadyOwn = loadHtml "test/files/DeadSpaceAlreadyOwn.html"

loadDukeNukemOpen :: IO Cursor
loadDukeNukemOpen = loadHtml "test/files/DukeNukemOpen.html"

loadThe39StepsContributosOnly :: IO Cursor
loadThe39StepsContributosOnly =
        loadHtml "test/files/The39StepsContributorsOnly.html"

loadRedAlert3Open :: IO Cursor
loadRedAlert3Open = loadHtml "test/files/RedAlert3Open.html"

loadFezEntered :: IO Cursor
loadFezEntered = loadHtml "test/files/FezEntered.html"

loadMirrorsEdgeComingSoon :: IO Cursor
loadMirrorsEdgeComingSoon = loadHtml "test/files/MirrorsEdgeComingSoon.html"

loadXcomDlcMissingBaseGame :: IO Cursor
loadXcomDlcMissingBaseGame =
        loadHtml "test/files/XcomEliteSoldierPackMissingBaseGame.html"

loadDeadlightNotEnoughPoints :: IO Cursor
loadDeadlightNotEnoughPoints = loadHtml "test/files/DeadlightNotEnoughPoints.html"

loadGiveawayRemoved :: IO Cursor
loadGiveawayRemoved = loadHtml "test/files/GiveawayRemoved.html"

spec :: Spec
spec =
    describe "parse" $ do
        it "should parse giveaway with NoLogin status and 236 entries" $ do
            let g = Giveaway {
                        _url = "cogs/url",
                        _status = NoLogin,
                        _entries = 236,
                        _formKey = Nothing,
                        _accPoints = 0
                    }
            fmap (parse "cogs/url") loadCogsNoLogin `shouldReturn` Right g
        it "should parse givaway with AlreadyOwn status and 247 entries" $ do
            let g = Giveaway {
                        _url = "ds/url",
                        _status = AlreadyOwn,
                        _entries = 247,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 300
                    }
            fmap (parse "ds/url") loadDeadSpaceAlreadyOwn `shouldReturn` Right g
        it "should parse givaway with Closed status and 3459 entries" $ do
            let g = Giveaway {
                        _url = "dn/url",
                        _status = Closed,
                        _entries = 3459,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 300
                    }
            fmap (parse "dn/url") loadDukeNukemOpen `shouldReturn` Right g
        it "should parse givaway with ContributorsOnly status and 226 entries" $ do
            let g = Giveaway {
                        _url = "39s/url",
                        _status = ContributorsOnly,
                        _entries = 226,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 300
                    }
            fmap (parse "39s/url") loadThe39StepsContributosOnly `shouldReturn` Right g
        it "should parse givaway with Open status and 28 entries" $ do
            let g = Giveaway {
                        _url = "ra3",
                        _status = Open 20,
                        _entries = 28,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 300
                    }
            fmap (parse "ra3") loadRedAlert3Open `shouldReturn` Right g
        it "should parse givaway with Entered status and 3034 entries" $ do
            let g = Giveaway {
                        _url = "fez",
                        _status = Entered,
                        _entries = 3034,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 192
                    }
            fmap (parse "fez") loadFezEntered `shouldReturn` Right g
        it "should parse givaway with MissingBaseGame status and 284 entries" $ do
            let g = Giveaway {
                        _url = "xcom",
                        _status = MissingBaseGame,
                        _entries = 284,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 192
                    }
            fmap (parse "xcom") loadXcomDlcMissingBaseGame `shouldReturn` Right g
        it "should parse givaway with ComingSoon status" $ do
            let g = Giveaway {
                        _url = "me",
                        _status = ComingSoon,
                        _entries = 0,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 277
                    }
            fmap (parse "me") loadMirrorsEdgeComingSoon `shouldReturn` Right g
        it "should parse givaway with NotEnoughpoints status and 2485 entries" $ do
            let g = Giveaway {
                        _url = "de",
                        _status = NotEnoughPoints,
                        _entries = 2485,
                        _formKey = Just "c0235212cf41b36612029de779bf5261",
                        _accPoints = 2
                    }
            fmap (parse "de") loadDeadlightNotEnoughPoints `shouldReturn` Right g
        it "shoud return error status GiveawayRemoved if the giveaway has been removed" $ do
            fmap (parse "removed") loadGiveawayRemoved `shouldReturn` Left GiveawayRemoved
