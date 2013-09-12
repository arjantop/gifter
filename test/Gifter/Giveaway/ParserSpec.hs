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

spec :: Spec
spec =
    describe "parse" $ do
        it "should parse giveaway with NoLogin status and 236 entries" $ do
            let g = Giveaway {
                        url = "cogs/url",
                        status = NoLogin,
                        entries = 236,
                        formKey = Nothing
                    }
            fmap (parse "cogs/url") loadCogsNoLogin `shouldReturn` Just g
        it "should parse givaway with AlreadyOwn status and 247 entries" $ do
            let g = Giveaway {
                        url = "ds/url",
                        status = AlreadyOwn,
                        entries = 247,
                        formKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap (parse "ds/url") loadDeadSpaceAlreadyOwn `shouldReturn` Just g
        it "should parse givaway with Closed status and 3459 entries" $ do
            let g = Giveaway {
                        url = "dn/url",
                        status = Closed,
                        entries = 3459,
                        formKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap (parse "dn/url") loadDukeNukemOpen `shouldReturn` Just g
        it "should parse givaway with ContributorsOnly status and 226 entries" $ do
            let g = Giveaway {
                        url = "39s/url",
                        status = ContributorsOnly,
                        entries = 226,
                        formKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap (parse "39s/url") loadThe39StepsContributosOnly `shouldReturn` Just g
        it "should parse givaway with Open status and 28 entries" $ do
            let g = Giveaway {
                        url = "ra3",
                        status = Open 20,
                        entries = 28,
                        formKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap (parse "ra3") loadRedAlert3Open `shouldReturn` Just g
        it "should parse givaway with Entered status and 3034 entries" $ do
            let g = Giveaway {
                        url = "fez",
                        status = Entered,
                        entries = 3034,
                        formKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap (parse "fez") loadFezEntered `shouldReturn` Just g
        it "should parse givaway with MissingBaseGame status and 284 entries" $ do
            let g = Giveaway {
                        url = "xcom",
                        status = MissingBaseGame,
                        entries = 284,
                        formKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap (parse "xcom") loadXcomDlcMissingBaseGame `shouldReturn` Just g
        it "should parse givaway with ComingSoon status" $ do
            let g = Giveaway {
                        url = "me",
                        status = ComingSoon,
                        entries = 0,
                        formKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap (parse "me") loadMirrorsEdgeComingSoon `shouldReturn` Just g
