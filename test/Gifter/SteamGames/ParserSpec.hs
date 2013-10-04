{-# LANGUAGE OverloadedStrings #-}
module Gifter.SteamGames.ParserSpec (
    main,
    spec
) where

import Test.Hspec

import Text.XML.Cursor
import qualified Text.HTML.DOM as D (readFile)

import qualified Data.HashSet as HS

import Gifter.SteamGames.Internal
import Gifter.SteamGames.Parser

main :: IO ()
main = hspec spec

loadSteamGames :: IO Cursor
loadSteamGames = fmap fromDocument $ D.readFile "test/files/SteamGames.html"

spec :: Spec
spec =
    describe "parse" $ do
        it "should parse all currently owned and wishlist games" $ do
            let sg = SteamGames {
                        _sOwned = HS.fromList
                                     ["Command & Conquer",
                                      "Amnesia: The Dark Descent",
                                      "Garry's Mod",
                                      "Anomaly 2"],
                        _sWishlist = HS.fromList
                                        ["Tomb Raider",
                                         "Cubemen 2",
                                         "Kentucky Route Zero"],
                        _sFormKey = Just "c0235212cf41b36612029de779bf5261"
                    }
            fmap parse loadSteamGames `shouldReturn` sg
