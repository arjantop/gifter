{-# LANGUAGE OverloadedStrings #-}
module Gifter.SteamGames
    ( SteamGames
    , sOwned
    , sWishlist
    , sFormKey
    , emptySteamGames
    , numberOwned
    , numberWishlist
    , getSteamGames
    , syncSteamGames
    , isAlreadyOwned
    , isInWishlist
    ) where

import Control.Lens

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (fromDocument)

import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as SL
import qualified Data.ByteString.Char8 as SC

import Gifter.Network
import Gifter.GiveawayEntry
import Gifter.SteamGames.Internal
import Gifter.SteamGames.Parser

sgUrl :: T.Text
sgUrl = "http://www.steamgifts.com/sync"

emptySteamGames :: SteamGames
emptySteamGames = SteamGames (HS.fromList []) (HS.fromList []) Nothing

numberOwned :: SteamGames -> Int
numberOwned sg = HS.size (sg^.sOwned)

numberWishlist :: SteamGames -> Int
numberWishlist sg = HS.size (sg^.sWishlist)

getSteamGames :: T.Text -> IO SteamGames
getSteamGames sid = request sgUrl "GET" [] sid parseSteamGames

parseSteamGames :: SL.ByteString -> SteamGames
parseSteamGames = parse . fromDocument . parseLBS

syncSteamGames :: T.Text -> T.Text -> IO SteamGames
syncSteamGames sid fk =
    let params = ("form_key", convert fk)
    in request sgUrl "POST" [params] sid parseSteamGames
  where
    convert = SC.pack . T.unpack

isAlreadyOwned :: SteamGames -> GiveawayEntry -> Bool
isAlreadyOwned sg ge = HS.member (ge^.gameTitle) (sg^.sOwned)

isInWishlist :: SteamGames -> GiveawayEntry -> Bool
isInWishlist sg ge = HS.member (ge^.gameTitle) (sg^.sWishlist)
