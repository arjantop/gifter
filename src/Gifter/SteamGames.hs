{-# LANGUAGE OverloadedStrings #-}
module Gifter.SteamGames
    ( SteamGames
    , sOwned
    , sWishlist
    , emptySteamGames
    , numberOwned
    , numberWishlist
    , getSteamGames
    , isAlreadyOwned
    , isInWishlist
    ) where

import Control.Lens

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (fromDocument)

import qualified Data.Text as T
import qualified Data.HashSet as HS

import Gifter.Network
import Gifter.GiveawayEntry
import Gifter.SteamGames.Internal
import Gifter.SteamGames.Parser

emptySteamGames :: SteamGames
emptySteamGames = SteamGames (HS.fromList []) (HS.fromList [])

numberOwned :: SteamGames -> Int
numberOwned sg = HS.size (sg^.sOwned)

numberWishlist :: SteamGames -> Int
numberWishlist sg = HS.size (sg^.sWishlist)

getSteamGames :: T.Text -> IO SteamGames
getSteamGames sessId =
    request "http://www.steamgifts.com/sync" "GET" [] sessId f
  where
    f = parse . fromDocument . parseLBS

isAlreadyOwned :: SteamGames -> GiveawayEntry -> Bool
isAlreadyOwned sg ge = HS.member (ge^.gameTitle) (sg^.sOwned)

isInWishlist :: SteamGames -> GiveawayEntry -> Bool
isInWishlist sg ge = HS.member (ge^.gameTitle) (sg^.sWishlist)
