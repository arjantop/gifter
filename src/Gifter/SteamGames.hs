{-# LANGUAGE OverloadedStrings #-}
module Gifter.SteamGames (
    SteamGames,
    owned,
    wishlist,
    getSteamGames,
    isAlreadyOwned,
    isInWishlist
) where

import Control.Exception
import Control.Lens

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (fromDocument)

import qualified Data.Text as T
import qualified Data.HashSet as HS

import Gifter.Network
import Gifter.GiveawayEntry
import Gifter.SteamGames.Internal
import Gifter.SteamGames.Parser

getSteamGames :: T.Text -> IO (Either SomeException SteamGames)
getSteamGames sessId =
    try $ request "http://www.steamgifts.com/sync" "GET" [] sessId f
  where
    f = parse . fromDocument . parseLBS

isAlreadyOwned :: SteamGames -> GiveawayEntry -> Bool
isAlreadyOwned sg ge = HS.member (gameTitle ge) (sg^.owned)

isInWishlist :: SteamGames -> GiveawayEntry -> Bool
isInWishlist sg ge = HS.member (gameTitle ge) (sg^.wishlist)
