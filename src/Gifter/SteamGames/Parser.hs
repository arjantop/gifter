{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Gifter.SteamGames.Parser (
    parse
) where

import Text.XML.Cursor
import Text.XML.Scraping (innerHtml)
import Text.XML.Selector.TH

import qualified Data.Text as T
import qualified Data.HashSet as HS
import Data.Text.Lazy (toStrict)

import Gifter.SteamGames.Internal

parse :: Cursor -> SteamGames
parse c = SteamGames (HS.fromList ownedList) (HS.fromList wishList)
  where
    parseData query = fmap (toStrict . innerHtml) . queryT query
    d = parseData [jq| div.row div.code |] c
    (ownedList, rest) = span (not . T.isInfixOf "1. ") d
    wishlistRaw = takeWhile (not . T.isPrefixOf "<a href") rest
    wishList = map (T.tail . T.dropWhile (/= ' ')) wishlistRaw
