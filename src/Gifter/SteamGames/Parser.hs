{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Gifter.SteamGames.Parser
    ( parse
    ) where

import Text.XML.Cursor
import Text.XML.Scraping (innerHtml)
import Text.XML.Selector.TH
import Text.XML.Stream.Parse (decodeHtmlEntities)

import qualified Data.Text as T
import qualified Data.HashSet as HS
import Data.Text.Lazy (toStrict)
import Data.XML.Types (Content(..))

import Gifter.Parser.Common
import Gifter.SteamGames.Internal

parse :: Cursor -> SteamGames
parse c = SteamGames (process ownedList) (process wishList) (parseFormKey c)
  where
    parseData query = fmap (toStrict . innerHtml) . queryT query
    d = parseData [jq| div.row div.code |] c
    (ownedList, rest) = break (T.isInfixOf "1. ") d
    wishlistRaw = takeWhile (not . T.isPrefixOf "<a href") rest
    wishList = map (T.tail . T.dropWhile (/= ' ')) wishlistRaw
    process = HS.fromList . map decodeHtml

decodeHtml :: T.Text -> T.Text
decodeHtml t = case T.uncons t of
    Nothing -> T.empty
    Just ('&', cs) ->
        let (ent, rest') = T.span (/=';') cs
            rest = T.tail rest'
        in case decodeHtmlEntities ent of
               ContentText e -> e `T.append` rest
               ContentEntity _ -> rest
    Just (c, cs) -> c `T.cons` decodeHtml cs
