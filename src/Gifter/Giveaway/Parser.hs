{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Gifter.Giveaway.Parser (
    parse,
    DataError(..)
) where

import Control.Monad

import Text.XML.Cursor
import Text.XML.Scraping (innerHtml)
import Text.XML.Selector.TH
import Text.XML.Selector.Types (JQSelector)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)

import Safe

import Control.Applicative

import Gifter.Giveaway.Internal

data DataError = GiveawayRemoved
               | DataParseError
               deriving (Show, Eq)

parse :: Url -> Cursor -> Either DataError Giveaway
parse u c = case parseGiveaway u c of
                Nothing
                    | checkIfRemoved -> Left GiveawayRemoved
                    | otherwise      -> Left DataParseError
                Just g -> Right g
  where
    checkIfRemoved =
        let t = parseData [jq| div.notification |] c
            isRemoved = ("been removed" `T.isInfixOf`) `fmap` t
        in fromMaybe False isRemoved

parseGiveaway :: Url -> Cursor -> Maybe Giveaway
parseGiveaway u c = Giveaway <$>
            pure u <*>
            parseStatus c <*>
            parseEntries c `mplus` Just 0 <*>
            parseFormKey c

parseData :: [JQSelector] -> Cursor -> Maybe T.Text
parseData query = fmap (toStrict . innerHtml) . headMay . queryT query

parseAttribute :: [JQSelector] -> Cursor -> Maybe T.Text
parseAttribute query c =
    let attrList = fmap (attribute "value") . headMay . queryT query $ c
    in attrList >>= headMay

parseStatus :: Cursor -> Maybe GiveawayStatus
parseStatus c = parseData [jq| div.details .rounded |] c >>= toStatus
    where toStatus "Closed" = Just Closed
          toStatus "Login to Enter" = Just NoLogin
          toStatus "Exists in Your Account" = Just AlreadyOwn
          toStatus "Contributor Only" = Just ContributorsOnly
          toStatus "Missing Base Game" = Just MissingBaseGame
          toStatus "Coming Soon" = Just ComingSoon
          toStatus msg
            | "Enter to Win" `T.isPrefixOf` msg
                = Open <$> readMay (digitsOnly . T.unpack $ msg)
            | "Remove Entry" `T.isInfixOf` msg = Just Entered
          toStatus _ = Nothing

parseEntries :: Cursor -> Maybe Integer
parseEntries c = (fmap (digitsOnly . T.unpack) .
                    parseData [jq| div.rounded.entries |] $ c) >>= readMay

parseFormKey :: Cursor -> Maybe (Maybe BS.ByteString)
parseFormKey c = Just $
    fmap encodeUtf8 . parseAttribute [jq| input[name=form_key] |] $ c

digitsOnly :: String -> String
digitsOnly = filter (`elem` ['0'..'9'])
