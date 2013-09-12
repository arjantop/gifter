{-# LANGUAGE OverloadedStrings,RecordWildCards #-}
module Gifter.Config.EntryCondition (
    EntryCondition(..),
    match
) where

import Data.Aeson
import Data.List
import Data.Char (toLower)

import Control.Monad
import Control.Applicative

import Gifter.GiveawayEntry

data EntryCondition = EntryCondition {
        games :: Maybe [String],
        keywords :: Maybe [String]
    } deriving (Show, Eq)

instance FromJSON EntryCondition where
    parseJSON (Object v) = EntryCondition <$>
                           v .:? "games" <*>
                           v .:? "keywords"
    parseJSON _          = mzero

match :: GiveawayEntry -> EntryCondition -> Bool
match ge EntryCondition{..} = and [
        maybe True (matchGames ge) games,
        maybe True (matchKeywords ge) keywords
    ]
  where
    matchGames GiveawayEntry{..} gs = gameTitle `elem` gs
    matchKeywords GiveawayEntry{..} ks =
        let gameTitleLower = map toLower gameTitle
        in any (matchKeyword gameTitleLower) ks
    matchKeyword gt k = all (`isInfixOf` gt) (words k)

