{-# LANGUAGE OverloadedStrings,RecordWildCards #-}
module Gifter.Config.EntryCondition (
    EntryCondition(..),
    OrdCond(..),
    match
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

import Control.Monad
import Control.Applicative

import Gifter.GiveawayEntry as GE

data OrdCond a = Eq a
               | Lt a
               | Lte a
               | Gt a
               | Gte a
               deriving (Show, Eq)

withOrdCond :: (FromJSON a) => Object -> T.Text -> Parser (Maybe (OrdCond a))
withOrdCond obj key = parse (sfx `zip` cons)
  where
    sfx = [".eq", ".lt", ".lte", ".gt", ".gte"]
    cons = [Eq, Lt, Lte, Gt, Gte]
    parse []     = pure Nothing
    parse (s:ss) = case H.lookup (key `T.append` fst s) obj of
                       Nothing -> parse ss
                       Just v -> (snd s `fmap`) `fmap` parseJSON v

data EntryCondition = EntryCondition {
        games :: Maybe [String],
        keywords :: Maybe [String],
        notKeywords :: Maybe [String],
        points :: Maybe (OrdCond Integer),
        copies :: Maybe (OrdCond Integer)
    } deriving (Show, Eq)

instance FromJSON EntryCondition where
    parseJSON (Object v) = EntryCondition <$>
                           v .:? "games" <*>
                           v .:? "keywords" <*>
                           v .:? "not.keywords" <*>
                           v `withOrdCond` "points" <*>
                           v `withOrdCond` "copies"
    parseJSON _          = mzero

match :: GiveawayEntry -> EntryCondition -> Bool
match ge EntryCondition{..} = and [
        c (matchGames ge) games,
        c (matchKeywords ge) keywords,
        c (matchNotKeywords ge) notKeywords,
        c (matchOrdCond ge GE.points) points,
        c (matchOrdCond ge GE.copies) copies
    ]
  where 
    c = maybe True
    matchGames GiveawayEntry{gameTitle=gameTitle} gs = gameTitle `elem` gs
    matchKeywords GiveawayEntry{gameTitle=gameTitle} ks =
        let gameTitleLower = map toLower gameTitle
        in any (matchKeyword gameTitleLower) ks
    matchKeyword gt k = all (`isInfixOf` gt) (words k)
    matchNotKeywords g = not . matchKeywords g
    matchOrdCond g f oc = case oc of
                             Eq x -> f g == x
                             Lt x -> f g < x
                             Lte x -> f g <= x
                             Gt x -> f g > x
                             Gte x -> f g >= x
