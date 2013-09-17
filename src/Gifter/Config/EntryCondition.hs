{-# LANGUAGE OverloadedStrings,RecordWildCards #-}
module Gifter.Config.EntryCondition (
    EntryCondition(..),
    OrdCond(..),
    match
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Control.Lens
import Control.Monad
import Control.Applicative

import Gifter.GiveawayEntry as GE
import qualified Gifter.SteamGames as SG

data OrdCond a = Eq a
               | Lt a
               | Lte a
               | Gt a
               | Gte a
               deriving (Show, Eq)

withOrdCond :: (FromJSON a) => Object -> T.Text -> Parser (Maybe (OrdCond a))
withOrdCond obj key = parse (sfx `zip` dcons)
  where
    sfx = [".eq", ".lt", ".lte", ".gt", ".gte"]
    dcons = [Eq, Lt, Lte, Gt, Gte]
    parse []     = pure Nothing
    parse (s:ss) = case H.lookup (key `T.append` fst s) obj of
                       Nothing -> parse ss
                       Just v -> (snd s `fmap`) `fmap` parseJSON v

data EntryCondition = EntryCondition {
        games :: Maybe [T.Text],
        notGames :: Maybe [T.Text],
        keywords :: Maybe [T.Text],
        notKeywords :: Maybe [T.Text],
        points :: Maybe (OrdCond Integer),
        copies :: Maybe (OrdCond Integer),
        wishlist :: Bool
    } deriving (Show, Eq)

instance FromJSON EntryCondition where
    parseJSON (Object v) = EntryCondition <$>
                           v .:? "games" <*>
                           v .:? "not.games" <*>
                           v .:? "keywords" <*>
                           v .:? "not.keywords" <*>
                           v `withOrdCond` "points" <*>
                           v `withOrdCond` "copies" <*>
                           v .:? "wishlist" .!= False
    parseJSON _          = mzero

match :: GiveawayEntry -> SG.SteamGames -> EntryCondition -> Bool
match ge sg EntryCondition{..} = and [
        c (matchGames ge) games,
        c (matchNotGamed ge) notGames,
        c (matchKeywords ge) keywords,
        c (matchNotKeywords ge) notKeywords,
        c (matchOrdCond ge GE.points) points,
        c (matchOrdCond ge GE.copies) copies,
        matchWishlist wishlist
    ]
  where 
    c = maybe True
    matchGames GiveawayEntry{gameTitle=gameTitle} gs = gameTitle `elem` gs
    matchNotGamed g = not . matchGames g
    matchKeywords GiveawayEntry{gameTitle=gameTitle} ks =
        let gameTitleLower = T.toLower gameTitle
        in any (matchKeyword gameTitleLower) ks
    matchKeyword gt k = all (`T.isInfixOf` gt) (T.words k)
    matchNotKeywords g = not . matchKeywords g
    matchWishlist False = True
    matchWishlist True = gameTitle ge `HS.member` (sg^.SG.wishlist)
    matchOrdCond g f oc = case oc of
                             Eq x -> f g == x
                             Lt x -> f g < x
                             Lte x -> f g <= x
                             Gt x -> f g > x
                             Gte x -> f g >= x
