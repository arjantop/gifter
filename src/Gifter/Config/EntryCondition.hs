{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Gifter.Config.EntryCondition (
    EntryCondition(..),
    games,
    notGames,
    keywords,
    notKeywords,
    copies,
    points,
    wishlist,
    emptyEntryCondition,
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

import Gifter.GiveawayEntry
import Gifter.SteamGames

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
        _games :: Maybe [T.Text],
        _notGames :: Maybe [T.Text],
        _keywords :: Maybe [T.Text],
        _notKeywords :: Maybe [T.Text],
        _points :: Maybe (OrdCond Integer),
        _copies :: Maybe (OrdCond Integer),
        _wishlist :: Bool
    } deriving (Show, Eq)
makeLenses ''EntryCondition

emptyEntryCondition :: EntryCondition
emptyEntryCondition = EntryCondition {
        _games = Nothing,
        _notGames = Nothing,
        _keywords = Nothing,
        _notKeywords = Nothing,
        _points = Nothing,
        _copies = Nothing,
        _wishlist = False
    }

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

match :: GiveawayEntry -> SteamGames -> EntryCondition -> Bool
match ge sg ec = and [
        c matchGames (ec^.games),
        c matchNotGamed (ec^.notGames),
        c matchKeywords (ec^.keywords),
        c matchNotKeywords (ec^.notKeywords),
        c (matchOrdCond gCopies) (ec^.copies),
        c (matchOrdCond gPoints) (ec^.points),
        matchWishlist (ec^.wishlist)
    ]
  where 
    c = maybe True
    matchGames gs = (ge^.gameTitle) `elem` gs
    matchNotGamed = not . matchGames
    matchKeywords ks =
        let gameTitleLower = T.toLower $ ge^.gameTitle
        in any (matchKeyword gameTitleLower) ks
    matchKeyword gt k = all (`T.isInfixOf` gt) (T.words k)
    matchNotKeywords = not . matchKeywords
    matchWishlist False = True
    matchWishlist True = (ge^.gameTitle) `HS.member` (sg^.sWishlist)
    matchOrdCond f oc =
        case oc of
            Eq x -> ge^.f == x
            Lt x -> ge^.f < x
            Lte x -> ge^.f <= x
            Gt x -> ge^.f > x
            Gte x -> ge^.f >= x
