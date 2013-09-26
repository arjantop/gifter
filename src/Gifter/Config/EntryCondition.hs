{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Gifter.Config.EntryCondition
    ( EntryCondition(..)
    , games
    , notGames
    , keywords
    , notKeywords
    , copies
    , points
    , accP
    , wishlist
    , andCond
    , orCond
    , emptyEntryCondition
    , OrdCond(..)
    , match
    , matchAny
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import Data.Maybe

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

data EntryCondition = EntryCondition
    { _games :: Maybe [T.Text]
    , _notGames :: Maybe [T.Text]
    , _keywords :: Maybe [T.Text]
    , _notKeywords :: Maybe [T.Text]
    , _points :: Maybe (OrdCond Integer)
    , _copies :: Maybe (OrdCond Integer)
    , _accP :: Maybe (OrdCond Integer)
    , _wishlist :: Bool
    , _andCond :: Maybe EntryCondition
    , _orCond :: Maybe EntryCondition
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
        _accP = Nothing,
        _wishlist = False,
        _andCond = Nothing,
        _orCond = Nothing
    }

instance FromJSON EntryCondition where
    parseJSON (Object v) = EntryCondition <$>
                           v .:? "games" <*>
                           v .:? "not.games" <*>
                           v .:? "keywords" <*>
                           v .:? "not.keywords" <*>
                           v `withOrdCond` "points" <*>
                           v `withOrdCond` "copies" <*>
                           v `withOrdCond` "accpoints" <*>
                           v .:? "wishlist" .!= False <*>
                           v .:? "and" <*>
                           v .:? "or"
    parseJSON _          = mzero

match :: GiveawayEntry
         -> SteamGames
         -> Maybe Integer
         -> EntryCondition
         -> Bool
match = matchCond and

matchAny :: GiveawayEntry
         -> SteamGames
         -> Maybe Integer
         -> [EntryCondition]
         -> Bool
matchAny ge sg acp = any (match ge sg acp)

matchCond :: ([Bool] -> Bool)
          -> GiveawayEntry
          -> SteamGames
          -> Maybe Integer
          -> EntryCondition
          -> Bool
matchCond bo ge sg acp ec = bo [
        c matchGames (ec^.games),
        c matchNotGamed (ec^.notGames),
        c matchKeywords (ec^.keywords),
        c matchNotKeywords (ec^.notKeywords),
        c (matchOrdCond $ ge^.gCopies) (ec^.copies),
        c (matchOrdCond $ ge^.gPoints) (ec^.points),
        c matchAccountPoints (ec^.accP),
        matchWishlist (ec^.wishlist),
        c (matchCond or ge sg acp) (ec^.orCond),
        c (matchCond and ge sg acp) (ec^.andCond)
    ]
  where 
    defVal = not $ bo [True, False]
    c = maybe defVal
    matchGames gs = (ge^.gameTitle) `elem` gs
    matchNotGamed = not . matchGames
    matchKeywords ks =
        let gameTitleLower = T.toLower $ ge^.gameTitle
        in any (matchKeyword gameTitleLower) ks
    matchKeyword gt k = all (`T.isInfixOf` gt) (T.words k)
    matchNotKeywords = not . matchKeywords
    matchWishlist False = defVal
    matchWishlist True = (ge^.gameTitle) `HS.member` (sg^.sWishlist)
    matchAccountPoints oc = fromMaybe False $ (flip matchOrdCond oc) `fmap` acp
    matchOrdCond v oc =
        case oc of
            Eq x -> v == x
            Lt x -> v < x
            Lte x -> v <= x
            Gt x -> v > x
            Gte x -> v >= x
