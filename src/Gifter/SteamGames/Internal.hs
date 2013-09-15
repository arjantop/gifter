{-# LANGUAGE TemplateHaskell #-}
module Gifter.SteamGames.Internal (
    SteamGames(..),
    owned,
    wishlist
) where

import Control.Lens

import qualified Data.Text as T
import qualified Data.HashSet as HS

data SteamGames = SteamGames {
                    _owned :: HS.HashSet T.Text,
                    _wishlist :: HS.HashSet T.Text
                } deriving (Show, Eq)
makeLenses ''SteamGames
