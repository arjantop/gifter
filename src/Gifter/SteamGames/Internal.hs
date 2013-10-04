{-# LANGUAGE TemplateHaskell #-}
module Gifter.SteamGames.Internal
    ( SteamGames(..)
    , sOwned
    , sWishlist
    , sFormKey
    ) where

import Control.Lens

import qualified Data.Text as T
import qualified Data.HashSet as HS

data SteamGames = SteamGames
    { _sOwned :: HS.HashSet T.Text
    , _sWishlist :: HS.HashSet T.Text
    , _sFormKey :: Maybe T.Text
    } deriving (Show, Eq)
makeLenses ''SteamGames
