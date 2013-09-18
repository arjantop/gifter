{-# LANGUAGE TemplateHaskell #-}
module Gifter.GiveawayEntry.Internal where

import Control.Lens

import qualified Data.Text as T

data GiveawayEntry =
        GiveawayEntry {
            _gUrl :: T.Text,
            _gameTitle :: T.Text,
            _gCopies :: Integer,
            _gPoints :: Integer
        } deriving (Show, Eq)
makeLenses ''GiveawayEntry
