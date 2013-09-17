module Gifter.GiveawayEntry.Internal (
    GiveawayEntry(..)
) where

import qualified Data.Text as T

data GiveawayEntry =
        GiveawayEntry {
            url :: T.Text,
            gameTitle :: T.Text,
            copies :: Integer,
            points :: Integer
        } deriving (Show, Eq)
