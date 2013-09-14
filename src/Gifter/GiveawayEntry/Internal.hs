module Gifter.GiveawayEntry.Internal (
    GiveawayEntry(..)
) where


data GiveawayEntry =
        GiveawayEntry {
            url :: String,
            gameTitle :: String,
            copies :: Integer,
            points :: Integer
        } deriving (Show, Eq)
