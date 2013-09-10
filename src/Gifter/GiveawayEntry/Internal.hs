module Gifter.GiveawayEntry.Internal (
    GiveawayEntry(..)
)	where


data GiveawayEntry =
        GiveawayEntry {
            url :: String,
            gameTitle :: String,
            points :: Int
        } deriving (Show, Eq)
