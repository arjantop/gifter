module Gifter.Giveaway.Internal where

import qualified Data.Text as T

data Giveaway = Giveaway {
                url     :: T.Text
              , status  :: GiveawayStatus
              , entries :: Integer
              , formKey :: Maybe T.Text
              } deriving (Show, Eq)

type Points = Int

data GiveawayStatus =
          Open Points
        | Closed
        | ContributorsOnly
        | AlreadyOwn
        | NoLogin
        | Entered
        | MissingBaseGame
        | ComingSoon
        | NotEnoughPoints
        deriving (Show, Eq)
