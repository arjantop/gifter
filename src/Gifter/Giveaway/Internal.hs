module Gifter.Giveaway.Internal where

import qualified Data.ByteString as S

type Url = String

data Giveaway = Giveaway {
                url     :: Url
              , status  :: GiveawayStatus
              , entries :: Integer
              , formKey :: Maybe S.ByteString
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
        deriving (Show, Eq)
