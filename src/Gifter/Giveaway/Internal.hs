{-# LANGUAGE TemplateHaskell #-}
module Gifter.Giveaway.Internal where

import Control.Lens

import qualified Data.Text as T

type Points = Integer

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

data Giveaway = Giveaway {
                _url     :: T.Text
              , _status  :: GiveawayStatus
              , _entries :: Integer
              , _formKey :: Maybe T.Text
              , _accPoints :: Integer
              } deriving (Show, Eq)
makeLenses ''Giveaway
