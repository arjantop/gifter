{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Gifter.Giveaway (
    Giveaway(..),
    GiveawayStatus(..),
    GiveawayError(..),
    canEnter,
    isRemoved,
    getGiveaway,
    enterGiveaway
) where


import Text.XML.Cursor (fromDocument)
import Text.HTML.DOM (parseLBS)

import qualified Network.HTTP.Conduit as CH

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as SL
import qualified Data.Text as T

import Control.Lens
import Control.Monad
import Control.Exception (try)
import Control.Arrow

import Gifter.Network
import Gifter.Config
import Gifter.Giveaway.Internal
import Gifter.Giveaway.Parser

data GiveawayError = HttpError CH.HttpException
                   | ResponseParseError DataError

canEnter :: Giveaway -> Bool
canEnter Giveaway {status = Open _} = True
canEnter _                          = False

isRemoved :: GiveawayError -> Bool
isRemoved (ResponseParseError GiveawayRemoved) = True
isRemoved _                                   = False

getGiveaway :: T.Text -> Config -> IO (Either GiveawayError Giveaway)
getGiveaway gurl cfg =
    handleResponse $ request gurl "GET" [] (cfg^.sessionId) (parseResponse gurl)

enterGiveaway :: Giveaway -> Config -> IO (Either GiveawayError Bool)
enterGiveaway Giveaway{formKey=formKey,url=url} cfg = do
    let key = fromMaybe "" formKey
        params = [("enter_giveaway", "1"), ("form_key", SC.pack . T.unpack $ key)]
    r <- handleResponse $ request url "POST" params (cfg^.sessionId) (parseResponse url)
    return . right ((==Entered) . status) $ r

parseResponse :: T.Text -> SL.ByteString -> Either DataError Giveaway
parseResponse gurl = parse gurl . fromDocument . parseLBS

handleResponse :: IO (Either DataError a) -> IO (Either GiveawayError a)
handleResponse r =
    liftM (join . handleLeft . handleRight) (try r)
  where
    handleLeft = left HttpError
    handleRight = right (toEither ResponseParseError)
    toEither ec (Left e) = Left $ ec e
    toEither _ (Right x) = Right x
