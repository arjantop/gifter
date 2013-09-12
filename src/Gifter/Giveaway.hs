{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Gifter.Giveaway (
    Giveaway(..),
    GiveawayStatus(..),
    GiveawayError(..),
    canEnter,
    getGiveaway,
    enterGiveaway
) where


import qualified Data.ByteString.Char8 as SC

import Text.XML.Cursor (fromDocument)
import Text.HTML.DOM (parseLBS)

import Network.HTTP.Conduit (Request(..),Cookie(..))
import qualified Network.HTTP.Conduit as CH

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Calendar

import Control.Monad (join)
import Control.Exception (try)
import Control.Arrow

import Gifter.Config
import Gifter.Giveaway.Internal
import Gifter.Giveaway.Parser (parse)

data GiveawayError = HttpError CH.HttpException
                   | ParseError

canEnter :: Giveaway -> Bool
canEnter Giveaway {status=Open _} = True
canEnter _ = False

getGiveaway :: Url -> Config -> IO (Either GiveawayError Giveaway)
getGiveaway gurl Config{..} = request gurl "GET" [] sessionId

enterGiveaway :: Giveaway -> Config -> IO (Either GiveawayError Bool)
enterGiveaway Giveaway{formKey=formKey,url=url} Config{..} = do
    let key = fromMaybe "" formKey
        params = [("enter_giveaway", "1"), ("form_key", key)]
    r <- request url "POST" params sessionId
    return . right ((==Entered) . status) $ r

request :: String
        -> SC.ByteString
        -> [(SC.ByteString, SC.ByteString)]
        -> String
        -> IO (Either GiveawayError Giveaway)
request gurl m qs sessionId = do
    req' <- CH.parseUrl gurl
    let reqWithCookies = req' {
                cookieJar = Just $ CH.createCookieJar [sessionCookie sessionId],
                method = m
            }
        req = if m == "POST"
                  then CH.urlEncodedBody qs reqWithCookies
                  else reqWithCookies
    response <- try $ CH.withManager $ \manager -> do
        response <- CH.httpLbs req manager
        let root = fromDocument . parseLBS $ CH.responseBody response
            giveaway = parse gurl root
        return giveaway
    return . join $ left HttpError . right (toEither ParseError) $ response
  where
    toEither e Nothing = Left e
    toEither _ (Just x) = Right x

sessionCookie :: String -> Cookie
sessionCookie sid =
        Cookie {
            cookie_name = "PHPSESSID",
            cookie_value = SC.pack sid,
            cookie_expiry_time = expire,
            cookie_domain = "steamgifts.com",
            cookie_path = "/",
            cookie_creation_time = creation,
            cookie_last_access_time = creation,
            cookie_persistent = False,
            cookie_host_only = False,
            cookie_secure_only = False,
            cookie_http_only = False
        }
  where
    expire = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
    creation = UTCTime (fromGregorian 2012 1 1) (secondsToDiffTime 0)
