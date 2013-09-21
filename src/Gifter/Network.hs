{-# LANGUAGE OverloadedStrings #-}
module Gifter.Network (
    request
) where

import Network (withSocketsDo)
import qualified Network.HTTP.Conduit as CH
import Network.HTTP.Conduit (Request(..),Cookie(..))

import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as SL
import qualified Data.ByteString.Char8 as SC

request :: T.Text
        -> SC.ByteString
        -> [(SC.ByteString, SC.ByteString)]
        -> T.Text
        -> (SL.ByteString -> a)
        -> IO a
request gurl m qs sessionId f = withSocketsDo $ do
    req' <- CH.parseUrl (T.unpack gurl)
    let jar = Just $ CH.createCookieJar [sessionCookie (T.unpack sessionId)]
        reqWithCookies = req' {
                cookieJar = jar,
                method = m,
                responseTimeout = Just 10000000
            }
        req = if m == "POST"
                  then CH.urlEncodedBody qs reqWithCookies
                  else reqWithCookies
    CH.withManager $ \manager -> do
        response <- CH.httpLbs req manager
        return $ f (CH.responseBody response)

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
