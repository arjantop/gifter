{-# LANGUAGE OverloadedStrings #-}
module Gifter.GiveawayEntry.Parser (
    parseEntries
) where

import Control.Monad.Trans.Resource
import Control.Applicative ((<$>),(<*>))
import Control.Monad

import Data.Conduit
import Data.Text (Text, unpack)
import Data.XML.Types
import Data.Maybe (fromMaybe)

import Text.XML.Stream.Parse
import Text.Regex.Posix

import Safe

import Gifter.GiveawayEntry.Internal

parseEntries :: ConduitM Event o (ResourceT IO) [GiveawayEntry]
parseEntries = force "rss required" parseRss

parseRss :: ConduitM Event o (ResourceT IO) (Maybe [GiveawayEntry])
parseRss = tagName "rss" ignoreAttrs
                (const $ force "channel required" parseChannel)

parseChannel :: ConduitM Event o (ResourceT IO) (Maybe [GiveawayEntry])
parseChannel = tagNoAttr "channel" $ do
    skipUntilTag "item"
    many (join `liftM` parseItem)

parseItem :: ConduitM Event o (ResourceT IO) (Maybe (Maybe GiveawayEntry))
parseItem = tagName "item" ignoreAttrs $ \_ -> do
    title <- tagNoAttr "title" content
    let (t, c, p) = parseTitle (unpack . fromMaybe "" $ title)
    skipTag "link"
    guid <- tagName "guid" ignoreAttrs (const content)
    skipTag "pubDate"
    skipTag "description"
    skipTag "origLink"
    return $ GiveawayEntry <$> (unpack `fmap` guid) <*> t <*> c <*> p
  where
    parseTitle rt
        = let (_, _, _, matches) = rt =~ pat :: (String, String, String, [String])
              c = atMay matches 2 >>= readMay
          in (atMay matches 0,
              c `mplus` Just 1,
              atMay matches 3 >>= readMay)
    pat = "^([^(]+?) (\\(([0-9]+) Copies\\) )?\\(([0-9]+)P\\)$" :: String

skipUntilTag :: Monad m => Text -> ConduitM Event o m [()]
skipUntilTag name = many skipAllTagsUntil
  where
    skipAllTagsUntil = do
        me <- await
        case me of
            Just e@(EventBeginElement n _) | nameLocalName n == name -> do
                leftover e
                return Nothing
            Nothing -> return Nothing
            _ -> return $ Just ()

skipTag :: MonadThrow m => Text -> ConduitM Event o m (Maybe ())
skipTag name = tagPredicate ((== name) . nameLocalName) ignoreAttrs
    (const . void $ many manySkipTagsUltilClose)
  where
    manySkipTagsUltilClose = do
        me <- await
        case me of
            Just e@(EventEndElement n) | nameLocalName n == name -> do
                leftover e
                return Nothing
            Nothing -> return Nothing
            _ -> return $ Just ()

