{-# LANGUAGE OverloadedStrings #-}
module Gifter.GiveawayEntry.Parser (
    parseEntries
) where

import Control.Lens
import Control.Applicative ((<$>),(<*>))
import Control.Monad

import Data.Conduit
import Data.XML.Types
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Text.XML.Stream.Parse
import Text.Regex.TDFA

import Safe

import Gifter.GiveawayEntry.Internal

parseEntries :: ConduitM Event o (ResourceT IO) [GiveawayEntry]
parseEntries = force "rss required" parseRss

parseRss :: ConduitM Event o (ResourceT IO) (Maybe [GiveawayEntry])
parseRss = tagName "rss" ignoreAttrs
                (const $ force "channel required" parseChannel)

parseChannel :: ConduitM Event o (ResourceT IO) (Maybe [GiveawayEntry])
parseChannel = tagNoAttr "channel" $ do
    _ <- skipUntilTag "item"
    many (join `liftM` parseItem)

parseItem :: ConduitM Event o (ResourceT IO) (Maybe (Maybe GiveawayEntry))
parseItem = tagName "item" ignoreAttrs $ \_ -> do
    title <- tagNoAttr "title" content
    let (t, c, p) = parseTitle (T.unpack . fromMaybe "" $ title)
    _ <- skipTag "link"
    guid <- tagName "guid" ignoreAttrs (const content)
    _ <- skipTag "pubDate"
    _ <- skipTag "description"
    _ <- skipTag "origLink"
    return $ GiveawayEntry <$> guid <*> t <*> c <*> p

type RegexResult = (String, String, String, [String])

parseTitle :: String -> (Maybe T.Text, Maybe Integer, Maybe Integer)
parseTitle rt = matches & _1 %~ toT
                        & _2 %~ ((`mplus` Just 1) . (>>= readMay))
                        & _3 %~ (>>=readMay)
  where
    matches = if rt =~ patC
                then let ms = view _4 (rt =~ patC :: RegexResult)
                     in (atMay ms 0, atMay ms 1, atMay ms 2)
                else let ms = view _4 (rt =~ patN :: RegexResult)
                     in (atMay ms 0, Nothing, atMay ms 1)
    pat s = "^(.+) " ++ s ++ "\\(([0-9]+)P\\)$" :: String
    patN = pat ""
    patC = pat "\\(([0-9]+) Copies\\) "
    toT = (T.pack `fmap`)

skipUntilTag :: Monad m => T.Text -> ConduitM Event o m [()]
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

skipTag :: MonadThrow m => T.Text -> ConduitM Event o m (Maybe ())
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

