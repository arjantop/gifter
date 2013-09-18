module Gifter.GiveawayEntry (
    GiveawayEntry(..),
    gUrl,
    gameTitle,
    gCopies,
    gPoints,
    getEntries
) where

import Network (withSocketsDo)
import qualified Network.HTTP.Conduit as CH

import Control.Monad.Trans.Resource (runResourceT)
import Control.Exception

import Data.Conduit (($$))

import qualified Text.XML.Stream.Parse as XML

import Gifter.GiveawayEntry.Internal
import Gifter.GiveawayEntry.Parser

getEntries :: Int -> IO (Either SomeException [GiveawayEntry])
getEntries n = withSocketsDo . try $
    CH.simpleHttp "http://feeds.feedburner.com/steamgifts" >>= \s -> do
        gs <- runResourceT $ XML.parseLBS XML.def s $$ parseEntries
        return . take n $ gs
