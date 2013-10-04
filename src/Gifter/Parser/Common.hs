{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Gifter.Parser.Common
    ( parseFormKey
    ) where

import Text.XML.Cursor
import Text.XML.Selector.TH
import Text.XML.Selector.Types (JQSelector)

import qualified Data.Text as T

import Safe

parseFormKey :: Cursor -> Maybe T.Text
parseFormKey c = parseAttribute [jq| input[name=form_key] |] c

parseAttribute :: [JQSelector] -> Cursor -> Maybe T.Text
parseAttribute query c =
    let attrList = fmap (attribute "value") . headMay . queryT query $ c
    in attrList >>= headMay
