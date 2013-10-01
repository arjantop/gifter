module Gifter.TaggedValue
    ( TaggedValue
    , Tagged
    , tagValue
    , getTag
    , getValue
    , isExpired
    ) where

import Data.Time.Clock

data TaggedValue t a = TaggedValue t a

tagValue :: t -> a -> TaggedValue t a
tagValue = TaggedValue

getTag :: TaggedValue t a -> t
getTag (TaggedValue t _) = t

getValue :: TaggedValue t a -> a
getValue (TaggedValue _ a) = a

class Tagged t where
    isExpired :: t -> TaggedValue t a -> Bool

instance Tagged UTCTime where
    isExpired t tv = diffUTCTime (getTag tv) t < 0
