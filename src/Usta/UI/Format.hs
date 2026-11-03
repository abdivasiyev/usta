{-# LANGUAGE OverloadedStrings #-}

-- | Times as the pages show them: "5 min ago" for the last week, then the
-- date.
module Usta.UI.Format
  ( Clock (..)
  , clock
  , ago
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( TimeZone
  , UTCTime
  , defaultTimeLocale
  , diffUTCTime
  , formatTime
  , getCurrentTime
  , getCurrentTimeZone
  , localDay
  , toGregorian
  , utcToLocalTime
  )

-- | The moment pages are drawn at, and the zone their dates are in.
type Clock :: Type
data Clock = Clock
  { now :: UTCTime
  , zone :: TimeZone
  }

clock :: IO Clock
clock = Clock <$> getCurrentTime <*> getCurrentTimeZone

-- | How long before the clock a time was. A time after it (a forge whose
-- clock runs ahead) is "just now".
ago :: Clock -> UTCTime -> Text
ago c time
  | minutes < 1 = "just now"
  | minutes < 60 = count minutes "min" "min"
  | hours < 24 = count hours "hour" "hours"
  | days < 7 = count days "day" "days"
  | year time == year c.now = date "%b %-d"
  | otherwise = date "%b %-d, %Y"
  where
    seconds = floor (diffUTCTime c.now time) :: Int
    minutes = seconds `div` 60
    hours = minutes `div` 60
    days = hours `div` 24
    count n one many = T.pack (show n) <> " " <> (if n == 1 then one else many) <> " ago"
    year t = let (y, _, _) = toGregorian (local t) in y
    local t = localDay (utcToLocalTime c.zone t)
    date format = T.pack (formatTime defaultTimeLocale format (utcToLocalTime c.zone time))
