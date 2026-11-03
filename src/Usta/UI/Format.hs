{-# LANGUAGE OverloadedStrings #-}

-- | Times as the pages show them: "5 min ago" in sentences, "5m" in list
-- columns; the date once they are a week old.
module Usta.UI.Format
  ( Clock (..)
  , clock
  , ago
  , short
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( Day
  , TimeZone
  , UTCTime
  , addDays
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
  | otherwise = date c time
  where
    (minutes, hours, days) = elapsed c time
    count n one many = T.pack (show n) <> " " <> (if n == 1 then one else many) <> " ago"

-- | 'ago' for a list column: "5m", "3h", "Yesterday", "Mon", then the date.
short :: Clock -> UTCTime -> Text
short c time
  | minutes < 1 = "now"
  | minutes < 60 = T.pack (show minutes) <> "m"
  | hours < 24 = T.pack (show hours) <> "h"
  | day c time == addDays (-1) (day c c.now) = "Yesterday"
  | days < 7 = local c "%a" time
  | otherwise = date c time
  where
    (minutes, hours, days) = elapsed c time

-- | Whole minutes, hours and days from a time to the clock.
elapsed :: Clock -> UTCTime -> (Int, Int, Int)
elapsed c time = (minutes, minutes `div` 60, minutes `div` (60 * 24))
  where
    minutes = floor (diffUTCTime c.now time) `div` 60

-- | "Oct 29", with the year when it is not the clock's.
date :: Clock -> UTCTime -> Text
date c time
  | year time == year c.now = local c "%b %-d" time
  | otherwise = local c "%b %-d, %Y" time
  where
    year t = let (y, _, _) = toGregorian (day c t) in y

day :: Clock -> UTCTime -> Day
day c t = localDay (utcToLocalTime c.zone t)

local :: Clock -> String -> UTCTime -> Text
local c format t = T.pack (formatTime defaultTimeLocale format (utcToLocalTime c.zone t))
