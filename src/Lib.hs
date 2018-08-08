module Lib
    ( elapsed
    ) where

import Data.List
import Data.List.Split

elapsed :: [Char] -> [Char] -> Int
elapsed fromDateStr toDateStr =
  let [fromYr, fromMth, fromDay, fromHr, fromMin, fromSec] = dateTimeParts fromDateStr
      [toYr, toMth, toDay, toHr, toMin, toSec] = dateTimeParts toDateStr
  in toSec - fromSec
     + minsToSeconds (toMin - fromMin)
     + hoursToSeconds (toHr - fromHr)
     + daysToSeconds (toDay - fromDay)
     + monthsToSeconds fromMth toMth
     + yearsToSeconds fromYr fromMth toYr toMth

dateTimeParts :: [Char] -> [Int]
dateTimeParts dateStr =
  let [date, time] = splitOn "T" dateStr
      [yr, mth, day] = splitOn "-" date
      [hr, mins, sec] = splitOn ":" time
  in [read yr :: Int,
      read mth :: Int,
      read day :: Int,
      read hr :: Int,
      read mins :: Int,
      read sec :: Int]

minsToSeconds :: Int -> Int
minsToSeconds mins = mins * 60

hoursToSeconds :: Int -> Int
hoursToSeconds hrs = hrs * 60 * 60

daysToSeconds :: Int -> Int
daysToSeconds days = days * 60 * 60 * 24

daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

monthsToSeconds :: Int -> Int -> Int
monthsToSeconds from to =
  let days = foldl (\acc month -> acc + (daysInMonth !! (pred month))) 0 [from..(pred to)]
  in daysToSeconds days

yearsToSeconds :: Int -> Int -> Int -> Int -> Int
yearsToSeconds fromYear fromMonth toYear toMonth =
  let daysExclLeaps = (toYear - fromYear) * 365
      days = daysExclLeaps + (extraLeapYrDays fromYear fromMonth toYear toMonth)
  in daysToSeconds days

extraLeapYrDays :: Int -> Int -> Int -> Int -> Int
extraLeapYrDays fromYear fromMonth toYear toMonth =
  let numLeapYrsExclBoundaries = sum [1 | x <- [(succ fromYear)..(pred toYear)], isLeapYear x == True]
      lowerBoundaryLeapYearAdjustment = if isLeapYear fromYear && fromMonth <= 2 && (toMonth > 2 || toYear > fromYear)
                                          then 1
                                          else 0
      upperBoundaryLeapYearAdjustment = if isLeapYear toYear && toMonth > 2 && (fromMonth <= 2 || fromYear < toYear)
                                          then 1
                                          else 0
   in numLeapYrsExclBoundaries + lowerBoundaryLeapYearAdjustment + upperBoundaryLeapYearAdjustment


isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0