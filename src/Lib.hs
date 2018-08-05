module Lib
    ( elapsed
    ) where

import Data.List
import Data.List.Split

elapsed :: String -> String -> Int
elapsed fromDateStr toDateStr =
  let fromDateTimeParts = dateTimeParts fromDateStr
      toDateTimeParts = dateTimeParts toDateStr
  in (toDateTimeParts !! 5) - (fromDateTimeParts !! 5)
     + minsToSeconds (toDateTimeParts !! 4 - fromDateTimeParts !! 4)
     + hoursToSeconds (toDateTimeParts !! 3 - fromDateTimeParts !! 3)
     + daysToSeconds (toDateTimeParts !! 2 - fromDateTimeParts !! 2)
     + monthsToSeconds (fromDateTimeParts !! 1) (toDateTimeParts !! 1)
     + yearsToSeconds (fromDateTimeParts !! 0) (fromDateTimeParts !! 1) (toDateTimeParts !! 0) (toDateTimeParts !! 1)

dateTimeParts :: String -> [Int]
dateTimeParts dateStr =
  let dateTime = splitOn "T" dateStr
      date = splitOn "-" (head dateTime)
      time = splitOn ":" (last dateTime)
  in [read (date !! 0) :: Int,
      read (date !! 1) :: Int,
      read (date !! 2) :: Int,
      read (time !! 0) :: Int,
      read (time !! 1) :: Int,
      read (time !! 2) :: Int]

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