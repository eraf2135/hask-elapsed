module Main where

import Lib

main :: IO ()
main = do
  fromDate <- getLine
  toDate <- getLine
  print (elapsed fromDate toDate)
