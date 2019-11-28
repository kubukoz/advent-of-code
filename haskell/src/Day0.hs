module Day0 where

import           Core
import           Data.List
import           Data.Foldable

main = do
  content <- readWhole (Day 0)
  print . getLevel $ content
  print . basementLevel $ content


getLevel :: String -> Int
getLevel = last . sums

basementLevel :: String -> Maybe Int
basementLevel = findIndex (0 >) . sums

sums = scanl (+) 0 . map parseToDiff

parseToDiff '(' = 1
parseToDiff ')' = -1
parseToDiff _   = 0
