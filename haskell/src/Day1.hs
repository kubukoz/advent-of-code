module Day1 where

import           Core

file = readLines (Day 1)

fuel n = (n `div` 3) - 2

fuelRec :: Int -> Int
fuelRec n = current + remaining where
  current = fuel n `max` 0
  remaining | current == 0 = 0
            | otherwise    = fuelRec current

forEachModule :: (Int -> Int) -> [String] -> IO Int
forEachModule getFuel = fmap (sum . fmap getFuel) . traverse readIO

part1 = forEachModule fuel
part2 = forEachModule fuelRec

main = do
  content <- file
  part1 content >>= print
  part2 content >>= print
