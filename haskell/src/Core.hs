module Core where
import           Data.Foldable

newtype Day = Day { number :: Int }

instance Show Day where
  show = show . number


readWhole :: Day -> IO String
readWhole day = readFile path where path = "./data/day" ++ show day ++ ".txt"

-- Reads a file from the data directory by the convention day<daynumber>.txt
readLines :: Day -> IO [String]
readLines = fmap lines . readWhole

-- Shows all lines of file by day
showLines day = readLines day >>= traverse_ print
