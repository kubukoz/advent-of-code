module Core where
import           Data.Foldable

newtype Day = Day { number :: Int }

instance Show Day where
  show = show . number


-- Reads a file from the data directory by the convention day<daynumber>.txt
readLines :: Day -> IO [String]
readLines day = lines <$> readFile path
  where path = "./data/day" ++ show day ++ ".txt"

showLines day = readLines day >>= traverse_ print
