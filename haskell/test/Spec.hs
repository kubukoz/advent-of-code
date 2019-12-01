import           Test.Hspec
import           Control.Exception              ( evaluate )
import qualified Day0
import qualified Day1

main :: IO ()
main = hspec . sequence_ $ [day0, day1]

day0 = describe "Day0" $ do
  it "succeeds in first part" $ Day0.getLevel `shouldReturnWithFile` 232
  it "succeeds in second part" $ Day0.basementLevel `shouldReturnWithFile` Just
    1783
  where a `shouldReturnWithFile` b = fmap a Day0.file `shouldReturn` b


day1 = describe "Day1" $ do
  it "succeeds in first part" $ Day1.part1 `shouldReturnWithLinesM` 3402634
  it "succeeds in second part" $ Day1.part2 `shouldReturnWithLinesM` 5101069
  where a `shouldReturnWithLinesM` b = (Day1.file >>= a) `shouldReturn` b

