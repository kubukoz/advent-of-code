import           Test.Hspec
import           Control.Exception              ( evaluate )
import qualified Day0

main :: IO ()
main = hspec day0

a `shouldReturnWithFile` b = fmap a Day0.file `shouldReturn` b

day0 = describe "Day0" $ do
  it "succeeds in first part" $ Day0.getLevel `shouldReturnWithFile` 232
  it "succeeds in second part" $ Day0.basementLevel `shouldReturnWithFile` Just
    1783
