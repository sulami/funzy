import           Test.Hspec
import           Test.QuickCheck

import           Text.Funzy

main :: IO ()
main = hspec $ do

  describe "the sanity" $
    it "exists" $
      property $
        \x -> x `shouldBe` (x :: Int)

  describe "the main process" $
    it "exits gracefully" $
      run `shouldReturn` ()
