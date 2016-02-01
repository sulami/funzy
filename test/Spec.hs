import           Data.List       (isSubsequenceOf, sort, sortBy)
import           Data.Ord        (comparing)

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

  describe "the runner" $ do
    let nonNull = arbitrary `suchThat` (not . null)

    it "returns an empty list without any input data" $
      property $
        \x -> runner x [] `shouldBe` []

    it "returns the sorted input if the search term is empty" $
      property $
        \x -> runner "" x `shouldBe` sort x

    it "only returns proper superstrings of the search term" $
      forAll nonNull $ \x -> property $
        \y -> runner x y `shouldBe` filter (isSubsequenceOf x) y


