import           Data.List       (isSubsequenceOf, sort, sortBy, subsequences)
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

  describe "the finder" $ do
    let nonNull = arbitrary `suchThat` (not . null)
        options = listOf nonNull

    it "returns an empty list without any input data" $
      property $
        \x -> finder x [] `shouldBe` []

    it "returns the sorted input if the search term is empty" $
      property $
        \x -> finder "" x `shouldBe` sort x

    it "only returns proper superstrings of the search term" $
      forAll nonNull $ \x -> property $
        \y -> let rv = finder x y
              in rv `shouldBe` filter (isSubsequenceOf x) rv

    it "returns the results with the most dense match first" $ do
      let density []     _  = 0
          density _      [] = error "failed do calculate match density"
          density (x:xs) y  = let (a,b) = span (/= x) y
                              in 1 + length a + density xs (drop 1 b)
      forAll options $ \y -> property $
        \x -> let rv = finder x y
              in rv `shouldBe` sortBy (comparing (density x)) rv

