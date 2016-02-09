import           Data.List       (isSubsequenceOf, sort, sortBy, subsequences)
import           Data.Ord        (comparing)
import           System.IO       (BufferMode (..), IOMode (..), hClose,
                                  hGetContents, hPutStr, hSetBuffering,
                                  openTempFile)

import           Test.Hspec
import qualified Test.Proctest   as PT
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
        matchInput y = (elements y >>= sublistOf) `suchThat` (not . null)

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

    it "returns the results with the most dense match first" $
      forAll options $ \y -> forAll (matchInput y) $
        \x -> let rv = finder x $ map (dropWhile (/= head x)) y
              in rv `shouldBe` sortBy (comparing (density x)) rv

    it "returns the results with the closest match first" $ do
      let paddings = listOf . listOf $ elements ['a'..'y']
          proximity (x:xs) = length . takeWhile (/=x)
      forAll paddings $
        \p -> let ma = "zzz"
                  rv = finder ma $ map (\x -> x ++ ma ++ x) p
              in rv `shouldBe` sortBy (comparing (proximity ma)) rv

    it "returns the results with equal scores sorted by length" $ do
      let paddings = listOf . listOf $ elements ['a'..'y']
      forAll paddings $
        \p -> let ma = "zzz"
                  rv = finder ma $ map (\x -> x ++ ma ++ x) p
              in rv `shouldBe` sortBy (comparing length) rv

  describe "the interface" $
    it "displays the sorted first five items of input data on startup" $ do
      (ttyPath, tty) <- openTempFile "/tmp" "funzy-faux"
      (hIn, _, _, _) <- PT.run "stack" ["exec", "funzy", "--", "--tty", ttyPath]
      hPutStr hIn $ unlines [ [c] | c <- reverse ['a'..'z'] ]
      PT.sleep $ PT.seconds 0.5
      hSetBuffering tty NoBuffering
      screen <- lines <$> hGetContents tty
      screen `shouldBe` "" : [ [c] | c <- ['a'..'e'] ]
      hClose tty

density :: String -> String -> Int
density []      _ = 0
density x@(h:_) y = density' x $ dropWhile (/= h) y
  where
    density' []     _  = 0
    density' _      [] = error "failed do calculate match density, exhausted"
    density' (x:xs) y  = let (a,b) = span (/= x) y
                          in length a + density' xs (drop 1 b)
