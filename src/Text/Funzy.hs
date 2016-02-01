module Text.Funzy where

import           Data.List (isSubsequenceOf, sort, sortBy)
import           Data.Ord  (comparing)

run :: IO ()
run = return ()

finder :: String -> [String] -> [String]
finder _  [] = []
finder "" xs = sort xs
finder x  xs = sortBy (comparing (density x)) $ filter (isSubsequenceOf x) xs
  where
    density []     _  = 0
    density _      [] = error "failed do calculate match density"
    density (x:xs) y  = let (a,b) = span (/= x) y
                        in 1 + length a + density xs (drop 1 b)

