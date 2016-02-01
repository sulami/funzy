module Text.Funzy where

import           Data.List (isSubsequenceOf, sort, sortBy)
import           Data.Ord  (comparing)

run :: IO ()
run = return ()

runner :: String -> [String] -> [String]
runner _  [] = []
runner "" xs = sort xs
runner x  xs = filter (isSubsequenceOf x) xs


