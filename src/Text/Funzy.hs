module Text.Funzy where

import           Data.List (isSubsequenceOf, sort, sortBy)
import           Data.Ord  (comparing)

run :: IO ()
run = return ()

finder :: String -> [String] -> [String]
finder _  [] = []
finder "" xs = sort xs
finder x  xs = filter (isSubsequenceOf x) xs


