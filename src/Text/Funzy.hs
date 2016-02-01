module Text.Funzy where

import           Data.Function (on)
import           Data.List     (groupBy, isSubsequenceOf, sort, sortBy)
import           Data.Ord      (comparing)

run :: IO ()
run = return ()

finder :: String -> [String] -> [String]
finder _  [] = []
finder "" xs = sort xs
finder x  xs = let valid = filter (isSubsequenceOf x) xs
                   densGroups = groupBy ((==) `on` density x) valid
                in concatMap (sortBy (comparing (proximity x))) densGroups
  where
    -- Distance of the match from the start
    proximity :: String -> String -> Int
    proximity []     = error "failed do calculate match proximity, exhausted"
    proximity (x:xs) = length . takeWhile (/= x)

    -- Drop the non-matching prefix
    density :: String -> String -> Int
    density []      _ = 0
    density x@(h:_) y = density' x $ dropWhile (/= h) y

    -- Distance between all matching chars
    density' []     _  = 0
    density' _      [] = error "failed do calculate match density, exhausted"
    density' (x:xs) y  = let (a,b) = span (/= x) y
                          in length a + density' xs (drop 1 b)

