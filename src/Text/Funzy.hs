module Text.Funzy where

import           Data.List (sort)

run :: IO ()
run = return ()

runner :: String -> [String] -> [String]
runner _ [] = []
runner _ xs= sort xs

