module Day11 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.MemoTrie (memoFix)

run :: String -> (String, String)
run = both show . (run' 25 &&& run' 75) . parse
  where
    run' n = sum . map (memoFix compNum . (n,))

compNum :: ((Int, Int) -> Int) -> (Int, Int) -> Int
compNum _ (0, _) = 1
compNum rec (n, 0) = rec (n - 1, 1)
compNum rec (n, x)
  | even $ length $ show x = uncurry (+) $ both (rec . (n - 1,)) $ splitNum x
  | otherwise = rec (n - 1, x * 2024)

splitNum :: Int -> (Int, Int)
splitNum = both read . (splitAt =<< (`div` 2) . length) . show

parse :: String -> [Int]
parse = map read . words
