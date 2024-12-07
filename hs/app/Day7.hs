module Day7 (run) where

import Combinators (both, (.:))
import Control.Arrow ((&&&))
import Data.Function (on)

run :: String -> (String, String)
run = both show . (run' part1 &&& run' part2) . parse
  where
    run' = sum . map fst .: filter . isValid
    part1 = [(*), (+)]
    part2 = (read .: (++) `on` show) : part1

isValid :: [Int -> Int -> Int] -> (Int, [Int]) -> Bool
isValid _ (res, x : _) | x > res = False
isValid ops (res, x : y : ns) = any testOp ops
  where
    testOp op = isValid ops (res, x `op` y : ns)
isValid _ (res, [n]) = res == n
isValid _ (_, []) = False

parse :: String -> [(Int, [Int])]
parse = map (parseRes &&& parseNums) . lines
  where
    parseRes = read . takeWhile (/= ':')
    parseNums = map read . words . drop 2 . dropWhile (/= ':')
