module Day1 (run) where

import Combinators
import Control.Arrow ((&&&))
import Data.List (sort, transpose)
import Utils

run :: String -> IO (String, String)
run = return . both show . (part1 &&& part2) . parse

part1, part2 :: ([Int], [Int]) -> Int
part1 = sum . uncurry (zipWith (abs .: (-))) . both sort
part2 = sum . map fst . filter (uncurry (==)) . uncurry combinations

parse :: String -> ([Int], [Int])
parse = (head &&& last) . transpose . chunk 2 . map read . words
