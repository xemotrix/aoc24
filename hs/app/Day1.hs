module Day1 (run) where

import Combinators
import Data.List (sort, transpose)
import Utils

run :: String -> IO (String, String)
run = return . both show . fork part1 part2 . parse

part1 :: ([Int], [Int]) -> Int
part1 = sum . uncurry (zipWith (abs .: (-))) . both sort

part2 :: ([Int], [Int]) -> Int
part2 = sum . map fst . filter (uncurry (==)) . uncurry combinations

parse :: String -> ([Int], [Int])
parse = fork head last . transpose . chunk 2 . map read . words
