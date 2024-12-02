module Day2 (run) where

import Combinators (both, (.:))
import Control.Applicative (liftA2)
import Control.Arrow (second, (&&&))
import Data.List (nub)
import Utils (between, count)

run :: String -> IO (String, String)
run = return . both show . (fst &&& uncurry (+)) . (part1 &&& part2) . parse

part1 :: [[Int]] -> Int
part1 = count isSafe

part2 :: [[Int]] -> Int
part2 = count (any isSafe . variations) . filter (not . isSafe)
  where
    variations = zipWith dropAt [0 ..] . (replicate =<< length)
    dropAt = uncurry (++) . second tail .: splitAt

isSafe :: [Int] -> Bool
isSafe = liftA2 (&&) sameSign max3 . differences
  where
    sameSign = (1 ==) . length . nub . map signum
    max3 = all (between 1 3 . abs)
    differences = zipWith (-) <*> tail

parse :: String -> [[Int]]
parse = map (map read . words) . lines
