module Day7Fast (run) where

import Combinators (both, (.:))
import Control.Arrow ((&&&))

run :: String -> (String, String)
run = both show . (run' 1 &&& run' 2) . parse
  where
    run' = sum . map fst .: filter . isValid

isValid :: Int -> (Int, [Int]) -> Bool
isValid part (res, x : xs) = mul || add || cat
  where
    mul = (res `mod` x == 0) && isValid part (res `div` x, xs)
    add = (x < res) && isValid part (res - x, xs)
    cat = (part == 2) && res `endsWith` x && isValid part (x `removeEnd` res, xs)
    endsWith a b = and $ zipWith (==) (reverse $ show a) (reverse $ show b)
    removeEnd end = read . ('0' :) . reverse . drop (length (show end)) . reverse . show
isValid _ (1, []) = True
isValid _ _ = False

parse :: String -> [(Int, [Int])]
parse = map (parseRes &&& parseNums) . lines
  where
    parseRes = read . takeWhile (/= ':')
    parseNums = reverse . map read . words . drop 2 . dropWhile (/= ':')
